{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Syntax (
  -- * Parsing @.cbl@ files
  parseProgram,
  parsedProgramCfgMap,

  -- * Parsing overrides YAML files
  parseOverridesYaml,
  ParsedOverridesYaml (..),
  resolveOverridesYaml,
  ResolvedOverridesYaml (..),
) where

import Control.Exception.Safe (throw)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Macaw.BinaryLoader.ELF qualified as Loader
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Word (Word64)
import Data.Yaml qualified as Yaml
import Grease.Utility (GreaseException (..), tshow)
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Syntax.Atoms qualified as CSyn (atom)
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.ExprParse qualified as CSyn
import Lang.Crucible.Syntax.SExpr qualified as SExpr
import Text.Megaparsec qualified as MP
import Text.Read qualified as Read
import What4.FunctionName qualified as W4

-----
-- Parsing .cbl files
-----

-- | Parse a program in Crucible S-expression syntax
parseProgram ::
  ( C.IsSyntaxExtension ext
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  C.HandleAllocator ->
  FilePath ->
  IO (CSyn.ParsedProgram ext)
parseProgram halloc path = do
  txt <- Text.IO.readFile path
  case MP.parse (SExpr.skipWhitespace *> MP.many (SExpr.sexp CSyn.atom) <* MP.eof) path txt of
    Left err -> throw (GreaseException (Text.pack (MP.errorBundlePretty err)))
    Right v -> do
      parseResult <- CSyn.top globalNonceGenerator halloc [] $ CSyn.prog v
      case parseResult of
        Left (CSyn.SyntaxParseError e) ->
          throw (GreaseException (CSyn.printSyntaxError e))
        Left err -> throw (GreaseException (Text.pack (show err)))
        Right parsedProg -> pure parsedProg

parsedProgramCfgMap ::
  CSyn.ParsedProgram ext ->
  Map Text (C.Reg.AnyCFG ext)
parsedProgramCfgMap prog =
  let cfgName cfg = W4.functionName (C.handleName (C.Reg.cfgHandle cfg))
      cfgWithName c@(C.Reg.AnyCFG cfg) = (cfgName cfg, c)
   in Map.fromList (List.map cfgWithName (CSyn.parsedProgCFGs prog))

-----
-- Parsing overrides YAML files
-----

-- | The parsed contents of an overrides-related @.yaml@ file. These contents
-- have not yet been checked for validity. For the validated version, see
-- 'ResolvedOverridesYaml'.
newtype ParsedOverridesYaml = ParsedOverridesYaml
  {getParsedOverridesYaml :: Map.Map Word64 W4.FunctionName}
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

-- | The contents of an overrides-related @.yaml@ file, which have been
-- validity-checked to ensure that all of the addresses exist in the binary and
-- that all of the override names actually exist. For the pre-validated version,
-- see 'ParsedOverridesYaml'.
newtype ResolvedOverridesYaml w = ResolvedOverridesYaml
  {getResolvedOverridesYaml :: Map.Map (MM.MemSegmentOff w) W4.FunctionName}
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

-- | Parse an overrides-related @.yaml@ file.
parseOverridesYaml ::
  FilePath ->
  IO ParsedOverridesYaml
parseOverridesYaml yamlPath = do
  res <- Yaml.decodeFileEither yamlPath
  case res of
    Left ex -> throw $ GreaseException $ Text.pack $ Yaml.prettyPrintParseException ex
    Right val -> parseFunctionAddressOverrides val

-- | Validate the contents of an overrides-related @.yaml@ file. See the
-- Haddocks for 'ResolvedOverridesYaml' for a description of what validity
-- checks are performed.
resolveOverridesYaml ::
  MM.MemWidth w =>
  MML.LoadOptions ->
  MM.Memory w ->
  -- | The set of override names.
  Set W4.FunctionName ->
  ParsedOverridesYaml ->
  IO (ResolvedOverridesYaml w)
resolveOverridesYaml loadOpts mem fnOvNames (ParsedOverridesYaml parsedFunAddrs) = do
  resolvedFunAddrs <-
    traverse
      ( \(addr, funName) -> do
          let addrWord = MM.memWord $ addr + loadOffset
          addrSegOff <-
            case Loader.resolveAbsoluteAddress mem addrWord of
              Just addrSegOff -> pure addrSegOff
              Nothing ->
                throw $
                  GreaseException $
                    "Could not resolve "
                      <> tshow addrWord
                      <> "to an address in the binary."
          unless (Set.member funName fnOvNames) $
            throw $
              GreaseException $
                "Could not find an override for a function named '"
                  <> W4.functionName funName
                  <> "'."
          pure (addrSegOff, funName)
      )
      (Map.toList parsedFunAddrs)
  pure $ ResolvedOverridesYaml $ Map.fromList resolvedFunAddrs
 where
  loadOffset = fromMaybe 0 $ MML.loadOffset loadOpts

-- | Helper, not exported.
--
-- Parse the contents of an overrides-related @.yaml@ file from a 'Aeson.Value'.
-- This also ensures that the top-level YAML object does not have any unexpected
-- keys.
parseFunctionAddressOverrides ::
  Aeson.Value ->
  IO ParsedOverridesYaml
parseFunctionAddressOverrides val = do
  mbObj <- asNullableObject val
  case mbObj of
    Nothing -> pure mempty
    Just obj -> do
      let objSansFunAddrOvs = KeyMap.delete funAddrOvsKey obj
      unless (KeyMap.null objSansFunAddrOvs) $
        throw $
          GreaseException $
            Text.unlines $
              "Unexpected keys in overrides.yaml file:"
                : map
                  (\key -> "- " <> Key.toText key)
                  (KeyMap.keys objSansFunAddrOvs)
      case KeyMap.lookup funAddrOvsKey obj of
        Nothing -> pure mempty
        Just funAddrOvs -> do
          funAddrOvsObj <- asObject funAddrOvs
          funAddrOvPairs <-
            traverse
              ( \(addrKey, funName) -> do
                  addr <-
                    case Read.readMaybe (Key.toString addrKey) of
                      Just addr -> pure addr
                      Nothing ->
                        throw $
                          GreaseException $
                            "Expected address in overrides YAML file, but encountered "
                              <> Key.toText addrKey
                  funNameText <- asString funName
                  pure (addr, W4.functionNameFromText funNameText)
              )
              (KeyMap.toList funAddrOvsObj)
          pure $ ParsedOverridesYaml $ Map.fromList funAddrOvPairs
 where
  funAddrOvsKey :: Key.Key
  funAddrOvsKey = "function address overrides"

-- | Helper, not exported.
--
-- Assert that a JSON 'Aeson.Value' is a 'Aeson.String'. If this is the case,
-- return the underlying text. Otherwise, throw an exception.
asString :: Aeson.Value -> IO Text
asString (Aeson.String t) = pure t
asString v =
  throw $
    GreaseException $
      "Expected string in overrides YAML file, but encountered "
        <> Text.decodeUtf8 (BS.toStrict (Aeson.encode v))

-- | Helper, not exported.
--
-- Assert that a JSON 'Aeson.Value' is an 'Aeson.Object'. If this is the case,
-- return the underlying object. Otherwise, throw an exception.
asObject :: Aeson.Value -> IO Aeson.Object
asObject (Aeson.Object o) = pure o
asObject v =
  throw $
    GreaseException $
      "Expected object in overrides YAML file, but encountered "
        <> Text.decodeUtf8 (BS.toStrict (Aeson.encode v))

-- | Helper, not exported.
--
-- Assert that a JSON 'Aeson.Value' is an 'Aeson.Object' or 'Aeson.Null'. If it
-- is an 'Aeson.Object', return 'Just' the underlying object. If it is
-- 'Aeson.Null', return 'Nothing'. If neither of these are the case, throw an
-- exception.
asNullableObject :: Aeson.Value -> IO (Maybe Aeson.Object)
asNullableObject (Aeson.Object o) = pure $ Just o
asNullableObject Aeson.Null = pure Nothing
asNullableObject v =
  throw $
    GreaseException $
      "Expected null or object in overrides YAML file, but encountered "
        <> Text.decodeUtf8 (BS.toStrict (Aeson.encode v))
