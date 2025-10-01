{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Syntax (
  -- * Error types
  ParseProgramError (..),
  ParseOverridesYamlError (..),
  ResolveOverridesYamlError (..),

  -- * Parsing @.cbl@ files
  parseProgram,
  parsedProgramCfgMap,

  -- * Parsing overrides YAML files
  parseOverridesYaml,
  ParsedOverridesYaml (..),
  resolveOverridesYaml,
  ResolvedOverridesYaml (..),
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
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
import Data.Void (Void)
import Data.Word (Word64)
import Data.Yaml qualified as Yaml
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Syntax.Atoms qualified as CSyn (atom)
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.ExprParse qualified as CSyn
import Lang.Crucible.Syntax.SExpr qualified as SExpr
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as MP
import Text.Read qualified as Read
import What4.FunctionName qualified as W4

-----
-- Error types
-----

-- | Errors that can occur during program parsing
--
-- See the 'PP.Pretty' instance for details on each constructor.
data ParseProgramError
  = SExpressionParseError (MP.ParseErrorBundle Text.Text Void)
  | SyntaxParseError Text.Text

instance PP.Pretty ParseProgramError where
  pretty =
    \case
      SExpressionParseError err -> PP.pretty (MP.errorBundlePretty err)
      SyntaxParseError err -> PP.pretty err

-- | Errors that can occur during YAML parsing
--
-- See the 'PP.Pretty' instance for details on each constructor.
data ParseOverridesYamlError
  = YamlParseError Yaml.ParseException
  | UnexpectedYamlKeys [Text.Text]
  | InvalidAddress Text.Text
  | ExpectedString Text.Text
  | ExpectedObject Text.Text
  | ExpectedNullableObject Text.Text

instance PP.Pretty ParseOverridesYamlError where
  pretty =
    \case
      YamlParseError err ->
        PP.pretty (Yaml.prettyPrintParseException err)
      UnexpectedYamlKeys keys ->
        PP.vsep $
          "Unexpected keys in overrides.yaml file:"
            : map (\key -> "- " <> PP.pretty key) keys
      InvalidAddress addr ->
        "Expected address in overrides YAML file, but encountered:" PP.<+> PP.pretty addr
      ExpectedString val ->
        "Expected string in overrides YAML file, but encountered:" PP.<+> PP.pretty val
      ExpectedObject val ->
        "Expected object in overrides YAML file, but encountered:" PP.<+> PP.pretty val
      ExpectedNullableObject val ->
        "Expected null or object in overrides YAML file, but encountered:" PP.<+> PP.pretty val

-- | Errors that can occur during YAML resolution
--
-- See the 'PP.Pretty' instance for details on each constructor.
data ResolveOverridesYamlError
  = AddressUnresolvable Word64
  | FunctionNameNotFound Text.Text

instance PP.Pretty ResolveOverridesYamlError where
  pretty =
    \case
      AddressUnresolvable addr ->
        "Could not resolve" PP.<+> PP.pretty addr PP.<+> "to an address in the binary."
      FunctionNameNotFound name ->
        "Could not find an override for a function named:" PP.<+> PP.dquotes (PP.pretty name)

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
  IO (Either ParseProgramError (CSyn.ParsedProgram ext))
parseProgram halloc path = runExceptT $ do
  txt <- liftIO $ Text.IO.readFile path
  case MP.parse (SExpr.skipWhitespace *> MP.many (SExpr.sexp CSyn.atom) <* MP.eof) path txt of
    Left err -> throwE $ SExpressionParseError err
    Right v -> do
      parseResult <- liftIO $ CSyn.top globalNonceGenerator halloc [] $ CSyn.prog v
      case parseResult of
        Left (CSyn.SyntaxParseError e) ->
          throwE (SyntaxParseError (CSyn.printSyntaxError e))
        Left err ->
          throwE (SyntaxParseError (Text.pack (show err)))
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
  IO (Either ParseOverridesYamlError ParsedOverridesYaml)
parseOverridesYaml yamlPath = runExceptT $ do
  res <- liftIO $ Yaml.decodeFileEither yamlPath
  case res of
    Left ex -> throwE $ YamlParseError ex
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
  IO (Either ResolveOverridesYamlError (ResolvedOverridesYaml w))
resolveOverridesYaml loadOpts mem fnOvNames (ParsedOverridesYaml parsedFunAddrs) = runExceptT $ do
  resolvedFunAddrs <-
    traverse
      ( \(addr, funName) -> do
          let addrWord = MM.memWord $ addr + loadOffset
          addrSegOff <-
            case Loader.resolveAbsoluteAddress mem addrWord of
              Just addrSegOff -> pure addrSegOff
              Nothing ->
                throwE $ AddressUnresolvable (MM.memWordValue addrWord)
          unless (Set.member funName fnOvNames) $
            throwE $
              FunctionNameNotFound (W4.functionName funName)
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
  ExceptT ParseOverridesYamlError IO ParsedOverridesYaml
parseFunctionAddressOverrides val = do
  mbObj <- asNullableObject val
  case mbObj of
    Nothing -> pure mempty
    Just obj -> do
      let objSansFunAddrOvs = KeyMap.delete funAddrOvsKey obj
      unless (KeyMap.null objSansFunAddrOvs) $
        throwE $
          UnexpectedYamlKeys $
            map Key.toText (KeyMap.keys objSansFunAddrOvs)
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
                        throwE $ InvalidAddress (Key.toText addrKey)
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
asString :: Aeson.Value -> ExceptT ParseOverridesYamlError IO Text
asString (Aeson.String t) = pure t
asString v =
  throwE $
    ExpectedString (Text.decodeUtf8 (BS.toStrict (Aeson.encode v)))

-- | Helper, not exported.
--
-- Assert that a JSON 'Aeson.Value' is an 'Aeson.Object'. If this is the case,
-- return the underlying object. Otherwise, throw an exception.
asObject :: Aeson.Value -> ExceptT ParseOverridesYamlError IO Aeson.Object
asObject (Aeson.Object o) = pure o
asObject v =
  throwE $
    ExpectedObject (Text.decodeUtf8 (BS.toStrict (Aeson.encode v)))

-- | Helper, not exported.
--
-- Assert that a JSON 'Aeson.Value' is an 'Aeson.Object' or 'Aeson.Null'. If it
-- is an 'Aeson.Object', return 'Just' the underlying object. If it is
-- 'Aeson.Null', return 'Nothing'. If neither of these are the case, throw an
-- exception.
asNullableObject :: Aeson.Value -> ExceptT ParseOverridesYamlError IO (Maybe Aeson.Object)
asNullableObject (Aeson.Object o) = pure $ Just o
asNullableObject Aeson.Null = pure Nothing
asNullableObject v =
  throwE $
    ExpectedNullableObject (Text.decodeUtf8 (BS.toStrict (Aeson.encode v)))
