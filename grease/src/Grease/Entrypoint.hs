{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Entrypoint (
  -- * Core definitions
  Entrypoint (..),
  EntrypointLocation (..),
  entrypointNoStartupOv,
  entrypointFromBytestring,

  -- * Parsing entrypoints
  EntrypointParser,
  entrypointSymbolStartupOvParser,
  entrypointAddressStartupOvParser,

  -- * @EntrypointCfgs@ and friends
  EntrypointCfgs (..),
  entrypointCfgsToSsa,
  toSsaSomeCfg,
  MacawEntrypointCfgs (..),
  StartupOvError (..),
  StartupOv (..),
  CFGNotFound (..),
  parseEntrypointStartupOv,
) where

import Control.Applicative (Alternative (empty))
import Data.ByteString qualified as BS
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Void (Void)
import Data.Word (Word64)
import Grease.Syntax (ParseProgramError, parseProgram, parsedProgramCfgMap)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Numeric (showHex)
import Prettyprinter qualified as PP
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.FunctionName qualified as WFN

-- | An 'EntrypointLocation' and its optional startup override.
data Entrypoint
  = Entrypoint
  { entrypointLocation :: EntrypointLocation
  -- ^ Where the entrypoint is located in a program.
  , entrypointStartupOvPath :: Maybe FilePath
  -- ^ An optional path to a startup override.
  }
  deriving (Eq, Ord, Show)

-- | The location of an entrypoint in a program.
data EntrypointLocation
  = -- | A function symbol.
    EntrypointSymbolName Text
  | -- | A function address (as supplied on the CLI).
    EntrypointAddress Text
  | -- | A core dump file.
    EntrypointCoreDump FilePath
  deriving (Eq, Ord, Show)

-- | Create an 'Entrypoint' without a startup override.
entrypointNoStartupOv :: EntrypointLocation -> Entrypoint
entrypointNoStartupOv loc =
  Entrypoint
    { entrypointLocation = loc
    , entrypointStartupOvPath = Nothing
    }

-- | Create an 'Entrypoint' from a UTF-8–encoded 'BS.ByteString'. This assumes
-- that the entrypoint does not have a corresponding startup override.
entrypointFromBytestring :: BS.ByteString -> Entrypoint
entrypointFromBytestring =
  entrypointNoStartupOv . EntrypointSymbolName . Text.decodeUtf8

-- | A @megaparsec@ parser type for 'Entrypoint's.
type EntrypointParser = TM.Parsec Void Text

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> EntrypointParser Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: EntrypointParser ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse an override path.
ovPathParser :: EntrypointParser (Maybe FilePath)
ovPathParser = do
  ovPath <- TM.takeWhileP (Just "override path") (const True)
  pure $ Just $ Text.unpack ovPath

-- | Parse an 'Entrypoint' in the format @SYMBOL:FILE@, where @SYMBOL@ is a
-- function symbol and @FILE@ is a path to a startup override.
entrypointSymbolStartupOvParser :: EntrypointParser Entrypoint
entrypointSymbolStartupOvParser = do
  symbolName <- TM.takeWhileP (Just "function symbol") (/= ':')
  _ <- TMC.char ':'
  ovPath <- ovPathParser
  TM.eof
  pure $
    Entrypoint
      { entrypointLocation = EntrypointSymbolName symbolName
      , entrypointStartupOvPath = ovPath
      }

-- | Parse an 'Entrypoint' in the format @ADDR:FILE@, where @ADDR@ is a function
-- address (in hexadecimal) and @FILE@ is a path to a startup override.
entrypointAddressStartupOvParser :: EntrypointParser Entrypoint
entrypointAddressStartupOvParser = do
  (addr :: Word64) <- symbol "0x" *> TMCL.hexadecimal
  let addrStr :: String
      addrStr = showString "0x" $ showHex addr ""
  _ <- TMC.char ':'
  ovPath <- ovPathParser
  TM.eof
  pure $
    Entrypoint
      { entrypointLocation = EntrypointAddress (Text.pack addrStr)
      , entrypointStartupOvPath = ovPath
      }

instance PP.Pretty EntrypointLocation where
  pretty =
    \case
      EntrypointAddress addr -> PP.pretty addr
      EntrypointSymbolName nm -> PP.pretty nm
      EntrypointCoreDump fp -> PP.pretty fp

instance PP.Pretty Entrypoint where
  pretty entry =
    case entrypointLocation entry of
      EntrypointAddress addr ->
        PP.pretty addr PP.<+> ppStartupOv
      EntrypointSymbolName nm ->
        PP.squotes (PP.pretty nm) PP.<+> ppStartupOv
      EntrypointCoreDump fp ->
        PP.pretty fp PP.<+> PP.parens "core dump file" PP.<+> ppStartupOv
   where
    ppStartupOv =
      case entrypointStartupOvPath entry of
        Nothing ->
          mempty
        Just ovPath ->
          PP.parens ("startup override:" PP.<+> PP.pretty ovPath)

-- | A startup override to run just before the user-specified entrypoint
-- function. This is parameterized over the type of CFG to run (@cfg@).
data StartupOv cfg
  = StartupOv
  { startupOvCfg :: cfg
  -- ^ The CFG of the startup override's @startup@ function.
  , startupOvForwardDecs :: Map WFN.FunctionName C.SomeHandle
  -- ^ Forward declarations declared in the startup override file.
  }
  deriving Functor

-- | All of the CFGs and associated information related to a user-specified
-- entrypoint function. This is parameterized over the type of CFG to run
-- (@cfg@).
data EntrypointCfgs cfg
  = EntrypointCfgs
  { entrypointStartupOv :: Maybe (StartupOv cfg)
  -- ^ An optional startup override. If 'Just', the corresponding
  -- 'startupOvCFG' will be called before calling the entrypoint's CFG.
  , entrypointCfg :: cfg
  -- ^ The entrypoint's CFG.
  }
  deriving Functor

-- | The CFGs for an entrypoint for a function in a Macaw program (i.e., machine
-- code or Macaw S-expression code).
data MacawEntrypointCfgs arch
  = MacawEntrypointCfgs
      -- | The CFG for the user-requested entrypoint function, as well as the CFG
      -- for the startup override (if one was supplied).
      (EntrypointCfgs (C.Reg.AnyCFG (Symbolic.MacawExt arch)))
      -- | If simulating a binary, the second element of the pair is 'Just' the
      -- address of the entrypoint function. Otherwise, the second element is
      -- Nothing.
      (Maybe (MC.ArchSegmentOff arch))

-- | Error type for 'parseEntrypointStartupOv'
data CFGNotFound = CFGNotFound
  { cfgNotFoundFuncName :: Text
  , cfgNotFoundPath :: FilePath
  }

data StartupOvError
  = StartupOvParseError ParseProgramError
  | StartupOvCFGNotFound CFGNotFound

instance PP.Pretty CFGNotFound where
  pretty (CFGNotFound funcName filePath) =
    PP.vcat
      [ "Could not find a function named" PP.<+> PP.squotes (PP.pretty funcName)
      , "In the startup override located in" PP.<+> PP.pretty filePath
      ]

instance PP.Pretty StartupOvError where
  pretty =
    \case
      StartupOvParseError err -> PP.pretty err
      StartupOvCFGNotFound err -> PP.pretty err

-- | Parse a startup override in Crucible S-expression syntax and perform a
-- light amount of validation.
parseEntrypointStartupOv ::
  ( C.IsSyntaxExtension ext
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  C.HandleAllocator ->
  FilePath ->
  IO (Either StartupOvError (StartupOv (C.Reg.AnyCFG ext)))
parseEntrypointStartupOv halloc startupOvPath = do
  -- Parse the program...
  startupOvProgResult <- parseProgram halloc startupOvPath
  case startupOvProgResult of
    Left parseErr -> pure $ Left $ StartupOvParseError parseErr
    Right startupOvProg -> do
      -- ...ensure it has no externs...
      CSyn.assertNoExterns (CSyn.parsedProgExterns startupOvProg)
      --- ...and then ensure that it has a function named `startup`.
      case Map.lookup "startup" (parsedProgramCfgMap startupOvProg) of
        Nothing ->
          pure $
            Left $
              StartupOvCFGNotFound $
                CFGNotFound
                  { cfgNotFoundFuncName = "startup"
                  , cfgNotFoundPath = startupOvPath
                  }
        Just cfg ->
          pure $
            Right $
              StartupOv
                { startupOvCfg = cfg
                , startupOvForwardDecs = CSyn.parsedProgForwardDecs startupOvProg
                }

-- | Convert the CFGs in an 'EntrypointCfgs' to SSA form.
entrypointCfgsToSsa ::
  C.IsSyntaxExtension ext =>
  EntrypointCfgs (C.Reg.SomeCFG ext args ret) ->
  EntrypointCfgs (C.SomeCFG ext args ret)
entrypointCfgsToSsa = (toSsaSomeCfg <$>)

-- | Convert a register-based CFG ('C.Reg.SomeCFG') to an SSA-based CFG
-- ('C.SomeCFG').
toSsaSomeCfg ::
  C.IsSyntaxExtension ext =>
  C.Reg.SomeCFG ext init ret ->
  C.SomeCFG ext init ret
toSsaSomeCfg (C.Reg.SomeCFG cfg) = C.toSSA cfg
