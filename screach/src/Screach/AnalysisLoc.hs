{-# LANGUAGE OverloadedStrings #-}

-- | Analysis locations, i.e., locations in a program that direct @screach@
-- where to look during its reachability analysis. Many of the data types in
-- this module are directly inspired by "Grease.Entrypoint", although they are
-- not quite the same conceptually, as @screach@ has a distinction between
-- /entrypoint/ locations and /target/ locations that does not exist in
-- @grease@.
module Screach.AnalysisLoc (
  EntryLoc (..),
  entryLocToGreaseEntrypoint,
  EntryLocParser,
  entrySymbolStartupOvParser,
  entryAddressStartupOvParser,
) where

import Control.Applicative (Alternative (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Grease.Reachability.AnalysisLoc (AnalysisLoc (AnalysisLocAddress, AnalysisLocSymbol))
import Grease.Entrypoint as Grease
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.FunctionName qualified as WFN

-- | The 'AnalysisLoc' for the entrypoint, plus an optional startup override
-- path.
data EntryLoc = EntryLoc
  { entryAnalysisLoc :: AnalysisLoc
  , entryStartupOvPath :: Maybe FilePath
  }

-- | Convert an 'EntryLoc' to a 'Grease.Entrypoint'.
entryLocToGreaseEntrypoint :: EntryLoc -> Grease.Entrypoint
entryLocToGreaseEntrypoint (EntryLoc analysisLoc startupOvPath) =
  Grease.Entrypoint
    { Grease.entrypointLocation = location
    , Grease.entrypointStartupOvPath = startupOvPath
    }
 where
  location :: Grease.EntrypointLocation
  location =
    case analysisLoc of
      AnalysisLocAddress addr ->
        Grease.EntrypointAddress $
          PP.renderStrict $
            PP.layoutCompact $
              PP.pretty addr
      AnalysisLocSymbol name ->
        Grease.EntrypointSymbolName $ WFN.functionName name

-- | Parse an 'EntryLoc' in the format @SYMBOL:FILE@, where @SYMBOL@ is a
-- function symbol and @FILE@ is a path to a startup override.
entrySymbolStartupOvParser :: EntryLocParser EntryLoc
entrySymbolStartupOvParser = do
  symbolName <- TM.takeWhileP (Just "function symbol") (/= ':')
  _ <- TMC.char ':'
  ovPath <- ovPathParser
  TM.eof
  pure $
    EntryLoc
      { entryAnalysisLoc =
          AnalysisLocSymbol $ WFN.functionNameFromText symbolName
      , entryStartupOvPath = ovPath
      }

-- | Parse an 'EntryLoc' in the format @ADDR:FILE@, where @ADDR@ is a function
-- address (in hexadecimal) and @FILE@ is a path to a startup override.
entryAddressStartupOvParser :: EntryLocParser EntryLoc
entryAddressStartupOvParser = do
  addr <- symbol "0x" *> TMCL.hexadecimal
  _ <- TMC.char ':'
  ovPath <- ovPathParser
  TM.eof
  pure $
    EntryLoc
      { entryAnalysisLoc = AnalysisLocAddress addr
      , entryStartupOvPath = ovPath
      }

-- | A @megaparsec@ parser type for 'Entrypoint's.
type EntryLocParser = TM.Parsec Void Text

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> EntryLocParser Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: EntryLocParser ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse an override path.
ovPathParser :: EntryLocParser (Maybe FilePath)
ovPathParser = do
  ovPath <- TM.takeWhileP (Just "override path") (const True)
  pure $ Just $ Text.unpack ovPath
