{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis locations, i.e., locations in a program that direct @screach@
-- where to look during its reachability analysis. Many of the data types in
-- this module are directly inspired by "Grease.Entrypoint", although they are
-- not quite the same conceptually, as @screach@ has a distinction between
-- /entrypoint/ locations and /target/ locations that does not exist in
-- @grease@.
module Screach.AnalysisLoc (
  AnalysisLoc (..),
  EntryLoc (..),
  entryLocToGreaseEntrypoint,
  EntryLocParser,
  entrySymbolStartupOvParser,
  entryAddressStartupOvParser,
  TargetLoc (..),
  ResolvedTargetLoc (..),
  resolvedTargetAddr,
) where

import Control.Applicative (Alternative (..))
import Data.Macaw.Memory qualified as MM
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Data.Word (Word64)
import Grease.Entrypoint as Grease
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.FunctionName qualified as WFN

-- | An 'AnalysisLoc' is a location in a program that directs @screach@ where to
-- look during its reachability analysis. We use 'AnalysisLoc's to specify both
-- the entrypoint location (where the program should start execution) and the
-- target location (where the program should end up, assuming that the target is
-- reachable).
data AnalysisLoc
  = -- | A machine address (binaries only).
    AnalysisLocAddress Word64
  | -- | A function symbol name.
    AnalysisLocSymbol WFN.FunctionName

-- | The 'AnalysicLoc' for the entrypoint, plus an optional startup override
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

-- | The 'AnalysicLoc' for the target. This data type is for targets that have
-- been parsed from user input but not yet validated. For validated targets,
-- see 'ResolvedTargetLoc'.
newtype TargetLoc = TargetLoc {targetAnalysisLoc :: AnalysisLoc}

-- | The target location after validating a 'TargetLoc' to verify that the
-- target actually exists somewhere in the program.
data ResolvedTargetLoc w
  = -- | A machine address (binaries only).
    ResolvedTargetLocAddress
      -- | The address.
      (MM.MemWord w)
      -- | If the address is contained within a function, this contains 'Just'
      -- the address and name of the function.
      (Maybe (MM.MemSegmentOff w, WFN.FunctionName))
  | -- | A function symbol name.
    ResolvedTargetLocSymbol
      -- | The function's name.
      WFN.FunctionName
      -- | If the program is a binary, this contains 'Just' the function's
      -- address. For S-expression programs, this will always be 'Nothing'.
      (Maybe (MM.MemWord w))

-- | The address associated with this target.
--
-- See docs on 'ResolvedTargetLoc' for details.
resolvedTargetAddr :: ResolvedTargetLoc w -> Maybe (MM.MemWord w)
resolvedTargetAddr =
  \case
    ResolvedTargetLocAddress targetAddr _ -> Just targetAddr
    ResolvedTargetLocSymbol _ mbTgtAddr -> mbTgtAddr

instance MM.MemWidth w => PP.Pretty (ResolvedTargetLoc w) where
  pretty rtLoc =
    case rtLoc of
      ResolvedTargetLocAddress addr mbNearestFun ->
        let ppNearestFun =
              case mbNearestFun of
                Nothing -> mempty
                Just (nearestFunAddr, nearestFunName) ->
                  PP.brackets $
                    "in function"
                      PP.<+> PP.squotes (PP.pretty nearestFunName)
                      PP.<+> "at address"
                      PP.<+> PP.pretty nearestFunAddr
         in "address" PP.<+> PP.pretty addr PP.<+> ppNearestFun
      ResolvedTargetLocSymbol name mbLocAddr ->
        let ppLocAddr =
              case mbLocAddr of
                Nothing -> mempty
                Just locAddr ->
                  PP.brackets $ "address" PP.<+> PP.pretty locAddr
         in "function" PP.<+> PP.squotes (PP.pretty name) PP.<+> ppLocAddr
