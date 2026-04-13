{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Analysis locations, i.e., locations in a program that direct @grease@
-- where to look during its reachability analysis.
--
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.AnalysisLoc (
  AnalysisLoc (..),
  TargetLoc (..),
  ResolvedTargetLoc (..),
  resolvedTargetAddr,
  resolveNearestFunction,
  resolveTargetLoc,
) where

import Data.Macaw.BinaryLoader.ELF qualified as Loader
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text
import Data.Tuple qualified as Tuple
import Data.Word (Word64)
import Grease.Macaw.BinMd (BinMd (binLoadOptions, binSymMap))
import Grease.Utility (segoffToAbsoluteAddr)
import Prettyprinter qualified as PP
import What4.FunctionName qualified as WFN

data AnalysisLoc
  = -- | A machine address (binaries only).
    AnalysisLocAddress Word64
  | -- | A function symbol name.
    AnalysisLocSymbol WFN.FunctionName
  deriving Show

-- | The 'AnalysisLoc' for the target. This data type is for targets that have
-- been parsed from user input but not yet validated. For validated targets,
-- see 'ResolvedTargetLoc'.
newtype TargetLoc = TargetLoc {targetAnalysisLoc :: AnalysisLoc}
  deriving Show

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

-- | Given an address in a binary, find the address and name of the nearest
-- function at or before that address. We use this as a heuristic to determine
-- what function is likely to contain the supplied address.
--
-- Returns 'Nothing' if either (1) the address does not fall within any mapped
-- memory segment, or (2) no function symbol precedes the address in the symbol
-- map. Both cases are non-fatal: the result is used only for display purposes
-- (see 'ResolvedTargetLoc'), not to drive analysis.
--
-- NB: This heuristic is not perfect, as we really ought to be considering the
-- size of the function to determine if it actually contains the supplied
-- address. Doing so would require plumbing through some extra function symbol
-- information to the macaw-loader API, however, which would prove annoying.
-- For now, this heuristic likely suffices, and it is the same approach used to
-- power the @--core-dump@ flag in @grease@.
resolveNearestFunction ::
  MM.MemWidth w =>
  MM.Memory w ->
  Map.Map (MM.MemSegmentOff w) WFN.FunctionName ->
  MM.MemWord w ->
  Maybe (MM.MemSegmentOff w, WFN.FunctionName)
resolveNearestFunction mem symMap addr = do
  addrSegOff <- Loader.resolveAbsoluteAddress mem addr
  Map.lookupLE addrSegOff symMap

-- | Resolve an 'AnalysisLoc' against a loaded binary to produce a
-- 'ResolvedTargetLoc'.
resolveTargetLoc ::
  (w ~ MC.ArchAddrWidth arch, MC.MemWidth w) =>
  MM.Memory w ->
  BinMd arch ->
  AnalysisLoc ->
  ResolvedTargetLoc w
resolveTargetLoc mem binMd loc =
  let symMapUtf8 = fmap (WFN.functionNameFromText . Text.decodeUtf8Lenient) (binSymMap binMd)
      revSymMap = Map.fromList $ fmap Tuple.swap $ Map.toList symMapUtf8
      loadOffset = fromMaybe 0 (LC.loadOffset (binLoadOptions binMd))
   in case loc of
        AnalysisLocAddress addr ->
          let targetAddr = MC.memWord $ addr + loadOffset
              nearestFun = resolveNearestFunction mem symMapUtf8 targetAddr
           in ResolvedTargetLocAddress targetAddr nearestFun
        AnalysisLocSymbol name ->
          case Map.lookup name revSymMap of
            Just segOff ->
              let targetAddr = segoffToAbsoluteAddr mem segOff
               in ResolvedTargetLocSymbol name (Just targetAddr)
            Nothing ->
              ResolvedTargetLocSymbol name Nothing
