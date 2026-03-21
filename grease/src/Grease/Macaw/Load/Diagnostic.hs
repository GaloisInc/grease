{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Load.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.ByteString.Char8 qualified as BSC
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as MD
import Data.Macaw.Memory qualified as MM
import Data.Map qualified as Map
import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity (Debug, Info))
import Prettyprinter qualified as PP
import What4.FunctionName qualified as WFN

data Diagnostic where
  DiscoveryEvent ::
    ( MC.ArchConstraints arch
    , w ~ MC.RegAddrWidth (MC.ArchReg arch)
    ) =>
    MD.AddrSymMap w ->
    MD.DiscoveryEvent arch ->
    Diagnostic
  DiscoveredCoreDumpEntrypoint ::
    MM.MemWidth w =>
    -- | The address of the entrypoint function.
    MM.MemSegmentOff w ->
    -- | The name of the entrypoint function.
    WFN.FunctionName ->
    -- | The address where the core was dumped.
    MM.MemWord w ->
    Diagnostic
  LoadedSharedLibrary ::
    -- | Path to the shared library.
    FilePath ->
    -- | Index of the shared library (1-based).
    Word64 ->
    Diagnostic
  SharedLibraryNotFound ::
    -- | Name of the shared library that was not found.
    FilePath ->
    Diagnostic
  SharedLibraryLoadingDisabled ::
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty d =
    case d of
      DiscoveryEvent symMap de ->
        case de of
          MD.ReportAnalyzeFunction memOff ->
            "Starting to analyze a function at address " <> PP.pretty memOff
          MD.ReportAnalyzeFunctionDone memOff ->
            "Finished analyzing a function at address " <> PP.pretty memOff
          MD.ReportIdentifyFunction _ tgt rsn ->
            PP.hcat
              [ "Identified a candidate function entry point for function "
              , PP.pretty (ppSymbol (Map.lookup tgt symMap) tgt)
              , " because "
              , PP.pretty (MD.ppFunReason rsn)
              ]
          MD.ReportAnalyzeBlock _ _ baddr ->
            "Analyzing a block at address " <> PP.pretty baddr
      DiscoveredCoreDumpEntrypoint entrypointAddr entrypointName coreDumpAddr ->
        PP.vcat
          [ "Starting simulation at address"
              PP.<+> PP.pretty entrypointAddr
              PP.<+> PP.parens (PP.pretty entrypointName)
              PP.<> ", which is"
          , "the most likely function to contain the address"
              PP.<+> PP.pretty coreDumpAddr
              <> ", where the core was dumped"
          ]
      LoadedSharedLibrary path idx ->
        "Loaded shared library"
          PP.<+> PP.pretty path
          PP.<+> "at index"
          PP.<+> PP.pretty idx
      SharedLibraryNotFound name ->
        "Shared library not found:"
          PP.<+> PP.pretty name
      SharedLibraryLoadingDisabled ->
        "Shared library loading disabled"
   where
    ppSymbol :: MM.MemWidth w => Maybe BSC.ByteString -> MM.MemSegmentOff w -> String
    ppSymbol (Just fnName) addr = show addr ++ " (" ++ BSC.unpack fnName ++ ")"
    ppSymbol Nothing addr = show addr

severity :: Diagnostic -> Severity
severity =
  \case
    DiscoveryEvent{} -> Debug
    DiscoveredCoreDumpEntrypoint{} -> Info
    LoadedSharedLibrary{} -> Info
    SharedLibraryNotFound{} -> Info
    SharedLibraryLoadingDisabled{} -> Info
