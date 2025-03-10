{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.Load.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map

import qualified Prettyprinter as PP

-- what4
import qualified What4.FunctionName as W4

-- macaw
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM

import Grease.Diagnostic.Severity (Severity(Info, Debug))

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
    W4.FunctionName ->
    -- | The address where the core was dumped.
    MM.MemWord w ->
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
            PP.hcat [ "Identified a candidate function entry point for function "
                    , PP.pretty (ppSymbol (Map.lookup tgt symMap) tgt)
                    , " because "
                    , PP.pretty (MD.ppFunReason rsn)
                    ]
          MD.ReportAnalyzeBlock _ _ baddr ->
            "Analyzing a block at address " <> PP.pretty baddr
      DiscoveredCoreDumpEntrypoint entrypointAddr entrypointName coreDumpAddr ->
        PP.vcat
          [ "Starting simulation at address" PP.<+>
            PP.pretty entrypointAddr PP.<+>
            PP.parens (PP.pretty entrypointName) PP.<>
            ", which is"
          , "the most likely function to contain the address" PP.<+>
            PP.pretty coreDumpAddr <> ", where the core was dumped"
          ]
    where
      ppSymbol :: MM.MemWidth w => Maybe BSC.ByteString -> MM.MemSegmentOff w -> String
      ppSymbol (Just fnName) addr = show addr ++ " (" ++ BSC.unpack fnName ++ ")"
      ppSymbol Nothing addr = show addr

severity :: Diagnostic -> Severity
severity =
  \case
    DiscoveryEvent{} -> Debug
    DiscoveredCoreDumpEntrypoint{} -> Info
