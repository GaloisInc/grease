{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Main.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Void (Void, absurd)
import Grease.Diagnostic.Severity (Severity(Debug, Info, Warn))
import Grease.Entrypoint (Entrypoint, EntrypointLocation)
import Grease.Output (BatchStatus)
import Grease.Requirement (Requirement, displayReq)
import Grease.Time (Nanoseconds, nanosToMillis)
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Translation qualified as Trans
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L

data Diagnostic where
  AnalyzedEntrypoint ::
    EntrypointLocation -> BatchStatus -> Diagnostic
  AnalyzingEntrypoint ::
    Entrypoint -> Diagnostic
  FinishedAnalyzingEntrypoint ::
    EntrypointLocation -> Nanoseconds -> Diagnostic
  NoEntrypoints ::
    Diagnostic
  LLVMTranslationWarning ::
    Trans.LLVMTranslationWarning -> Diagnostic
  SimulationTestingRequirements ::
    [Requirement] -> Diagnostic
  SimulationAllGoalsPassed ::
    Diagnostic
  SimulationGoalsFailed ::
    Diagnostic
  TargetCFG ::
    C.PrettyExt ext => C.CFG ext blocks args ret -> Diagnostic
  TypeContextError ::
    PP.Doc Void -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      AnalyzedEntrypoint entry status ->
        PP.hsep
        [ "Finished analyzing"
        , PP.squotes (PP.pretty entry) PP.<> "."
        , PP.pretty status
        ]
      AnalyzingEntrypoint entry ->
        "Analyzing from entrypoint" PP.<+> PP.pretty entry
      FinishedAnalyzingEntrypoint entry duration ->
        PP.hsep
        [ "Analysis of"
        , PP.squotes (PP.pretty entry)
        , "took"
        , PP.pretty (nanosToMillis duration)
        ]
      NoEntrypoints ->
        "No entry points specified, analyzing all known functions."
      LLVMTranslationWarning (Trans.LLVMTranslationWarning (L.Symbol symbol) pos warn) ->
        PP.pretty symbol <> ":" <> PP.pretty pos <> ":" PP.<+> PP.pretty warn
      SimulationTestingRequirements rs ->
        if List.null rs
        then "Not testing any requirements beyond memory safety"
        else PP.pretty $
               "Testing requirements: " <> Text.intercalate ", " (List.map displayReq rs)
      SimulationAllGoalsPassed ->
        "All goals passed (with assertions added)!"
      SimulationGoalsFailed ->
        "Failed to prove goals (with assertions added)"
      TargetCFG cfg ->
        PP.nest 2 $
          PP.vcat
            [ "Target CFG:" PP.<+> PP.viaShow (C.cfgHandle cfg)
            , C.ppCFG' True (C.postdomInfo cfg) cfg
            ]
      TypeContextError e -> fmap absurd e

severity :: Diagnostic -> Severity
severity =
  \case
    AnalyzedEntrypoint {} -> Info
    AnalyzingEntrypoint {} -> Info
    FinishedAnalyzingEntrypoint {} -> Debug
    NoEntrypoints -> Warn
    LLVMTranslationWarning{} -> Warn
    SimulationTestingRequirements{} -> Info
    SimulationAllGoalsPassed{} -> Info
    SimulationGoalsFailed{} -> Info
    TargetCFG{} -> Debug
    TypeContextError{} -> Warn
