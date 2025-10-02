{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Main.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Functor.Const (Const)
import Data.LLVM.BitCode (ParseWarning, ppParseWarnings)
import Data.List qualified as List
import Data.Macaw.Memory qualified as MM
import Data.Parameterized.Context qualified as Ctx
import Data.Sequence (Seq)
import Data.Text qualified as Text
import Data.Void (Void, absurd)
import Grease.Diagnostic.Severity (Severity (Debug, Error, Info, Warn))
import Grease.Entrypoint (Entrypoint, EntrypointLocation)
import Grease.Output (BatchStatus)
import Grease.Requirement (Requirement, displayReq)
import Grease.Shape (ArgShapes (ArgShapes), ExtShape, PrettyExt)
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Print qualified as ShapePP
import Grease.Time (Nanoseconds, nanosToMillis)
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Prettyprinter qualified as PP

data Diagnostic where
  AnalyzedEntrypoint ::
    EntrypointLocation -> BatchStatus -> Diagnostic
  AnalyzingEntrypoint ::
    Entrypoint ->
    -- | Index of current entrypoint
    Int ->
    -- | Total number of entrypoints
    Int ->
    Diagnostic
  BitcodeParseWarnings ::
    Seq ParseWarning -> Diagnostic
  LoadedPrecondition ::
    forall w ext tag tys.
    ( ExtShape ext ~ PtrShape ext w
    , PrettyExt ext tag
    ) =>
    FilePath ->
    MM.AddrWidthRepr w ->
    -- | Argument names
    Ctx.Assignment (Const String) tys ->
    ArgShapes ext tag tys ->
    Diagnostic
  FinishedAnalyzingEntrypoint ::
    EntrypointLocation -> Nanoseconds -> Diagnostic
  MalformedElf ::
    PP.Doc Void ->
    Diagnostic
  NoEntrypoints ::
    Diagnostic
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
  UserError ::
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
      AnalyzingEntrypoint entry idx total ->
        PP.hsep
          [ "Analyzing entrypoint"
          , PP.hcat [PP.pretty (idx + 1), "/", PP.pretty total] <> ","
          , PP.pretty entry
          ]
      BitcodeParseWarnings warns -> PP.viaShow (ppParseWarnings warns)
      FinishedAnalyzingEntrypoint entry duration ->
        PP.hsep
          [ "Analysis of"
          , PP.squotes (PP.pretty entry)
          , "took"
          , PP.pretty (nanosToMillis duration)
          ]
      LoadedPrecondition path w argNames (ArgShapes argShapes) ->
        PP.vcat
          [ "Loaded precondition from path" PP.<+> PP.pretty path PP.<> ":"
          , ShapePP.evalPrinter (printCfg w) (ShapePP.printNamedShapes argNames argShapes)
          ]
      MalformedElf err ->
        "Malformed ELF file: " <> fmap absurd err
      NoEntrypoints ->
        "No entry points specified, analyzing all known functions."
      SimulationTestingRequirements rs ->
        if List.null rs
          then "Not testing any requirements beyond memory safety"
          else
            PP.pretty $
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
      UserError e -> "User error:" PP.<+> fmap absurd e
   where
    printCfg :: MM.AddrWidthRepr w -> ShapePP.PrinterConfig w
    printCfg w =
      ShapePP.PrinterConfig
        { ShapePP.cfgAddrWidth = w
        , ShapePP.cfgRleThreshold = 8
        }

severity :: Diagnostic -> Severity
severity =
  \case
    AnalyzedEntrypoint{} -> Info
    AnalyzingEntrypoint{} -> Info
    BitcodeParseWarnings{} -> Warn
    FinishedAnalyzingEntrypoint{} -> Debug
    LoadedPrecondition{} -> Debug
    MalformedElf{} -> Error
    NoEntrypoints -> Warn
    SimulationTestingRequirements{} -> Debug
    SimulationAllGoalsPassed{} -> Info
    SimulationGoalsFailed{} -> Info
    TargetCFG{} -> Debug
    TypeContextError{} -> Warn
    UserError{} -> Error
