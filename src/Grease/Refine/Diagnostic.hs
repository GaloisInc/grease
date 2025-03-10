{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Refine.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Control.Lens ((^.))
import Data.Functor.Const (Const(..))
import qualified Data.List as List

import qualified Prettyprinter as PP

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx

-- what4
import qualified What4.Interface as W4
import qualified What4.LabeledPred as W4
import qualified What4.ProgramLoc as W4

-- crucible
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Errors as Mem
import qualified Lang.Crucible.LLVM.MemModel.CallStack as Mem

-- macaw-base
import qualified Data.Macaw.Memory as MM

import Grease.Diagnostic.Severity (Severity(Info, Debug))
import qualified Grease.Heuristic.Result as Heuristic
import Grease.Shape (ArgShapes (..), ExtShape, PrettyExt)
import Grease.Shape.Pointer (PtrShape)
import qualified Grease.Shape.Print as ShapePP

data Diagnostic where
  CantRefine ::
    Heuristic.CantRefine -> Diagnostic
  ExecutionResult ::
    C.ExecResult p sym ext (C.RegEntry sym ret) -> Diagnostic
  GoalNoMatchingHeuristic ::
    Diagnostic
  GoalMatchingHeuristic ::
    Diagnostic
  NoAnnotationOnPredicate ::
    Diagnostic
  RefinementFinalPrecondition ::
    forall w ext tag tys.
    ( ExtShape ext ~ PtrShape ext w
    , PrettyExt ext tag
    ) =>
    MM.AddrWidthRepr w ->
    -- | Argument names
    Ctx.Assignment (Const String) tys ->
    ArgShapes ext tag tys ->
    Diagnostic
  RefinementLoopMaximumIterationsExceeded ::
    Diagnostic
  RefinementLoopRetrying ::
    Diagnostic
  RefinementLoopAllGoalsPassed ::
    Diagnostic
  RefinementLoopNoHeuristic ::
    Diagnostic
  RefinementUsingPrecondition ::
    forall w ext tag tys.
    ( ExtShape ext ~ PtrShape ext w
    , PrettyExt ext tag
    ) =>
    MM.AddrWidthRepr w ->
    -- | Argument names
    Ctx.Assignment (Const String) tys ->
    ArgShapes ext tag tys ->
    Diagnostic
  SolverGoalPassed ::
    W4.ProgramLoc -> Diagnostic
  SolverGoalFailed ::
    W4.IsExpr (W4.SymExpr sym) =>
    -- | Symbolic backend
    sym ->
    -- | Goal that failed (the negation of this predicate was satisfiable)
    W4.LabeledPred (W4.Pred sym) C.SimError ->
    -- | Description of the problem, if available
    Maybe (Mem.CallStack, Mem.BadBehavior sym) ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      CantRefine reason ->
        PP.pretty reason
      ExecutionResult r ->
        case r of
          C.FinishedResult _ partRes ->
            case partRes of
              C.TotalRes {} -> "All execution paths returned a result"
              C.PartialRes {} -> "At least one execution path returned a result"
          C.AbortedResult {} -> "All execution paths aborted"
          C.TimeoutResult {} -> "Symbolic execution timed out!"
      GoalNoMatchingHeuristic ->
        "Could not identify heuristic for goal, skipping"
      GoalMatchingHeuristic ->
        "Found a matching heuristic and refined configuration"
      NoAnnotationOnPredicate ->
        "Warning: no annotation on predicate!"
      RefinementFinalPrecondition w argNames (ArgShapes argShapes) ->
        PP.vcat
          [ "Final refined precondition:"
          , ShapePP.evalPrinter (printCfg w) (ShapePP.printNamedShapes argNames argShapes)
          ]
      RefinementLoopMaximumIterationsExceeded ->
        "Exceeded the maximum number of iterations"
      RefinementLoopRetrying ->
        "Retrying with updated state"
      RefinementLoopAllGoalsPassed ->
        "All goals passed!"
      RefinementLoopNoHeuristic ->
        "Unable to find a heuristic for any goal"
      RefinementUsingPrecondition w argNames (ArgShapes argShapes) ->
        PP.vcat
          [ "Using precondition:"
          , ShapePP.evalPrinter (printCfg w) (ShapePP.printNamedShapes argNames argShapes)
          ]
      SolverGoalPassed loc ->
        PP.hsep ["Goal from", PP.pretty (W4.plSourceLoc loc), "passed"]
      SolverGoalFailed _sym lp minfo ->
        PP.vcat $
          [ "Goal failed:"
          , PP.indent 4 (C.ppSimError $ lp ^. W4.labeledPredMsg)
          -- , PP.indent 4 (W4.ppExpr $ lp ^. W4.labeledPred)
          , case minfo of
              Nothing -> "<no details available>"
              Just (callStack, bb) ->
                let ppCs = Mem.ppCallStack callStack
                in PP.vcat $
                     [ PP.indent 4 (Mem.ppBB bb) ] List.++
                       -- HACK(crucible#1112): No Eq on Doc, no access to cs
                       if Mem.null callStack
                       then []
                       else ["in context:", PP.indent 2 ppCs]
          ]
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
    CantRefine{} -> Info
    ExecutionResult{} -> Debug
    GoalNoMatchingHeuristic{} -> Debug
    GoalMatchingHeuristic{} -> Debug
    NoAnnotationOnPredicate{} -> Info
    RefinementFinalPrecondition{} -> Debug
    RefinementLoopMaximumIterationsExceeded{} -> Info
    RefinementLoopRetrying{} -> Debug
    RefinementLoopAllGoalsPassed{} -> Info
    RefinementLoopNoHeuristic{} -> Info
    RefinementUsingPrecondition{} -> Debug
    SolverGoalPassed{} -> Debug
    SolverGoalFailed{} -> Debug

