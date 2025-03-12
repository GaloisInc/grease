{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Grease.MustFail
  ( excludeMustFail
  , oneMustFail
  , checkOneMustFail
  ) where

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.Function qualified as Function
import Data.List qualified as List
import Data.Traversable qualified as Traversable

import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online (OnlineBackend, withSolverProcess)
import Lang.Crucible.Backend.Simple (Flags)
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.Errors.MemoryError qualified as Mem
import Lang.Crucible.Simulator.SimError qualified as C

import What4.Expr.Builder qualified as W4
import What4.Interface qualified as W4
import What4.LabeledPred qualified as W4
import What4.Protocol.Online qualified as W4
import What4.SatResult qualified as W4
import qualified Data.Tuple as Tuple

-- | Should this proof obligation be excluded from consideration by the must-
-- fail heuristic?
excludeMustFail ::
  C.ProofObligation sym ->
  Maybe (Mem.BadBehavior sym) ->
  Bool
excludeMustFail obligation minfo =
  let reason = C.simErrorReason (obligation ^. Lens.to C.proofGoal ^. W4.labeledPredMsg) in
  let msg = C.simErrorReasonMsg reason in
  List.or @[]
  [ case minfo of
      -- Symbolic function pointers may arise from calling function pointers that
      -- appear in function arguments. This is not a bug, just something GREASE
      -- can't handle.
      Just (Mem.BBMemoryError (Mem.MemoryError _ (Mem.BadFunctionPointer Mem.SymbolicPointer))) ->
        True
      _ |  -- macaw-symbolic does not make it straightforward to track which terms
           -- give rise to out-of-bounds global writes, so we catch these errors
           -- here in a very hacky way. See
           -- https://github.com/GaloisInc/macaw/issues/429 for a proposal to
           -- improve macaw-symbolic's assertion tracking so that we can intercept
           -- this properly.
           "PointerWrite outside of static memory range" `List.isPrefixOf` msg &&
             -- Make an exception for the concretely-null pointer
             not ("PointerWrite outside of static memory range (known BlockID 0): 0x0" `List.isPrefixOf` msg)
        -> True
        |  otherwise
        -> False
  , -- Hitting a loop/recursion bound does not indicate a bug in the program
    case reason of
      C.ResourceExhausted {} -> True
      _ -> False
  ]

-- | Given a 'C.ProofObligation', produce a 'W4.Pred' that is unsatisfiable when
-- the obligation \"must fail\", i.e., it is satisfiable when either the goal
-- is satisfiable, or the program is able to take another path.
mustFailPred ::
  C.IsSymBackend sym bak =>
  bak ->
  C.ProofObligation sym ->
  IO (W4.Pred sym)
mustFailPred bak obligation = do
  let sym = C.backendGetSym bak
  let lp = C.proofGoal obligation

  -- If `notAssumps` is unsat, then the program must take this path.
  assumps <- C.assumptionsPred sym (C.proofAssumptions obligation)
  notAssumps <- W4.notPred sym assumps

  -- If the safety predicate is unsatisfiable and we necessarily took this
  -- path, then the predicate fails regardless of any data GREASE invented
  -- (e.g., argument values), and we report it.
  let goal = lp Lens.^. W4.labeledPred
  W4.orPred sym goal notAssumps

-- | Check if at least one of the given 'C.ProofObligation's is guaranteed to
-- fail, i.e., if the conjunction of their 'mustFailPred's is unsatisfiable.
oneMustFail ::
  ( W4.OnlineSolver solver
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st (Flags fm)
  , bak ~ OnlineBackend solver t st (Flags fm)
  ) =>
  bak ->
  [C.ProofObligation sym] ->
  IO Bool
oneMustFail bak obligations = do
  let sym = C.backendGetSym bak
  mustFail <-
    W4.andAllOf sym Lens.folded Monad.=<<
      Traversable.traverse (mustFailPred bak) obligations
  let onlineDisabled = Monad.fail "`must-fail` requires online solving to be enabled"
  withSolverProcess bak onlineDisabled Function.$ \solverProc ->
    W4.checkSatisfiable solverProc "must-fail heuristic" mustFail Monad.>>=
      \case
        W4.Unknown -> pure False
        W4.Sat () -> pure False
        W4.Unsat () -> pure True

-- | After heuristics have failed to classify some collection of errors,
-- apply the \"one must fail\" heuristic: Was at least one of the predicates
-- guaranteed to fail?
checkOneMustFail ::
  ( W4.OnlineSolver solver
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st (Flags fm)
  , bak ~ OnlineBackend solver t st (Flags fm)
  ) =>
  bak ->
  -- | Predicates for which no heuristic succeeded
  [(C.ProofObligation sym, Maybe (Mem.BadBehavior sym))] ->
  IO Bool
checkOneMustFail bak failed =
  if List.any (Tuple.uncurry excludeMustFail) failed
  then pure False
  else oneMustFail bak (List.map Tuple.fst failed)
