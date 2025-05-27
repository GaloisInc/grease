{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Grease.BranchTracer
  ( BranchTracer(..)
  , branchTracerFeature
  , greaseBranchTracerFeature
  ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Grease.BranchTracer.Diagnostic qualified as Diag
import Grease.Diagnostic (GreaseLogAction, Diagnostic(BranchTracerDiagnostic))
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lumberjack qualified as LJ
import What4.Interface qualified as W4

-- | 'IO' action to run upon reaching a symbolic branch.
newtype BranchTracer p sym ext
  = BranchTracer
    { getBranchTracer ::
      forall rtp f args postdom_args.
      W4.Pred sym {- predicate to branch on -} ->
      C.PausedFrame p sym ext rtp f {- true path -} ->
      C.PausedFrame p sym ext rtp f  {- false path -} ->
      C.CrucibleBranchTarget f postdom_args {- merge point -} ->
      C.SimState p sym ext rtp f ('Just args) {- simulator state -} ->
      IO ()
    }

-- | Execute an 'IO' action for each symbolic branch executed.
branchTracerFeature ::
  BranchTracer p sym ext ->
  C.ExecutionFeature p sym ext rtp
branchTracerFeature tracer = C.ExecutionFeature $
  \case
    C.SymbolicBranchState stPred tpath fpath mergePoint simState -> do
      getBranchTracer tracer stPred tpath fpath mergePoint simState
      pure C.ExecutionFeatureNoChange
    _ -> pure C.ExecutionFeatureNoChange

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (BranchTracerDiagnostic diag)

greaseBranchTracerFeature ::
  GreaseLogAction ->
  C.ExecutionFeature p sym ext rtp
greaseBranchTracerFeature la =
  branchTracerFeature $
    BranchTracer $
      \_pred _tframe _fframe _tgt simState ->
        doLog la (Diag.ReachedBranch (simState ^. C.stateLocation))
