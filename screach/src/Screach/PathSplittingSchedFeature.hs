{-# LANGUAGE LambdaCase #-}

module Screach.PathSplittingSchedFeature (pathSplitFeature) where

import Control.Lens
import Control.Monad.State (MonadIO (liftIO), evalStateT)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.Operations qualified as C
import Screach.Diagnostic (ScreachLogAction)
import Screach.SchedulerFeature qualified as Sched
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL

-- TODO(internal#111): we should decouple this feature from 'Sched.setupNext' so that an arbitrary scheduler can be used

-- | Path splitting feature that splits execution between two branches by pushing two states onto a scheduler queue.
-- One state assumes the branch's true path was taken and the other assumes the predicate was false. The scheduler callback 'Sched.Callback'
-- provided to this feature is used to interact with the scheduler to schedule both of these states for later.
--
-- This function also takes a callback that checks the satisfiability of each path and only schedules paths if they are feasible.
-- 'C.considerSatisfiability' should typically be the callback if this filtering is required. A no-op callback const 'CBO.IndeterminateBranchResult'
-- will disable satisfiability checking and result in both branches being scheduled.
pathSplitFeature ::
  (Sched.HasSchedulerState p p sym ext rtp, CB.IsSymInterface sym) =>
  ScreachLogAction ->
  Sched.SchedulerCallback p sym ext rtp ->
  -- | This parameter is used to evaluate the feasibility of a branch
  -- prior to scheduling it. This parameter should typically be 'C.considerSatisfiability'
  (Maybe WPL.ProgramLoc -> WI.Pred sym -> IO CBO.BranchResult) ->
  C.ExecutionFeature p sym ext rtp
pathSplitFeature sla callback considerSat = C.ExecutionFeature $ \case
  C.SymbolicBranchState pr pf1 pf2 _ st -> do
    let pf1' = C.forgetPostdomFrame pf1
    let pf2' = C.forgetPostdomFrame pf2
    let sym = st ^. C.stateSymInterface
    loc <- WI.getCurrentProgramLoc sym
    pNot <- WI.notPred sym pr
    let
      assumptionOfPred :: WI.Pred sym -> C.PausedFrame p sym ext rtp f -> CB.Assumption sym
      assumptionOfPred p frm = CB.BranchCondition loc (C.pausedLoc frm) p
    evalStateT
      ( do
          callbackIfSat
            loc
            pr
            (callback [assumptionOfPred pr pf1'] pf1')
            (callback [assumptionOfPred pNot pf2'] pf2')
          zoom Sched.schedulerWorklist (Sched.setupNext sla)
      )
      st
  _ -> return C.ExecutionFeatureNoChange
 where
  callbackIfSat loc considerPred eff1 eff2 = do
    r <- liftIO $ considerSat (Just loc) considerPred
    case r of
      CBO.IndeterminateBranchResult -> do
        eff1
        eff2
      CBO.NoBranch True ->
        eff1
      CBO.NoBranch False ->
        eff2
      CBO.UnsatisfiableContext -> pure ()
