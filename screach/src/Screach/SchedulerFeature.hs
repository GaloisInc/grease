{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Screach.SchedulerFeature (
  HasSchedulerState (..),
  scheduleForLaterCallback,
  setupNext,
  schedulerFeature,
  SchedulerCallback,
  PrioritizationFunction,
  DistPriority (..),
  WorkList,
  emptyWorklist,
  getActiveObligations,
  isSchedulerFinished,
  defaultPrioritizationFunction,
  schedulerWorklist,
  SchedulerState (..),
  emptySchedulerState,
  SavedState (..),
  getTargetResults,
  execResultContextLens,
  execStateContextLens,
) where

import Control.Lens
import Control.Lens qualified as Lens
import Control.Monad.Reader (MonadIO (liftIO), runReaderT)
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Data.IntPSQ qualified as PSQ
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.Operations qualified as C
import Lumberjack qualified as LJ
import Screach.Diagnostic (ScreachLogAction)
import Screach.Diagnostic qualified as Diag
import Screach.LocationExecutionFeature (frameLocFromSimState)
import Screach.RefinementOptions qualified as RftOpt
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL

-- | A `PausedWorkItem` represents a suspended symbolic execution path that
--   can later be resumed.  It captures all the relevant context that
--   is required to recreate the simulator state at the point when
--   the path was suspended.
data PausedWorkItem p sym ext rtp
  = forall f args.
  PausedWorkItem
  { _workItemPreds :: [CB.Assumption sym]
  , _workItemFrame :: C.PausedFrame p sym ext rtp f
  -- ^ The paused execution frame
  , _workItemState :: C.SimState p sym ext rtp f ('Just args)
  -- ^ The overall execution state of this path
  , _workItemBackendState :: CB.AssumptionState sym
  -- ^ The state of the symbolic backend when we suspended this work item
  , _workItemLoc :: WPL.ProgramLoc
  }

data SavedState p sym ext rtp
  = SavedState
  { _savedBackendState :: CB.AssumptionState sym
  , _savedExecState :: C.ExecState p sym ext rtp
  }

-- | The lower bound of the distance of a given item from the target.
-- The minimum priority in the queue is popped first.
newtype DistPriority = DistPriority Int deriving (Eq, Show, Num, Ord)

-- | A `WorkList` represents a sequence of `WorkItems` that still
--   need to be explored.
--
--   The counter is used for producing new IDs for items.
data WorkList p sym ext rtp = WorkList
  { _wlCounter :: Int
  , _wlQueue :: PSQ.IntPSQ DistPriority (PausedWorkItem p sym ext rtp)
  }

-- | The current state of the scheduler, this state includes both
-- states that could be resumed, and results that have been reached thusfar.
data SchedulerState p sym ext rtp = SchedulerState
  {_resumeablesWorklist :: WorkList p sym ext rtp, _savedEndStates :: [SavedState p sym ext rtp]}

makeLenses ''SchedulerState

type SchedulerM p sym ext rtp = StateT (WorkList p sym ext rtp) IO

Lens.makeLenses ''WorkList

getActiveObligations ::
  (HasSchedulerState p p sym ext rtp) =>
  C.ExecState p sym ext rtp ->
  IO (CB.ProofObligations sym)
getActiveObligations st =
  let simCtx = C.execStateContext st
   in C.withBackend simCtx $ \bak ->
        CB.getProofObligations bak

schedulerWorklist ::
  HasSchedulerState p r sym ext rtp => Lens.Lens' p (WorkList r sym ext rtp)
schedulerWorklist = schedulerState . resumeablesWorklist

isSchedulerFinished ::
  (HasSchedulerState p p sym ext rtp) =>
  C.ExecState p sym ext rtp ->
  Bool
isSchedulerFinished
  st =
    case st of
      C.ResultState{} ->
        let wl = C.execStateContext st ^. C.cruciblePersonality . schedulerWorklist . wlQueue
         in PSQ.null wl
      _ -> False

emptyWorklist :: WorkList p sym ext rtp
emptyWorklist = WorkList{_wlCounter = 0, _wlQueue = PSQ.empty}

emptySchedulerState :: SchedulerState p sym ext rtp
emptySchedulerState = SchedulerState{_resumeablesWorklist = emptyWorklist, _savedEndStates = []}

defaultMaybeDist :: Maybe DistPriority -> DistPriority
defaultMaybeDist (Just d) = d
defaultMaybeDist Nothing = DistPriority maxBound

queueWorkItem :: Maybe DistPriority -> PausedWorkItem p sym ext rtp -> SchedulerM p sym ext rtp ()
queueWorkItem p it =
  do
    wlCounter += 1
    currCounter <- zoom wlCounter get
    wlQueue %= PSQ.insert currCounter (defaultMaybeDist p) it
    pure ()

-- | Pop work item ensures that the state gets the current queue in it's own state
popWorkItem ::
  (HasSchedulerState p p sym ext rtp) =>
  SchedulerM p sym ext rtp (Maybe (PausedWorkItem p sym ext rtp))
popWorkItem =
  do
    q <- zoom wlQueue get
    let it = (\(_, _, x) -> x) <$> PSQ.findMin q
    let currq = PSQ.deleteMin q
    wlQueue .= currq
    currState <- get
    let nit =
          -- TODO i think we might be able to write a lens that pair these together, probably in a record together
          -- Record update syntax doesnt work for these polymorphic records on <=9.4.8
          ( \PausedWorkItem
               { _workItemPreds = wp
               , _workItemState = st
               , _workItemFrame = frm
               , _workItemBackendState = bakSt
               , _workItemLoc = wLoc
               } ->
                PausedWorkItem
                  { _workItemPreds = wp
                  , _workItemState = Lens.set schedulerWorklist currState st
                  , _workItemFrame = frm
                  , _workItemBackendState = bakSt
                  , _workItemLoc = wLoc
                  }
          )
            <$> it
    pure nit

-- | Given a work item, restore the simulator state so that it is ready to resume
--   exploring the path that it represents.
restoreWorkItem ::
  (CB.IsSymInterface sym, HasSchedulerState p p sym ext rtp) =>
  ScreachLogAction ->
  PausedWorkItem p sym ext rtp ->
  IO (C.ExecState p sym ext rtp)
restoreWorkItem sla (PausedWorkItem preds pf simStat bakState loc) =
  do
    let simCtx = simStat ^. C.stateContext
    let sym = simStat ^. C.stateSymInterface
    C.withBackend
      simCtx
      $ \bak -> do
        LJ.writeLog sla (Diag.ResumingFrame loc)
        WI.setCurrentProgramLoc sym loc
        CB.clearProofObligations bak
        CB.restoreAssumptionState bak bakState
        mapM_ (CB.addAssumption bak) preds
        -- add obligations to the wstate
        let ctx = simStat ^. C.stateTree . C.actContext
        runReaderT (C.resumeFrame pf ctx) simStat

-- | A class for Crucible personality types @p@ which contain a
-- 'WorkList' the current state of the scheduler feature that maintains paused frames.
-- This execution feature is polymorphic over
-- 'HasSchedulerState` so that downstream users can supply their own
-- personality types that extend 'WorkList' further.
class HasSchedulerState p r sym ext rtp | p -> rtp r sym ext where
  schedulerState :: Lens.Lens' p (SchedulerState r sym ext rtp)

instance HasSchedulerState (SchedulerState p sym ext rtp) p sym ext rtp where
  schedulerState = id

instance
  (HasSchedulerState p p sym ext rtp) =>
  HasSchedulerState (C.SimState p sym ext rtp f a) p sym ext rtp
  where
  schedulerState = C.stateContext . C.cruciblePersonality . schedulerState

createWorkItem ::
  (HasSchedulerState p p sym ext rtp) =>
  [CB.Assumption sym] ->
  C.PausedFrame p sym ext rtp f ->
  C.SimState p sym ext rtp f ('Just args) ->
  IO (PausedWorkItem p sym ext rtp)
createWorkItem preds pf simState =
  let simCtx = simState ^. C.stateContext
      sym = simState ^. C.stateSymInterface
   in C.withBackend simCtx $ \bak -> do
        bakState <- CB.getBackendState bak
        loc <- WI.getCurrentProgramLoc sym
        pure $
          PausedWorkItem
            { _workItemPreds = preds
            , _workItemFrame = pf
            , _workItemState = simState
            , _workItemBackendState = bakState
            , _workItemLoc = loc
            }

-- | A prioritization function that given a stopped state evaluates that state to a queue priority where lower numbers are evaluated first.
type PrioritizationFunction p sym ext rtp =
  forall f args.
  C.PausedFrame p sym ext rtp f ->
  C.SimState p sym ext rtp f ('Just args) ->
  StateT (C.SimState p sym ext rtp f ('Just args)) IO (Maybe DistPriority)

-- | A default prioritization function that maps all states to the same priority.
defaultPrioritizationFunction :: PrioritizationFunction p sym ext rtp
defaultPrioritizationFunction _ _ = pure Nothing

-- | This callback allows clients to schedule a SimState for later based on some additional assumptions that will be enqueued
-- the paused frame and SimState is saved along with sub-personality data that will be restored (this allows this feature to play nice with say replay)
-- when items are restored we alway make sure to update the personality to our current "queue" state. this is possibly a good motivation for an IORef.
--
-- We schedule by updating a SimContext with our queued item
type SchedulerCallback p sym ext rtp =
  forall f args.
  [CB.Assumption sym] ->
  C.PausedFrame p sym ext rtp f ->
  StateT (C.SimState p sym ext rtp f ('Just args)) IO ()

scheduleForLaterCallback ::
  (HasSchedulerState p p sym ext rtp) =>
  ScreachLogAction ->
  PrioritizationFunction p sym ext rtp ->
  SchedulerCallback p sym ext rtp
scheduleForLaterCallback sla pfunc preds pf =
  do
    st <- get
    wi <- liftIO $ createWorkItem preds pf st
    dist <- pfunc pf st
    maybe
      (pure ())
      ( \(sFrom, scheduled) ->
          do
            let DistPriority num = defaultMaybeDist dist
            let lg = Diag.ScheduledSuccessor scheduled sFrom num
            LJ.writeLog sla lg
      )
      ((,) <$> frameLocFromSimState st <*> C.pausedLoc pf)
    zoom (schedulerState . resumeablesWorklist) (queueWorkItem dist wi)

setupNext ::
  (CB.IsSymInterface sym, HasSchedulerState p p sym ext rtp) =>
  ScreachLogAction ->
  StateT (WorkList p sym ext rtp) IO (C.ExecutionFeatureResult p sym ext rtp)
setupNext sla = do
  it <- popWorkItem
  case it of
    Nothing -> pure C.ExecutionFeatureNoChange
    Just nxt -> C.ExecutionFeatureNewState <$> liftIO (restoreWorkItem sla nxt)

createSaveItem ::
  (HasSchedulerState p p sym ext rtp) =>
  C.ExecResult p sym ext rtp ->
  IO (SavedState p sym ext rtp)
createSaveItem st =
  let simCtx = C.execResultContext st
   in C.withBackend simCtx $ \bak -> do
        bakState <- CB.getBackendState bak
        pure
          SavedState
            { _savedBackendState = bakState
            , _savedExecState = C.ResultState st
            }

execResultContextLens ::
  Lens' (C.ExecResult p sym ext rtp) (C.SimContext p sym ext)
execResultContextLens =
  lens
    C.execResultContext
    ( \res newContext ->
        case res of
          C.FinishedResult _ pr -> C.FinishedResult newContext pr
          C.AbortedResult _ ares -> C.AbortedResult newContext ares
          C.TimeoutResult st -> C.TimeoutResult $ st & execStateContextLens .~ newContext
    )

-- TODO: Upstream here https://github.com/GaloisInc/crucible/issues/1601
execStateContextLens ::
  forall p sym ext rtp.
  Lens' (C.ExecState p sym ext rtp) (C.SimContext p sym ext)
execStateContextLens =
  lens
    C.execStateContext
    ( \st newContext ->
        let
          updateSimState :: C.SimState p sym ext rtp f args -> C.SimState p sym ext rtp f args
          updateSimState stState = stState & C.stateContext .~ newContext
         in
          case st of
            C.ResultState r -> C.ResultState $ r & execResultContextLens .~ newContext
            C.AbortState rsn simstate -> C.AbortState rsn (updateSimState simstate)
            C.UnwindCallState vfv ar simstate -> C.UnwindCallState vfv ar (updateSimState simstate)
            C.CallState rh rc simstate -> C.CallState rh rc (updateSimState simstate)
            C.TailCallState vfv rc simstate -> C.TailCallState vfv rc (updateSimState simstate)
            C.ReturnState fn vfv regval simstate -> C.ReturnState fn vfv regval (updateSimState simstate)
            C.RunningState rsi simstate -> C.RunningState rsi (updateSimState simstate)
            C.SymbolicBranchState p pf1 pf2 cbt simstate -> C.SymbolicBranchState p pf1 pf2 cbt (updateSimState simstate)
            C.ControlTransferState cr simstate -> C.ControlTransferState cr (updateSimState simstate)
            C.OverrideState ov simstate -> C.OverrideState ov (updateSimState simstate)
            C.BranchMergeState cbt simstate -> C.BranchMergeState cbt (updateSimState simstate)
            C.InitialState _ symglobs ah retty execcont -> C.InitialState newContext symglobs ah retty execcont
    )

resultSchedState ::
  (HasSchedulerState p p sym ext rtp) =>
  Lens' (C.ExecResult p sym ext rtp) (SchedulerState p sym ext rtp)
resultSchedState = execResultContextLens . C.cruciblePersonality . schedulerState

saveResult ::
  (HasSchedulerState p p sym ext rtp) =>
  C.ExecResult p sym ext rtp ->
  IO (C.ExecResult p sym ext rtp)
saveResult st = do
  saved <- createSaveItem st
  let nst = st & resultSchedState . savedEndStates %~ (saved :)
  pure nst

getTargetResults ::
  (HasSchedulerState p p sym ext rtp) => C.ExecResult p sym ext rtp -> [SavedState p sym ext rtp]
getTargetResults res = res ^. resultSchedState . savedEndStates

schedulerFeature ::
  (HasSchedulerState p p sym ext rtp, CB.IsSymInterface sym) =>
  ScreachLogAction ->
  -- | If the scheduler should collect all solutions or just the first
  -- solution
  RftOpt.AllSolutions ->
  -- | Whether a given execution result should be considered a target state.
  (C.ExecResult p sym ext rtp -> IO Bool) ->
  C.ExecutionFeature p sym ext rtp
schedulerFeature sla (RftOpt.AllSolutions exploreMore) isTarget =
  let runNxt =
        evalStateT $ setupNext sla
      continue res =
        runNxt
          (Lens.view (C.cruciblePersonality . schedulerWorklist) $ C.execResultContext res)
   in C.ExecutionFeature $ \case
        C.ResultState r -> do
          t <- isTarget r
          if t
            then do
              newResult <- saveResult r
              if exploreMore
                then
                  continue newResult
                else
                  return $ C.ExecutionFeatureModifiedState $ C.ResultState newResult
            else
              continue r
        _ -> return C.ExecutionFeatureNoChange
