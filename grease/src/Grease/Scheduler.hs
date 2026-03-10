{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
-- Module           : Grease.Scheduler
-- Description      : Scheduler for symbolic execution
--
-- A language-agnostic, 'IORef'-backed scheduler abstraction for Crucible
-- symbolic execution. Provides work queue management, scheduling policies (DFS,
-- BFS, priority-based), and Crucible execution features for path splitting.
module Grease.Scheduler (
  -- * Priority
  Priority (..),

  -- * Work items
  WorkItem,
  workItemLoc,
  createWorkItem,
  restoreWorkItem,

  -- * Work queue
  WorkQueue,
  newWorkQueue,
  enqueueFront,
  enqueueBack,
  enqueuePrio,
  dequeue,
  isEmpty,
  drainAll,

  -- * Scheduling policies
  SchedulingPolicy (..),
  PrioritizationFunction,
  dfsPolicy,
  bfsPolicy,
  priorityPolicy,
  withSatisfiabilityCheck,

  -- * Execution features
  ResultAction (..),
  branchFeature,
  resultFeature,
  schedulerFeatures,

  -- * Utility lenses
  execResultContextLens,
  execStateContextLens,
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import Control.Monad.Reader (runReaderT)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.IntPSQ qualified as PSQ
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.Operations qualified as C
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL

-- | Priority for work items. Lower values are dequeued first.
newtype Priority = Priority Int
  deriving (Eq, Ord, Show)

-- | A suspended symbolic execution path.
data WorkItem p sym ext rtp = forall f args. WorkItem
  { workItemAssumption :: CB.Assumption sym
  -- ^ Branch condition to assert when this path is restored. At a symbolic
  -- branch, the backend state is captured *before* either branch direction is
  -- assumed. This assumption (e.g., that the branch predicate is true or
  -- false) is replayed on top of the restored backend state in
  -- 'restoreWorkItem' to place execution on the correct branch.
  , workItemLoc :: WPL.ProgramLoc
  , workItemFrame :: C.PausedFrame p sym ext rtp f
  , workItemState :: C.SimState p sym ext rtp f ('Just args)
  , workItemBackendState :: CB.AssumptionState sym
  }

-- | Create a 'WorkItem' from the current execution state at a branch.
createWorkItem ::
  CB.IsSymInterface sym =>
  CB.Assumption sym ->
  C.PausedFrame p sym ext rtp f ->
  C.SimState p sym ext rtp f ('Just args) ->
  IO (WorkItem p sym ext rtp)
createWorkItem assumption pf simState =
  let simCtx = simState ^. C.stateContext
      sym = simState ^. C.stateSymInterface
   in C.withBackend simCtx $ \bak -> do
        bakState <- CB.getBackendState bak
        loc <- WI.getCurrentProgramLoc sym
        pure
          WorkItem
            { workItemAssumption = assumption
            , workItemLoc = loc
            , workItemFrame = pf
            , workItemState = simState
            , workItemBackendState = bakState
            }

-- | Restore a 'WorkItem' to a runnable 'C.ExecState'.
restoreWorkItem ::
  CB.IsSymInterface sym =>
  WorkItem p sym ext rtp ->
  IO (C.ExecState p sym ext rtp)
restoreWorkItem (WorkItem assumption loc frm st assumes) = do
  let sym = st ^. C.stateSymInterface
  let simCtx = st ^. C.stateContext
  C.withBackend simCtx $ \bak -> do
    WI.setCurrentProgramLoc sym loc
    CB.restoreAssumptionState bak assumes
    CB.addAssumption bak assumption
    let ctx = st ^. C.stateTree . C.actContext
    runReaderT (C.resumeFrame frm ctx) st

------------------------------------------------------------------------
-- Work Queue
------------------------------------------------------------------------

-- | An opaque, IORef-backed priority queue of 'WorkItem's.
--
-- Using an IORef means the queue is a shared mutable reference. When a
-- 'WorkItem' captures a 'SimState' at a branch point, that state includes a
-- snapshot of the personality. With a personality-embedded worklist, the
-- snapshot would be stale when later restored. With an IORef, restored work
-- items automatically see the latest queue state.
newtype WorkQueue p sym ext rtp = WorkQueue
  {_wqRef :: IORef (WorkQueueState p sym ext rtp)}

-- | Internal mutable state of a 'WorkQueue'.
data WorkQueueState p sym ext rtp = WorkQueueState
  { wqsFrontCounter :: !Int
  -- ^ Decreasing counter for 'enqueueFront' (DFS ordering).
  , wqsBackCounter :: !Int
  -- ^ Increasing counter for 'enqueueBack' (BFS ordering) and 'enqueuePrio'.
  , wqsQueue :: !(PSQ.IntPSQ Priority (WorkItem p sym ext rtp))
  -- ^ Priority search queue of work items.
  }

-- | Create a new, empty 'WorkQueue'.
newWorkQueue :: IO (WorkQueue p sym ext rtp)
newWorkQueue = WorkQueue <$> IORef.newIORef (WorkQueueState 0 0 PSQ.empty)

-- | Atomically modify the work queue state.
modifyQueue ::
  WorkQueue p sym ext rtp ->
  (WorkQueueState p sym ext rtp -> WorkQueueState p sym ext rtp) ->
  IO ()
modifyQueue (WorkQueue ref) f = IORef.atomicModifyIORef' ref (\wqs -> (f wqs, ()))

-- | Enqueue a work item at the front (lowest priority key).
-- Used for DFS: new items get explored before older items.
enqueueFront :: WorkItem p sym ext rtp -> WorkQueue p sym ext rtp -> IO ()
enqueueFront wi wq =
  modifyQueue wq $ \wqs ->
    let fc' = wqsFrontCounter wqs - 1
     in wqs
          { wqsFrontCounter = fc'
          , wqsQueue = PSQ.insert fc' (Priority fc') wi (wqsQueue wqs)
          }

-- | Enqueue a work item at the back (highest priority key).
-- Used for BFS: new items get explored after older items.
enqueueBack :: WorkItem p sym ext rtp -> WorkQueue p sym ext rtp -> IO ()
enqueueBack wi wq =
  modifyQueue wq $ \wqs ->
    let bc' = wqsBackCounter wqs + 1
     in wqs
          { wqsBackCounter = bc'
          , wqsQueue = PSQ.insert bc' (Priority bc') wi (wqsQueue wqs)
          }

-- | Enqueue a work item with a caller-supplied priority.
-- Lower priority values are dequeued first. Within the same priority level,
-- newer items are dequeued first (DFS-like tiebreaking), since 'IntPSQ'
-- breaks ties by minimum key and we use a decreasing counter.
enqueuePrio :: Priority -> WorkItem p sym ext rtp -> WorkQueue p sym ext rtp -> IO ()
enqueuePrio prio wi wq =
  modifyQueue wq $ \wqs ->
    let fc' = wqsFrontCounter wqs - 1
     in wqs
          { wqsFrontCounter = fc'
          , wqsQueue = PSQ.insert fc' prio wi (wqsQueue wqs)
          }

-- | Dequeue the minimum-priority work item, if any.
dequeue :: WorkQueue p sym ext rtp -> IO (Maybe (WorkItem p sym ext rtp))
dequeue (WorkQueue ref) =
  IORef.atomicModifyIORef' ref $ \wqs ->
    case PSQ.minView (wqsQueue wqs) of
      Nothing -> (wqs, Nothing)
      Just (_, _, wi, q') -> (wqs{wqsQueue = q'}, Just wi)

-- | Check whether the work queue is empty.
isEmpty :: WorkQueue p sym ext rtp -> IO Bool
isEmpty (WorkQueue ref) = do
  wqs <- IORef.readIORef ref
  pure (PSQ.null (wqsQueue wqs))

-- | Drain all remaining work items from the queue.
drainAll :: WorkQueue p sym ext rtp -> IO (Seq (WorkItem p sym ext rtp))
drainAll wq = go Seq.empty
 where
  go acc = do
    mwi <- dequeue wq
    case mwi of
      Nothing -> pure acc
      Just wi -> go (acc Seq.|> wi)

------------------------------------------------------------------------
-- Scheduling Policies
------------------------------------------------------------------------

-- | A scheduling policy determines how to handle branches and (optionally)
-- how to handle results. It is a record of functions, not a typeclass,
-- to allow easy composition via combinators like 'withSatisfiabilityCheck'.
data SchedulingPolicy p sym ext rtp = SchedulingPolicy
  { policyName :: String
  , policyOnBranch ::
      forall f args postdom_args.
      WorkQueue p sym ext rtp ->
      WI.Pred sym ->
      C.PausedFrame p sym ext rtp f ->
      C.PausedFrame p sym ext rtp f ->
      C.CrucibleBranchTarget f postdom_args ->
      C.SimState p sym ext rtp f ('Just args) ->
      IO (C.ExecutionFeatureResult p sym ext rtp)
  }

-- | Assigns priority to a suspended path. Lower = explored sooner.
-- Returns 'Nothing' to use a default priority.
type PrioritizationFunction p sym ext rtp =
  forall f args.
  C.PausedFrame p sym ext rtp f ->
  C.SimState p sym ext rtp f ('Just args) ->
  IO (Maybe Priority)

-- | Common branch setup: compute negation, location, forget postdom frames,
-- and build assumptions.
data BranchSetup p sym ext rtp = forall f args. BranchSetup
  { bsLoc :: WPL.ProgramLoc
  , bsTrueFrame :: C.PausedFrame p sym ext rtp f
  , bsFalseFrame :: C.PausedFrame p sym ext rtp f
  , bsTrueAssumption :: CB.Assumption sym
  , bsFalseAssumption :: CB.Assumption sym
  , bsSimState :: C.SimState p sym ext rtp f ('Just args)
  }

prepareBranch ::
  CB.IsSymBackend sym bak =>
  bak ->
  WI.Pred sym ->
  C.PausedFrame p sym ext rtp f ->
  C.PausedFrame p sym ext rtp f ->
  C.SimState p sym ext rtp f ('Just args) ->
  IO (BranchSetup p sym ext rtp)
prepareBranch _bak p trueFrame falseFrame st = do
  let sym = st ^. C.stateSymInterface
  pnot <- WI.notPred sym p
  loc <- WI.getCurrentProgramLoc sym
  let pf1' = C.forgetPostdomFrame trueFrame
  let pf2' = C.forgetPostdomFrame falseFrame
  pure
    BranchSetup
      { bsLoc = loc
      , bsTrueFrame = pf1'
      , bsFalseFrame = pf2'
      , bsTrueAssumption = CB.BranchCondition loc (C.pausedLoc pf1') p
      , bsFalseAssumption = CB.BranchCondition loc (C.pausedLoc pf2') pnot
      , bsSimState = st
      }

-- | Enqueue both branches, then dequeue the next item to continue.
enqueueBothAndDequeue ::
  CB.IsSymInterface sym =>
  String ->
  (WorkItem p sym ext rtp -> WorkQueue p sym ext rtp -> IO ()) ->
  (WorkItem p sym ext rtp -> WorkQueue p sym ext rtp -> IO ()) ->
  WorkQueue p sym ext rtp ->
  BranchSetup p sym ext rtp ->
  IO (C.ExecutionFeatureResult p sym ext rtp)
enqueueBothAndDequeue name enqTrue enqFalse wq (BranchSetup{..}) = do
  truWi <- createWorkItem bsTrueAssumption bsTrueFrame bsSimState
  falWi <- createWorkItem bsFalseAssumption bsFalseFrame bsSimState
  enqTrue truWi wq
  enqFalse falWi wq
  mnext <- dequeue wq
  case mnext of
    Nothing ->
      panic name ["Empty queue after enqueueing two items"]
    Just next ->
      C.ExecutionFeatureNewState <$> restoreWorkItem next

-- | Path splitting with LIFO ordering (depth-first).
--
-- At a branch, saves the false branch to the front of the queue and
-- continues with the true branch. This matches Crucible's
-- @pathSplittingFeature@ behavior.
dfsPolicy ::
  CB.IsSymInterface sym =>
  SchedulingPolicy p sym ext rtp
dfsPolicy =
  SchedulingPolicy
    { policyName = "DFS"
    , policyOnBranch = \wq p trueFrame falseFrame _bt st ->
        C.withBackend (st ^. C.stateContext) $ \bak -> do
          BranchSetup{..} <- prepareBranch bak p trueFrame falseFrame st
          wi <- createWorkItem bsFalseAssumption bsFalseFrame bsSimState
          enqueueFront wi wq
          CB.addAssumption bak bsTrueAssumption
          let ctx = bsSimState ^. C.stateTree . C.actContext
          C.ExecutionFeatureNewState <$> runReaderT (C.resumeFrame bsTrueFrame ctx) bsSimState
    }

-- | Path splitting with FIFO ordering (breadth-first).
--
-- At a branch, saves both branches to the back of the queue, then
-- dequeues the oldest item to continue.
bfsPolicy ::
  CB.IsSymInterface sym =>
  SchedulingPolicy p sym ext rtp
bfsPolicy =
  SchedulingPolicy
    { policyName = "BFS"
    , policyOnBranch = \wq p trueFrame falseFrame _bt st ->
        C.withBackend (st ^. C.stateContext) $ \bak -> do
          bs <- prepareBranch bak p trueFrame falseFrame st
          enqueueBothAndDequeue "bfsPolicy" enqueueBack enqueueBack wq bs
    }

-- | Path splitting with custom priority assignment.
--
-- At a branch, calls the 'PrioritizationFunction' on each suspended frame
-- to get priorities, enqueues both, then dequeues the minimum-priority item
-- to continue.
priorityPolicy ::
  CB.IsSymInterface sym =>
  PrioritizationFunction p sym ext rtp ->
  SchedulingPolicy p sym ext rtp
priorityPolicy prioFn =
  SchedulingPolicy
    { policyName = "Priority"
    , policyOnBranch = \wq p trueFrame falseFrame _bt st ->
        C.withBackend (st ^. C.stateContext) $ \bak -> do
          bs@BranchSetup{..} <- prepareBranch bak p trueFrame falseFrame st
          truPrio <- prioFn bsTrueFrame bsSimState
          falPrio <- prioFn bsFalseFrame bsSimState
          enqueueBothAndDequeue
            "priorityPolicy"
            (enqueuePrio (defaultPriority truPrio))
            (enqueuePrio (defaultPriority falPrio))
            wq
            bs
    }

-- | Default priority for 'Nothing': use maxBound so items without priority
-- are explored last.
defaultPriority :: Maybe Priority -> Priority
defaultPriority (Just p) = p
defaultPriority Nothing = Priority maxBound

-- | Wraps a scheduling policy to check branch feasibility before
-- enqueueing. Modifies 'policyOnBranch' to call the satisfiability
-- checker and skip infeasible branches.
withSatisfiabilityCheck ::
  CB.IsSymInterface sym =>
  (Maybe WPL.ProgramLoc -> WI.Pred sym -> IO CBO.BranchResult) ->
  SchedulingPolicy p sym ext rtp ->
  SchedulingPolicy p sym ext rtp
withSatisfiabilityCheck considerSat policy =
  policy
    { policyOnBranch = \wq p trueFrame falseFrame bt st -> do
        let sym = st ^. C.stateSymInterface
        loc <- WI.getCurrentProgramLoc sym
        result <- considerSat (Just loc) p
        case result of
          CBO.NoBranch b ->
            C.withBackend (st ^. C.stateContext) $ \bak -> do
              let frm = if b then trueFrame else falseFrame
              let pf = C.forgetPostdomFrame frm
              p' <- if b then pure p else WI.notPred sym p
              CB.addAssumption bak (CB.BranchCondition loc (C.pausedLoc pf) p')
              let ctx = st ^. C.stateTree . C.actContext
              C.ExecutionFeatureNewState <$> runReaderT (C.resumeFrame pf ctx) st
          CBO.UnsatisfiableContext ->
            -- Neither branch feasible: let execution continue normally
            -- (this will likely result in an abort)
            pure C.ExecutionFeatureNoChange
          CBO.IndeterminateBranchResult ->
            -- Both branches may be feasible: delegate to the underlying policy
            policyOnBranch policy wq p trueFrame falseFrame bt st
    }

------------------------------------------------------------------------
-- Execution Features
------------------------------------------------------------------------

-- | Feature that intercepts 'C.SymbolicBranchState' and delegates to the
-- policy's 'policyOnBranch' handler.
branchFeature ::
  SchedulingPolicy p sym ext rtp ->
  WorkQueue p sym ext rtp ->
  C.ExecutionFeature p sym ext rtp
branchFeature policy wq = C.ExecutionFeature $ \case
  C.SymbolicBranchState p trueFrame falseFrame bt st ->
    policyOnBranch policy wq p trueFrame falseFrame bt st
  _ -> pure C.ExecutionFeatureNoChange

-- | The action to take after the result callback processes an 'C.ExecResult'.
data ResultAction
  = -- | Dequeue the next work item and continue exploring.
    ContinueExploring
  | -- | Stop execution. Returns 'C.ExecutionFeatureNoChange' so that
    -- 'C.executeCrucible' returns the current result to the caller.
    StopExploring
  deriving (Eq, Show)

-- | Feature that intercepts 'C.ResultState' and dequeues the next path.
--
-- Calls the result callback on each completed path. The callback
-- returns a 'ResultAction' indicating whether to continue exploring
-- or stop. When the queue is empty and the action is 'ContinueExploring',
-- returns 'ExecutionFeatureNoChange', causing 'C.executeCrucible' to
-- return the final 'C.ExecResult' to the caller.
--
-- Before restoring each work item, clears the current backend's proof
-- obligations to prevent contamination between paths. Since
-- restoreAssumptionState merges obligations via gcRestore, clearing
-- ensures only the saved obligations are present.
resultFeature ::
  CB.IsSymBackend sym bak =>
  bak ->
  WorkQueue p sym ext rtp ->
  (C.ExecResult p sym ext rtp -> IO ResultAction) ->
  C.ExecutionFeature p sym ext rtp
resultFeature bak wq callback = C.ExecutionFeature $ \case
  C.ResultState r -> do
    action <- callback r
    case action of
      StopExploring -> pure C.ExecutionFeatureNoChange
      ContinueExploring -> do
        mnext <- dequeue wq
        case mnext of
          Nothing -> pure C.ExecutionFeatureNoChange
          Just next -> do
            CB.clearProofObligations bak
            st <- restoreWorkItem next
            pure (C.ExecutionFeatureNewState st)
  _ -> pure C.ExecutionFeatureNoChange

-- | Convenience function: create both execution features and the work queue.
--
-- Returns the branch feature, result feature, and the work queue (for
-- inspection or draining remaining items after execution).
schedulerFeatures ::
  CB.IsSymBackend sym bak =>
  bak ->
  SchedulingPolicy p sym ext rtp ->
  (C.ExecResult p sym ext rtp -> IO ResultAction) ->
  IO
    ( C.ExecutionFeature p sym ext rtp
    , C.ExecutionFeature p sym ext rtp
    , WorkQueue p sym ext rtp
    )
schedulerFeatures bak policy callback = do
  wq <- newWorkQueue
  let bf = branchFeature policy wq
  let rf = resultFeature bak wq callback
  pure (bf, rf, wq)

------------------------------------------------------------------------
-- Utility lenses
------------------------------------------------------------------------

-- | A lens into the 'C.SimContext' of an 'C.ExecResult'.
--
-- TODO: Upstream to Crucible: https://github.com/GaloisInc/crucible/issues/1601
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

-- | A lens into the 'C.SimContext' of an 'C.ExecState'.
--
-- TODO: Upstream to Crucible: https://github.com/GaloisInc/crucible/issues/1601
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
