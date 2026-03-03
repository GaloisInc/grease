-- This is based on "Lang.Crucible.Simulator.PathSplitting"
--
-- TODO(internal#18): Upstream improved (B|D)FS execution feature to Crucible
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

module Screach.Explore (
  ExplorationState,
  HasExplorationState,
  Strategy (..),
  emptyExplorationState,
  exploreFeature,
  explorationState,
  finishedExploring,
  getObligations,
) where

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.Reader (runReaderT)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.Operations qualified as C
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL

-- | See 'exploreFeature'
data Strategy = BreadthFirst | DepthFirst
  deriving (Read)

-- | A `PausedWorkItem` represents a suspended symbolic execution path that
--   can later be resumed.  It captures all the relevant context that
--   is required to recreate the simulator state at the point when
--   the path was suspended.
data WorkItem p sym ext rtp
  = forall f args.
  PausedWorkItem
  { workItemPred :: WI.Pred sym
  -- ^ The predicate we branched on to generate this work item
  , workItemLoc :: WPL.ProgramLoc
  -- ^ The location of the symbolic branch
  , workItemBranch :: Bool
  -- ^ Which branch of the symbolic branch was followed
  , workItemFrame :: C.PausedFrame p sym ext rtp f
  -- ^ The paused execution frame
  , workItemState :: CS.SimState p sym ext rtp f ('Just args)
  -- ^ The overall execution state of this path
  , workItemAssumes :: CB.AssumptionState sym
  -- ^ The assumption state of the symbolic backend when we suspended this work item
  , workItemObligations :: CB.ProofObligations sym
  -- ^ The proof obligations of the symbolic backend when we suspended this work item
  }

-- | A `WorkList` represents a sequence of `WorkItems` that still
--   need to be explored.
type WorkList p sym ext rtp = IORef (Seq (WorkItem p sym ext rtp))

-- | Put a work item onto the front of the work list.
queueWorkItem ::
  WorkItem p sym ext rtp ->
  WorkList p sym ext rtp ->
  IO ()
queueWorkItem i wl = atomicModifyIORef' wl (\xs -> (i Seq.<| xs, ()))

-- | Put a work item onto the back of the work list.
queueBackWorkItem ::
  WorkItem p sym ext rtp ->
  WorkList p sym ext rtp ->
  IO ()
queueBackWorkItem i wl = atomicModifyIORef' wl (\xs -> (xs Seq.|> i, ()))

-- | Pull a work item off the front of the work list, if there are any left.
dequeueWorkItem ::
  WorkList p sym ext rtp ->
  IO (Maybe (WorkItem p sym ext rtp))
dequeueWorkItem wl =
  atomicModifyIORef' wl $ \xs ->
    case Seq.viewl xs of
      Seq.EmptyL -> (xs, Nothing)
      i Seq.:< xs' -> (xs', Just i)

-- | Given a work item, restore the simulator state so that it is ready to resume
--   exploring the path that it represents.
restoreWorkItem ::
  (CB.IsSymInterface sym) =>
  WorkItem p sym ext rtp ->
  IORef (CB.ProofObligations sym) ->
  IO (C.ExecState p sym ext rtp)
restoreWorkItem (PausedWorkItem branchPred loc _branch frm st assumes proofObs) obs =
  do
    let sym = st ^. C.stateSymInterface
    let simCtx = st ^. C.stateContext
    C.withBackend simCtx $ \bak ->
      do
        WI.setCurrentProgramLoc sym loc
        CB.clearProofObligations bak
        CB.restoreAssumptionState bak assumes
        writeIORef obs proofObs
        mapM_ (CB.addAssumption bak . CB.BranchCondition loc (C.pausedLoc frm)) (Just branchPred)
        let ctx = st ^. C.stateTree . C.actContext
        runReaderT (C.resumeFrame frm ctx) st

data ExplorationState p sym ext rtp = ExplorationState
  { _worklist :: WorkList p sym ext rtp
  , _obligations :: IORef (CB.ProofObligations sym)
  , _finalized :: IORef Bool
  }

Lens.makeLenses ''ExplorationState

-- | A class for Crucible personality types @p@ which contain a
-- 'ExplorationState'. This execution feature is polymorphic over
-- 'HasExplorationState` so that downstream users can supply their own
-- personality types that extend 'ExplorationState' further.
class HasExplorationState p r sym ext rtp | p -> r sym ext rtp where
  explorationState :: Lens.Lens' p (ExplorationState r sym ext rtp)

instance HasExplorationState (ExplorationState p sym ext rtp) p sym ext rtp where
  explorationState = id

instance
  (HasExplorationState p p sym ext rtp) =>
  HasExplorationState (C.SimState p sym ext rtp f a) p sym ext rtp
  where
  explorationState = C.stateContext . C.cruciblePersonality . explorationState

emptyExplorationState :: IO (ExplorationState p sym ext rtp)
emptyExplorationState = do
  wl <- newIORef Seq.empty
  po <- newIORef Nothing
  f <- newIORef False
  pure $ ExplorationState{_worklist = wl, _obligations = po, _finalized = f}

-- Helper, not exported
--
-- TODO: it is likely that old and new will often share a similar assumption
-- history. This function ought to try and attempt to restore that sharing
-- instead of simply returning a conjunction.
mergeProofObligations ::
  sym -> CB.ProofObligations sym -> CB.ProofObligations sym -> CB.ProofObligations sym
mergeProofObligations _ old new =
  case (old, new) of
    (Just oldGoals, Just newGoals) -> Just $ CB.ProveConj oldGoals newGoals
    (Just _, Nothing) -> old
    (Nothing, Just _) -> new
    (Nothing, Nothing) -> Nothing

-- TODO: consider modifying crucible upstream to support updating the backend proof obligations directly.

-- | Retrieve the proof obligations associated with the path that computed this 'ExecResult'.
getObligations ::
  (HasExplorationState p p sym ext rtp) =>
  C.ExecResult p sym ext rtp ->
  IO (CB.ProofObligations sym)
getObligations result =
  readIORef $ C.execResultContext result ^. C.cruciblePersonality . explorationState . obligations

-- Helper, not exported
getMergedProofObligations ::
  HasExplorationState p r sym ext rtp =>
  C.SimContext p sym ext ->
  IO (CB.ProofObligations sym)
getMergedProofObligations simCtx =
  C.withBackend simCtx $ \bak -> do
    let obsRef = simCtx ^. C.cruciblePersonality . explorationState . obligations
    oldObligations <- readIORef obsRef
    curObligations <- CB.getProofObligations bak
    let sym = CB.backendGetSym bak
    pure (mergeProofObligations sym oldObligations curObligations)

-- Helper, not exported
finalizeResult ::
  (HasExplorationState p r sym ext rtp) =>
  C.ExecResult p sym ext rtp ->
  IO ()
finalizeResult result =
  let ctx = C.execResultContext result
   in C.withBackend ctx $ \bak ->
        do
          let os = ctx ^. C.cruciblePersonality . explorationState . obligations
              f = ctx ^. C.cruciblePersonality . explorationState . finalized
          newObligations <- getMergedProofObligations ctx
          CB.clearProofObligations bak
          writeIORef os newObligations
          writeIORef f True

-- | Like 'Lang.Crucible.Simulator.PathSplitting', executing a Crucible CFG
--   with the explore execution feature will simulate paths through the CFG
--   independently, without symbolic branches or path merging. Unexplored
--   paths are suspended in a work list. Invoking 'executeCrucible' with
--   this execution feature will return a result for a single execution path.
--   Additional execution paths can be explored by invoking 'executeCrucible'
--   passing the 'ExecState' (of variant 'ResultState') returned by the previous
--   invocation as the new initial state. The order in which paths are explored
--   is controlled by the 'Strategy' argument. If 'DepthFirst' is selected, a
--   single path will be executed to completion before exploring any symbolic
--   branches. If 'BreadthFirst' is selected, encountering a symbolic branch
--   will suspend both branches of execution and resume the oldest state in
--   the worklist and 'executeCrucible' will return paths ordered by increasing
--   numbers of symbolic branches.
--
--   When using 'exploreFeature' clients should explicitly retrieve proof
--   obligations by calling 'getObligations' on the result, as the exploration
--   feature manages proof obligations on a per-path basis instead of
--   accumulating obligations in the backend imperatively.
--
--   Care should be taken when composing 'exploreFeature' with other execution
--   features that could cause a 'ResultState' to be visited multiple times.
--   Such an execution feature should be executed before 'exploreFeature' to
--   avoid skipping solutions.
exploreFeature ::
  ( CB.IsSymInterface sym
  , HasExplorationState p p sym ext rtp
  ) =>
  Strategy ->
  C.ExecutionFeature p sym ext rtp
exploreFeature strategy = C.ExecutionFeature $ \case
  C.ResultState result -> do
    let es = C.execResultContext result ^. C.cruciblePersonality . explorationState
    let wl = es ^. worklist
    let os = es ^. obligations
    final <- readIORef $ es ^. finalized
    if final
      then case result of
        C.TimeoutResult _ ->
          pure C.ExecutionFeatureNoChange
        _ ->
          dequeueWorkItem wl >>= \case
            Nothing ->
              pure C.ExecutionFeatureNoChange
            Just wi -> do
              writeIORef (es ^. finalized) False
              C.ExecutionFeatureNewState <$> restoreWorkItem wi os
      else do
        finalizeResult result
        pure C.ExecutionFeatureNoChange
  C.SymbolicBranchState p trueFrame falseFrame _bt st -> do
    let simCtx = st ^. C.stateContext
    C.withBackend simCtx $ \bak ->
      do
        let es = simCtx ^. C.cruciblePersonality . explorationState
            wl = es ^. worklist
            os = es ^. obligations
            sym = st ^. C.stateSymInterface
        assumes <- CB.saveAssumptionState bak
        newObligations <- getMergedProofObligations simCtx
        loc <- WI.getCurrentProgramLoc sym
        pNot <- WI.notPred sym p

        let falseWi =
              PausedWorkItem
                { workItemPred = pNot
                , workItemLoc = loc
                , workItemBranch = False
                , workItemFrame = C.forgetPostdomFrame falseFrame
                , workItemState = st
                , workItemAssumes = assumes
                , workItemObligations = newObligations
                }

        case strategy of
          BreadthFirst -> do
            let trueWi =
                  PausedWorkItem
                    { workItemPred = p
                    , workItemLoc = loc
                    , workItemBranch = True
                    , workItemFrame = C.forgetPostdomFrame trueFrame
                    , workItemState = st
                    , workItemAssumes = assumes
                    , workItemObligations = newObligations
                    }
            queueBackWorkItem trueWi wl
            queueBackWorkItem falseWi wl
            Just next <- dequeueWorkItem wl
            C.ExecutionFeatureNewState <$> restoreWorkItem next os
          DepthFirst -> do
            queueWorkItem falseWi wl
            CB.addAssumption bak (CB.BranchCondition loc (C.pausedLoc trueFrame) p)
            let ctx = st ^. C.stateTree . C.actContext
            st' <- runReaderT (C.resumeFrame (C.forgetPostdomFrame trueFrame) ctx) st
            pure $ C.ExecutionFeatureNewState st'
  _ -> return C.ExecutionFeatureNoChange

-- | Returns 'True' if a call to 'executeCrucible' could generate additional
--   results, and 'False' otherwise.
finishedExploring ::
  (HasExplorationState p p sym ext rtp) =>
  C.ExecState p sym ext rtp ->
  IO Bool
finishedExploring st =
  case st of
    C.ResultState{} -> do
      let wl = C.execStateContext st ^. C.cruciblePersonality . explorationState . worklist
      queue <- readIORef wl
      return $ null queue
    _ -> pure False
