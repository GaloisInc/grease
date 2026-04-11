{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Screach.RefineFeature (
  refineFeature,
  RefineResult (..),
  HasRefineResult (..),
  HasScreachPersonality,
  sdseExecFeatures,
) where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Proxy (Proxy))
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Kind (Type)
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.Ctx (Ctx)
import Grease.Bug qualified as GR
import Grease.Concretize qualified as GR
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (GreaseLogAction)
import Grease.Diagnostic qualified as GrDiag
import Grease.Personality qualified as GP
import Grease.Refine qualified as GR
import Grease.Refine.Diagnostic qualified as GRDiag
import Grease.Refine.RefinementData qualified as GR
import Grease.Scheduler qualified as Sched
import Grease.Shape (ArgShapes, ExtShape)
import Grease.Shape.NoTag qualified as Shape
import Grease.Shape.Pointer (PtrShape)
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.CFG.Expr qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types qualified as CT
import Lumberjack qualified as LJ
import Screach.Diagnostic (ScreachLogAction)
import Screach.Diagnostic qualified as Diag
import Screach.Panic qualified as Scrch
import Screach.RefinementOptions qualified as RftOpt
import Screach.Run.Diagnostic qualified as RDiag
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

-- | The result of refinement.
--
-- Type parameters:
--
-- - @sym@: instance of 'Lang.Crucible.Backend.IsSymInterface'
-- - @ext@: language extension, see "Lang.Crucible.CFG.Extension"
-- - @aty@: Crucible argument types for the target function
type RefineResult :: Type -> Type -> Ctx CT.CrucibleType -> Type
data RefineResult sym ext aty
  = RefineResult
  { refineResultBug :: GR.BugInstance
  , refineResultConcData :: GR.ConcretizedData sym ext aty
  }

-- | Class for personality types that hold a 'RefineResult'.
class HasRefineResult p sym ext tys | p -> sym ext tys where
  refineResult :: Lens.Lens' p (Maybe (RefineResult sym ext tys))

-- | Constraints for collaboration between the scheduler,
-- refinement, and record/replay so that refinement can add
-- saved states to a new state and setup replay on that new initial
-- state.
type HasScreachPersonality p sym bak t cExt ext tys ret rtp w =
  ( CS.RegEntry sym ret ~ rtp
  , GP.HasPersonality p sym bak t cExt ext ret tys w
  , HasRefineResult p sym ext tys
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasMemVar p
  )

-- | Take a fresh initial state (from a given refinement) and set it up for replay.
--
-- Returning this via 'C.ExecutionFeatureNewState' is not ideal because the
-- simulator will immediately begin exploring this state, without consulting
-- the worklist. But the refined state may be further than some other state on
-- the worklist from the target. Once we hit a branch we will get to visit the
-- scheduler again but until that point we are forced to run the refinement.
-- Ideally we would take the refinement and add it to the worklist. Then let
-- the scheduler pick the best state. This is not possible because we cannot
-- pause this state until all other features have "observed" the initial state.
-- The worklist can only hold running states but many features need to see the
-- non-running state first so we are forced to let this initial state bubble up.
-- There is some potential hack we could do where we could mark an initial state
-- that should be paused right after all features see it or something but it
-- would be really ugly.
--
-- Overall while this situation isn't ideally it is not super problematic because straightline symbolic execution on the refined state until some
-- split is relatively cheap.
pauseLinearState ::
  (CR.HasReplayState p p sym ext (CS.RegEntry sym ret)) =>
  sym ->
  -- | The new init state
  C.ExecState p sym ext rtp ->
  -- | The trace to replay when starting the new state
  CR.RecordedTrace sym ->
  -- | Whether to replay the trace after refinement
  RftOpt.RefineReplay ->
  IO (C.ExecState p sym ext rtp)
pauseLinearState sym st trc (RftOpt.RefineReplay shouldReplay) =
  case st of
    C.InitialState ctx glb ah tyRepr cont -> do
      empTrc <- CR.emptyRecordedTrace sym
      let toUseTrace = if shouldReplay then trc else empTrc
      let stWithTrace = ctx & C.cruciblePersonality . CR.replayState . CR.initialTrace .~ toUseTrace
      pure $ C.InitialState stWithTrace glb ah tyRepr cont
    -- TODO instead of panicking we can just make the callback return a newtype of an init state
    _ -> Scrch.panic "pauseLinearState" ["Expecting an initial state"]

doLog :: MonadIO m => ScreachLogAction -> RDiag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (Diag.RunDiagnostic diag)

-- | Given a result from symbolic execution use GREASE to
-- attempt to prove the obligations collected by the given state.
-- Return a feature result that
-- either queues a refinement, a state with bug information for a found bug,
-- or continues if the state is proven safe.
refineState ::
  forall solver sym bak t st fm ext w p rtp tys ret cExt.
  ( ?memOpts :: CLM.MemOptions
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , ExtShape ext ~ PtrShape ext w
  , C.IsSyntaxExtension ext
  , GP.HasPersonality p sym bak t cExt ext ret tys w
  , HasRefineResult p sym ext tys
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , CB.IsSymInterface sym
  , ToConc.HasToConcretize p
  , GP.HasMemVar p
  , 16 CT.<= w
  , CLM.HasPtrWidth w
  , CS.RegEntry sym ret ~ rtp
  , MC.MemWidth w
  ) =>
  bak ->
  ScreachLogAction ->
  GreaseLogAction ->
  RftOpt.RefineReplay ->
  C.ExecResult p sym ext rtp ->
  -- | A function that initializes a new crucible state from the argument specification.
  (ArgShapes ext Shape.NoTag tys -> IO (C.ExecState p sym ext (CS.RegEntry sym ret))) ->
  IO
    (C.ExecutionFeatureResult p sym ext rtp)
refineState bak sla gla refineReplay st config = do
  let pers = C.execResultContext st ^. C.cruciblePersonality
  let memVar = GP.getMemVar pers
  LJ.writeLog gla (GrDiag.RefineDiagnostic (GRDiag.ExecutionResult memVar st))
  -- Check if execution aborted before trying to prove obligations.
  -- Without this, aborted paths (e.g., unsupported relocations) are
  -- silently treated as "success" because there are no proof obligations.
  case GR.processExecResult st of
    Just cantRefine -> do
      doLog sla (RDiag.RefinementPathAborted (GR.shortResult (GR.ProveCantRefine cantRefine)))
      pure C.ExecutionFeatureNoChange
    Nothing -> do
      obls <- C.withBackend (C.execResultContext st) $ \bak' ->
        CB.getProofObligations bak'
      refResult <- GR.proveAndRefine bak st gla obls
      case refResult of
        GR.ProveSuccess -> do
          doLog sla RDiag.RefinementSuccess
          pure C.ExecutionFeatureNoChange
        GR.ProveCantRefine _ -> do
          doLog sla (RDiag.RefinementPathAborted (GR.shortResult refResult))
          pure C.ExecutionFeatureNoChange
        GR.ProveNoHeuristic{} -> do
          doLog sla RDiag.RefinementNoHeuristic
          pure C.ExecutionFeatureNoChange
        GR.ProveBug bi cdata ->
          let
            rresultL :: Lens.Lens' (C.ExecResult p sym ext rtp) (Maybe (RefineResult sym ext tys))
            rresultL = Sched.execResultContextLens . C.cruciblePersonality . refineResult
            nres = st & rresultL ?~ RefineResult bi cdata
           in
            pure $
              C.ExecutionFeatureModifiedState $
                C.ResultState nres
        GR.ProveRefine shp -> do
          let addrWidth = MC.addrWidthRepr (Proxy @w)
          let rdata = pers ^. GP.personality . GP.pRefinementData
          LJ.writeLog
            gla
            ( GrDiag.RefineDiagnostic $
                GRDiag.RefinementUsingPrecondition addrWidth (GR.refineInputArgNames (GR.refineInputs rdata)) shp
            )
          CB.clearProofObligations bak
          CB.resetAssumptionState bak
          initSt <- config shp
          trc <- getRecordedTrace st

          C.ExecutionFeatureNewState
            <$> pauseLinearState
              (CB.backendGetSym bak)
              initSt
              trc
              refineReplay
 where
  getRecordedTrace result = do
    -- These panics are impossible. Because we use path splitting, there will
    -- be no nontrivial predicates that are branched on.
    let mergeStates _simCtx _loc _pred _globs1 _globs2 =
          Scrch.panic "executeCfgPath " ["should use path splitting"]
    globs <- C.execResultGlobals mergeStates result
    let sym = CB.backendGetSym bak
    let evalBool b =
          case WI.asConstantPred b of
            Just b' -> pure b'
            Nothing -> Scrch.panic "getRecordedTrace" ["should use path splitting"]
    let traceVar = C.execResultContext result Lens.^. C.cruciblePersonality . CR.recordState
    CR.getConcreteRecordedTrace globs traceVar sym evalBool

-- | A feature that uses GREASE to refine states where the obligations are not provable and restart the symbolic executor
-- or annotates the state with bug information (if a potential bug is found.)
refineFeature ::
  forall solver sym bak t st fm ext w p tys ret rtp cExt.
  ( ?memOpts :: CLM.MemOptions
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , ExtShape ext ~ PtrShape ext w
  , C.IsSyntaxExtension ext
  , HasScreachPersonality p sym bak t cExt ext tys ret rtp w
  , CB.IsSymInterface sym
  , ToConc.HasToConcretize p
  , 16 CT.<= w
  , CLM.HasPtrWidth w
  , MC.MemWidth w
  ) =>
  bak ->
  ScreachLogAction ->
  GreaseLogAction ->
  RftOpt.RefineReplay ->
  (ArgShapes ext Shape.NoTag tys -> IO (C.ExecState p sym ext (CS.RegEntry sym ret))) ->
  C.ExecutionFeature p sym ext rtp
refineFeature bak sla gla refineReplay initCfg = C.ExecutionFeature $ \case
  C.ResultState (C.TimeoutResult _) -> do
    doLog
      sla
      RDiag.RefinementStateTimedOut
    pure
      C.ExecutionFeatureNoChange
  C.ResultState r ->
    refineState bak sla gla refineReplay r initCfg
  _ -> pure C.ExecutionFeatureNoChange

-- | Create the execution features for SDSE (Shortest Distance Symbolic Execution).
--
-- Returns the features and an IO action to retrieve saved target results.
sdseExecFeatures ::
  forall rtp sym ret p ext t st fs bak solver tys w fm cExt.
  ( rtp ~ CS.RegEntry sym ret
  , CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder t st fs
  , bak ~ CBO.OnlineBackend solver t st fs
  , ExtShape ext ~ PtrShape ext w
  , WPO.OnlineSolver solver
  , WE.Flags fm ~ fs
  , C.IsSyntaxExtension ext
  , GP.HasPersonality p sym bak t cExt ext ret tys w
  , HasRefineResult p sym ext tys
  , ToConc.HasToConcretize p
  , GP.HasMemVar p
  , 16 CT.<= w
  , CLM.HasPtrWidth w
  , MC.MemWidth w
  , ?memOpts :: CLM.MemOptions
  , CR.HasRecordState p p sym ext rtp
  , CR.HasReplayState p p sym ext rtp
  ) =>
  bak ->
  ScreachLogAction ->
  GreaseLogAction ->
  RftOpt.RefineReplay ->
  -- | The function that creates a new initial state from a fresh argument spec.
  (ArgShapes ext Shape.NoTag tys -> IO (C.ExecState p sym ext (CS.RegEntry sym ret))) ->
  Sched.PrioritizationFunction p sym ext rtp ->
  -- | Determines if a given state is the target state for SDSE
  (C.ExecResult p sym ext rtp -> IO Bool) ->
  RftOpt.AllSolutions ->
  -- | Returns the features and an IORef to retrieve 'RefineResult's after execution
  IO ([C.ExecutionFeature p sym ext (CS.RegEntry sym ret)], IORef [RefineResult sym ext tys])
sdseExecFeatures bak sla gla refineReplay initCFG priorityFunc isTargetPred (RftOpt.AllSolutions exploreMore) = do
  refineResultsRef <- newIORef []

  let satPolicy =
        Sched.withSatisfiabilityCheck (CBO.considerSatisfiability bak) $
          Sched.priorityPolicy priorityFunc
  let resultCallback r = do
        t <- isTargetPred r
        if t
          then do
            let pers = C.execResultContext r ^. C.cruciblePersonality
            case pers ^. refineResult of
              Just rr -> modifyIORef' refineResultsRef (rr :)
              _ -> pure ()
            if exploreMore
              then pure Sched.ContinueExploring
              else pure Sched.StopExploring
          else pure Sched.ContinueExploring
  (bf, rf, _wq) <- Sched.schedulerFeatures bak satPolicy resultCallback

  let feats =
        -- It is important that the refine feature comes before the scheduler so that we refine aborted states.
        -- otherwise we will just skip the state and throw it away since it is not a target abort.
        [ refineFeature bak sla gla refineReplay initCFG
        , rf
        , bf
        ]
  pure (feats, refineResultsRef)
