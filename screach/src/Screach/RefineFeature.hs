{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Screach.RefineFeature (
  refineFeature,
  HasRefinmentState (..),
  SrchRefineData (..),
  RefineResult (..),
  greaseRefineData,
  refineResult,
  llvmMemVar,
  refineErrMap,
  HasScreachPersonality,
  sdseExecFeatures,
) where

import Control.Lens ((&), (.~), (?~), (^.))
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Proxy (Proxy))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Constraint, Type)
import Data.Macaw.CFG qualified as MC
import Data.Map qualified as Map
import Data.Parameterized.Ctx (Ctx)
import Data.Parameterized.Nonce (Nonce)
import Data.Parameterized.Nonce qualified as Nonce
import GHC.IORef qualified as IORef
import GHC.TypeLits (type Natural)
import Grease.Bug qualified as GR
import Grease.Concretize qualified as GR
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (GreaseLogAction)
import Grease.Diagnostic qualified as GrDiag
import Grease.ErrorDescription (ErrorDescription)
import Grease.Heuristic qualified as GH
import Grease.Refine qualified as GR
import Grease.Refine.Diagnostic qualified as GRDiag
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
import Screach.RefinementOptions qualified as RftOpt
import Screach.Run.Diagnostic qualified as RDiag
import What4.Expr qualified as WE
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
  , refineResultTrace :: CR.RecordedTrace sym
  }

-- | Per-'ExecState' data needed for refinement.
--
-- Type parameters:
--
-- - @sym@: instance of 'Lang.Crucible.Backend.IsSymInterface'
-- - @bak@: instance of @'Lang.Crucible.Backend.IsSymBackend' sym@
-- - @scope@ type-level nonce generator name for 'What4.Expr.Expr'
-- - @ext@: language extension, see "Lang.Crucible.CFG.Extension"
-- - @aty@: Crucible argument types for the target function
-- - @w@: pointer width (Natural)
type SrchRefineData ::
  Type ->
  Type ->
  Type ->
  Type ->
  Ctx CT.CrucibleType ->
  Natural ->
  Type
data SrchRefineData sym bak scope ext aty w = SrchRefineData
  { _refineErrMap :: IORef (Map.Map (Nonce scope CT.BaseBoolType) (ErrorDescription sym))
  , _greaseRefineData :: GR.RefinementData sym bak ext aty w
  , _refineResult :: Maybe (RefineResult sym ext aty)
  -- ^ At termination if this is a ProveBug we store the CEX
  -- and details about what bug this CEX is for
  , _llvmMemVar :: CS.GlobalVar CLM.Mem
  }

Lens.makeLenses ''SrchRefineData

type HasRefinmentState ::
  Type -> Type -> Type -> Type -> Type -> CT.Ctx CT.CrucibleType -> Natural -> Constraint

-- | Class for Refinement state, each state has to hold onto its
-- refinement data for `proveAndRefine`
class HasRefinmentState p sym bak t ext aty w | p -> sym ext where
  refinementState :: Lens.Lens' p (SrchRefineData sym bak t ext aty w)

-- | Constraints for collaboration between the scheduler
-- refinement and record replay so that refinement can add
-- saved states to a new state and setup replay on that new initial
-- state
type HasScreachPersonality p sym bak t ext tys ret rtp w =
  ( CS.RegEntry sym ret ~ rtp
  , HasRefinmentState p sym bak t ext tys w
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  )

-- | Saved symbolic execution result with backend state.
--
-- When a target is reached during SDSE, the result and backend state are
-- captured so they can be verified later.
data SavedState p sym ext rtp
  = SavedState
  { savedBackendState :: CB.AssumptionState sym
  , savedExecState :: C.ExecState p sym ext rtp
  }

-- | Create a 'SavedState' from an 'C.ExecResult' by capturing the current
-- backend state.
createSaveItem ::
  C.ExecResult p sym ext rtp ->
  IO (SavedState p sym ext rtp)
createSaveItem st =
  let simCtx = C.execResultContext st
   in C.withBackend simCtx $ \bak -> do
        bakState <- CB.getBackendState bak
        pure
          SavedState
            { savedBackendState = bakState
            , savedExecState = C.ResultState st
            }

doLog :: MonadIO m => ScreachLogAction -> RDiag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (Diag.RunDiagnostic diag)

-- | Given a result from symbolic execution use GREASE to
-- attempt to prove the obligations collected by the given state.
-- Return a feature result that
-- either queues a refinement, a state with bug information for a found bug,
-- or continues if the state is proven safe.
refineState ::
  forall solver sym bak t st fm ext w p rtp tys ret.
  ( ?memOpts :: CLM.MemOptions
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , ExtShape ext ~ PtrShape ext w
  , C.IsSyntaxExtension ext
  , HasRefinmentState p sym bak t ext tys w
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , CB.IsSymInterface sym
  , ToConc.HasToConcretize p
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
  -- | A function that initializes a new crucible state from the argument specification and
  -- an annotation map of term nonces to error descriptions.
  ( ArgShapes ext Shape.NoTag tys ->
    Maybe (IORef.IORef (Map.Map (Nonce.Nonce t CT.BaseBoolType) (GH.ErrorDescription sym))) ->
    IO (C.ExecState p sym ext (CS.RegEntry sym ret))
  ) ->
  IO
    (C.ExecutionFeatureResult p sym ext rtp)
refineState bak sla gla refineReplay st config = do
  let SrchRefineData{_greaseRefineData = rdata, _refineErrMap = errMapRef, _llvmMemVar = memvar} = Lens.view (C.cruciblePersonality . refinementState) (C.execResultContext st)
  LJ.writeLog gla (GrDiag.RefineDiagnostic (GRDiag.ExecutionResult memvar st))
  obls <- C.withBackend (C.execResultContext st) $ \bak' ->
    CB.getProofObligations bak'
  errMap <- readIORef errMapRef
  refResult <- GR.proveAndRefine bak st gla errMap rdata obls
  trc <- GR.getRecordedTrace bak st
  case refResult of
    GR.ProveSuccess -> do
      doLog sla RDiag.RefinementSuccess
      pure C.ExecutionFeatureNoChange
    GR.ProveCantRefine{} -> do
      doLog sla RDiag.RefinementCantRefine
      pure C.ExecutionFeatureNoChange
    GR.ProveNoHeuristic{} -> do
      doLog sla RDiag.RefinementNoHeuristic
      pure C.ExecutionFeatureNoChange
    GR.ProveBug bi cdata ->
      let
        rstate :: Lens.Lens' (C.ExecResult p sym ext rtp) (SrchRefineData sym bak t ext tys w)
        rstate = Sched.execResultContextLens . C.cruciblePersonality . refinementState
        nres =
          st
            & rstate
              . refineResult
              ?~ RefineResult bi cdata trc
       in
        pure $
          C.ExecutionFeatureModifiedState $
            C.ResultState nres
    GR.ProveRefine shp -> do
      let addrWidth = MC.addrWidthRepr (Proxy @w)
      LJ.writeLog
        gla
        ( GrDiag.RefineDiagnostic $
            GRDiag.RefinementUsingPrecondition addrWidth (GR.refineArgNames rdata) shp
        )
      CB.clearProofObligations bak
      CB.resetAssumptionState bak
      initSt <-
        config
          shp
          (Just errMapRef)

      C.ExecutionFeatureNewState
        <$> GR.pauseLinearState
          (CB.backendGetSym bak)
          initSt
          trc
          refineReplay

-- | A feature that uses GREASE to refine states where the obligations are not provable and restart the symbolic executor
-- or annotates the state with bug information (if a potential bug is found.)
refineFeature ::
  ( ?memOpts :: CLM.MemOptions
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , ExtShape ext ~ PtrShape ext w
  , C.IsSyntaxExtension ext
  , HasScreachPersonality p sym bak t ext tys ret rtp w
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
  ( ArgShapes ext Shape.NoTag tys ->
    Maybe (IORef.IORef (Map.Map (Nonce.Nonce t CT.BaseBoolType) (GH.ErrorDescription sym))) ->
    IO (C.ExecState p sym ext (CS.RegEntry sym ret))
  ) ->
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
  forall rtp sym ret p ext scope st fs bak solver tys w fm.
  ( rtp ~ CS.RegEntry sym ret
  , CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , ExtShape ext ~ PtrShape ext w
  , WPO.OnlineSolver solver
  , WE.Flags fm ~ fs
  , C.IsSyntaxExtension ext
  , HasRefinmentState p sym bak scope ext tys w
  , ToConc.HasToConcretize p
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
  -- | The function that creates a new initial state from a fresh argument spec and
  -- an annotation map of errors.
  ( ArgShapes ext Shape.NoTag tys ->
    Maybe (IORef.IORef (Map.Map (Nonce.Nonce scope CT.BaseBoolType) (GH.ErrorDescription sym))) ->
    IO (C.ExecState p sym ext (CS.RegEntry sym ret))
  ) ->
  Sched.PrioritizationFunction p sym ext rtp ->
  -- | Determines if a given state is the target state for SDSE
  (HasRefinmentState p sym bak scope ext tys w => C.ExecResult p sym ext rtp -> IO Bool) ->
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
            let rst :: SrchRefineData sym bak scope ext tys w
                rst = C.execResultContext r ^. C.cruciblePersonality . refinementState
            case rst ^. refineResult of
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
