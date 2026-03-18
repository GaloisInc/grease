{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : (c) Galois, Inc. 2024
-- Maintainer  : GREASE Maintainers <grease@galois.com>
-- Module      : Grease.Refine
-- Description : Discover sufficient preconditions for executing a function
--
-- For a high-level overview of the refinement algorithm, see the project
-- documentation. The follow text describes the refinement step in additional
-- detail.
--
-- The goal of the refinement loop is to discover sufficient preconditions for
-- the successful execution of a target function. First, it symbolically simulates
-- the Crucible CFG corresponding to the function using the most general possible
-- precondition: all registers are set to fresh, symbolic values, other than the
-- stack and instruction pointers, which point to the end of an allocation and the
-- beginning of the function, respectively. If a memory error occurs (out-of-bounds
-- read/write, uninitialized read, etc.), the precondition is refined using
-- heuristics, such as expanding allocations or initializing memory.
--
-- In a bit more detail: the preconditions are represented as a map from
-- registers to \"shapes\" (see 'ArgShapes', 'Shape'). Most registers hold
-- either a 'ShapeBV' or 'ShapePtr'. Initially, these are \"minimal\" shapes (see
-- 'Grease.Shape.minimal'), meaning that pointers are not presumed to point to
-- anything in particular.
--
-- Before symbolic simulation, the preconditions flow to 'Grease.Setup.setup',
-- which initializes memory and registers according to the precondition.
-- Each fresh symbolic value (including the pointers and bitvectors inside
-- the registers, as well as anything they may point to) is \"annotated\"
-- via 'What4.Interface.annotateTerm'. These annotations are used later in
-- the heuristics that decide how to modify the precondition after an error.
-- The annotations are saved alongside a path to the annotated value (see
-- 'Grease.Setup.Annotations.Annotations'). Such paths are called 'Selector's, and
-- start at a register and consist of a sequence of offsets (indices). They denote
-- a series of pointer derferences that would start at the register and end at the
-- annotated value.
--
-- After setting up memory and registers, simulation can begin. If simulation
-- succeeds, a sufficient precondition has been found and the refinement loop
-- ends. If instead there is a memory error
-- ('Lang.Crucible.LLVM.Errors.MemoryError'), the error is passed along to a series
-- of heuristics ('Grease.Heuristic.defaultHeuristics').
--
-- Each heuristic takes in a memory error and attempts to abduce a new precondition
-- that would mitigate it. To do so, they must figure out which part of the inputs
-- (registers and their pointees) were involved in the error. They search the
-- subterms of the symbolic predicate that represents the error condition for the
-- annotations added in 'Grease.Setup.setup'. For example, if the function hit
-- an out-of-bounds write, then to know which allocation must be expanded, the
-- heuristics find the annotation on the pointer that was written to, and use the
-- corresponding saved 'Selector' to refine the precondition. The heuristics
-- generally:
--
-- - Increase the size of pointee shapes (by adding uninitialized bytes at the end)
-- - Turn uninitialized pointee shapes into initialized ones
-- - Replace (un)initialized pointee shapes with pointer shapes
--
-- After refining the precondition, provided no timeout or iteration limit has been
-- hit, symbolic simulation can be attempted again.
module Grease.Refine (
  -- * Core types
  ProveRefineResult (..),
  NoHeuristic (..),
  RefinementData (..),
  RefinementSummary (..),
  ErrorCallbacks (..),

  -- * Refinement feature
  RefineReplay (..),
  refineFeature,
  pauseLinearState,
  getRecordedTrace,

  -- * Proving
  proveAndRefine,
  processExecResult,
  buildErrMaps,

  -- * Implementation details
  findPredAnnotations,
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)
import Data.IORef qualified as IORef
import Data.List.NonEmpty qualified as NE
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce (Nonce)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IORef (newIORef)
import GHC.TypeNats (type (<=))
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (Diagnostic (RefineDiagnostic), GreaseLogAction)
import Grease.ErrorDescription (ErrorDescription (CrucibleLLVMError, MacawMemError))
import Grease.Heuristic (HeuristicResult (CantRefine, PossibleBug, RefinedPrecondition, Unknown), OnlineSolverAndBackend, RefineHeuristic)
import Grease.Heuristic.Result (CantRefine (Exhausted, Exit, MissingFunc, MissingSemantics, MutableGlobal, SolverTimeout, SolverUnknown, Timeout, Unsupported))
import Grease.Panic (panic)
import Grease.Refine.Diagnostic qualified as Diag
import Grease.Setup.Annotations qualified as Anns
import Grease.Shape (ArgShapes, ExtShape, PrettyExt)
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Pointer (PtrDataMode (Precond), PtrShape)
import Grease.Solver (Solver, solverAdapter)
import Grease.ValueName (ValueName)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Prove qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.Partial qualified as Mem
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.EvalStmt qualified as CSE
import Lang.Crucible.Simulator.ExecutionTree (execResultContext, execResultGlobals)
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Simulator.SimError qualified as C
import Lang.Crucible.Utils.Timeout qualified as C
import Lumberjack qualified as LJ
import System.Exit qualified as Exit
import What4.Expr qualified as WE
import What4.Expr.App qualified as W4
import What4.Interface qualified as WI
import What4.LabeledPred qualified as W4
import What4.Solver qualified as W4

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (RefineDiagnostic diag)

-- | Find annotations on a term and its subterms, all of type 'WI.BaseBoolType'.
--
-- Similar to 'Grease.Setup.Annotations.findAnnotation', but only for
-- annotations on predicates.
findPredAnnotations ::
  forall sym brand st fs t.
  ( CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder brand st fs
  ) =>
  sym ->
  WI.SymExpr sym t ->
  [WI.SymAnnotation sym WI.BaseBoolType]
findPredAnnotations sym e = case W4.asApp e of
  Just app -> do
    let anns :: [WI.SymAnnotation sym WI.BaseBoolType]
        anns = MC.foldMapFC (maybeToList . getPredAnn) app
    case getPredAnn e of
      Nothing -> anns
      Just ann -> ann : anns
  Nothing -> maybeToList (getPredAnn e)
 where
  getPredAnn ::
    forall tp.
    W4.Expr brand tp ->
    Maybe (WI.SymAnnotation sym WI.BaseBoolType)
  getPredAnn expr =
    case WI.exprType expr of
      WI.BaseBoolRepr -> WI.getAnnotation sym expr
      _ -> Nothing

-- | A proof obligation failed, and no heuristic identified a refinement
data NoHeuristic sym ext tys
  = NoHeuristic
  { noHeuristicGoal :: CB.ProofObligation sym
  , noHeuristicConcretizedData :: ConcretizedData sym ext tys
  , noHeuristicError :: Maybe (ErrorDescription sym)
  }

data ProveRefineResult sym ext tys
  = -- | All goals succeeded
    ProveSuccess
  | -- | Heuristic reports a possible bug
    ProveBug Bug.BugInstance (ConcretizedData sym ext tys)
  | -- | One or more goals failed, but no refinement was identified
    ProveNoHeuristic (NE.NonEmpty (NoHeuristic sym ext tys))
  | -- | Can\'t continue refining for some explicit reason
    ProveCantRefine CantRefine
  | -- | Goals failed and a refined configuration was built
    ProveRefine (ArgShapes ext NoTag tys)

-- | How to combine intermediate results. Not exported.
--
-- Uses the same logic as 'combineResults', but short-circuits when the first
-- result is already decisive (e.g., 'ProveCantRefine' or 'ProveRefine').
combiner :: C.Combiner (ExceptT C.TimedOut IO) (ProveRefineResult sym ext argTys)
combiner = C.Combiner $ \mr1 mr2 -> do
  r1 <- mr1
  let v1 = C.subgoalResult r1
  case v1 of
    -- Short-circuit: these results dominate regardless of other goals
    ProveCantRefine{} -> pure r1
    ProveRefine{} -> pure r1
    ProveSuccess -> mr2
    -- For remaining cases, evaluate the second goal and combine
    _ -> do
      r2 <- mr2
      pure (C.SubgoalResult False (combineResults v1 (C.subgoalResult r2)))

data ErrorCallbacks sym t
  = ErrorCallbacks
  { llvmErrCallback :: LLCS.CallStack -> Mem.BoolAnn sym -> CLLVM.BadBehavior sym -> IO ()
  , macawAssertionCallback :: sym -> WI.Pred sym -> MSM.MacawError sym -> IO (WI.Pred sym)
  , errorMap :: IORef (Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym))
  }

-- | Builds a mapping from assertions to errors if those assertions are unprovable
--
-- There are two types of errors 'CLLVM.BadBehavior' which
-- describes errors emitted from the llvm memory model/semantics
-- and 'MSM.MacawError' which is emitted from Macaw specific errors.
-- The LLVM callback is intended to be passed to 'recordLLVMAnnotation'
-- and the Macaw callback is intended to be passed to 'processMacawAssert'.
-- In this configuration, Macaw and Crucible-LLVM will record errors
-- to the 'errorMap' as an 'ErrorDescription'
buildErrMaps ::
  sym ~ WE.ExprBuilder t st fs =>
  Maybe
    ( IORef
        ( Map.Map
            (Nonce t WI.BaseBoolType)
            (ErrorDescription sym)
        )
    ) ->
  IO (ErrorCallbacks sym t)
buildErrMaps mbBBMap = do
  bbMapRef <- Maybe.maybe (newIORef Map.empty) pure mbBBMap
  let recordLLVMAnnotation callStack (Mem.BoolAnn ann) bb =
        modifyIORef bbMapRef $
          Map.insert ann (CrucibleLLVMError bb callStack)
  let processMacawAssert sym p err = do
        (ann, p') <- WI.annotateTerm sym p
        _ <- modifyIORef bbMapRef $ Map.insert ann (MacawMemError err)
        pure p'
  pure
    ErrorCallbacks
      { errorMap = bbMapRef
      , llvmErrCallback = recordLLVMAnnotation
      , macawAssertionCallback = processMacawAssert
      }

-- | Look up error information for a goal from its annotations. Not exported.
lookupErrorInfo ::
  forall sym t st fs.
  ( CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder t st fs
  ) =>
  sym ->
  GreaseLogAction ->
  WI.Pred sym ->
  C.SimError ->
  Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym) ->
  IO (Maybe (ErrorDescription sym))
lookupErrorInfo sym la goalPred _simErr bbMap =
  case findPredAnnotations sym goalPred of
    [] -> do
      doLog la Diag.NoAnnotationOnPredicate
      pure Nothing
    (ann : _) -> case Map.lookup ann bbMap of
      Nothing -> do
        doLog la Diag.PredNotFound
        pure Nothing
      info -> pure info

-- | How to consume the results of trying to prove a goal. Not exported.
consumer ::
  forall p ext r solver sym bak t st argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 <= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  CS.ExecResult p sym ext r ->
  GreaseLogAction ->
  Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym) ->
  RefinementData sym bak ext argTys w ->
  C.ProofConsumer sym t (ProveRefineResult sym ext argTys)
consumer bak execResult la bbMap refineData = do
  let RefinementData
        { refineAnns = anns
        , refineArgNames = argNames
        , refineArgShapes = argShapes
        , refineHeuristics = heuristics
        , refineInitState = initState
        } = refineData
  C.ProofConsumer $ \goal result -> do
    let sym = CB.backendGetSym bak
    let lp = CB.proofGoal goal
    let simErr = lp ^. W4.labeledPredMsg
    minfo <- lookupErrorInfo sym la (lp ^. W4.labeledPred) simErr bbMap
    case result of
      C.Proved{} -> do
        doLog la (Diag.SolverGoalPassed (C.simErrorLoc simErr))
        pure ProveSuccess
      C.Disproved groundEvalFn _ -> do
        toConc <- ToConc.readToConcretize groundEvalFn execResult
        cData <- Conc.makeConcretizedData bak groundEvalFn minfo initState toConc
        doLog la $ Diag.SolverGoalFailed sym lp minfo
        let
          runHeuristics ::
            [RefineHeuristic sym bak ext argTys] ->
            ArgShapes ext NoTag argTys ->
            IO (ProveRefineResult sym ext argTys)
          runHeuristics (h : hs) fc = do
            let initMem = Conc.initStateMem initState
            res <- liftIO (h bak anns initMem goal minfo argNames fc)
            case res of
              CantRefine reason -> do
                doLog la (Diag.CantRefine reason)
                pure $ ProveCantRefine reason
              PossibleBug bug -> pure (ProveBug bug cData)
              RefinedPrecondition fc' -> pure $ ProveRefine fc'
              Unknown -> runHeuristics hs fc
          runHeuristics [] _ =
            pure (ProveNoHeuristic (NE.singleton (NoHeuristic goal cData minfo)))
        runHeuristics heuristics argShapes
      C.Unknown{} -> pure (ProveCantRefine SolverUnknown)

-- | Data needed for refinement
data RefinementData sym bak ext argTys wptr
  = RefinementData
  { refineAnns :: Anns.Annotations sym ext argTys
  , refineArgNames :: Ctx.Assignment ValueName argTys
  , refineArgShapes :: ArgShapes ext NoTag argTys
  , refineHeuristics :: [RefineHeuristic sym bak ext argTys]
  , refineInitState :: Conc.InitialState sym ext argTys wptr
  , refineSolver :: Solver
  , refineSolverTimeout :: C.Timeout
  }

-- | Helper, not exported
proveAndRefine ::
  forall p ext r solver sym bak t st argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 <= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  CS.ExecResult p sym ext r ->
  GreaseLogAction ->
  Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym) ->
  RefinementData sym bak ext argTys w ->
  CB.ProofObligations sym ->
  IO (ProveRefineResult sym ext argTys)
proveAndRefine bak execResult la bbMap refineData goals = do
  let sym = CB.backendGetSym bak
  let solver = refineSolver refineData
  let tout = refineSolverTimeout refineData
  let prover = C.offlineProver tout sym W4.defaultLogData (solverAdapter solver)
  let strat = C.ProofStrategy prover combiner
  let cons = consumer bak execResult la bbMap refineData
  case goals of
    Nothing -> pure ProveSuccess
    Just goals' ->
      liftIO (runExceptT (C.proveGoals strat goals' cons))
        Monad.>>= \case
          Left C.TimedOut -> pure (ProveCantRefine SolverTimeout)
          Right r -> pure r

processExecResult ::
  CS.ExecResult p sym ext r ->
  Maybe CantRefine
processExecResult =
  \case
    CS.TimeoutResult _execState ->
      Just Timeout
    CS.AbortedResult _ (CS.AbortedExit Exit.ExitSuccess _) ->
      Just (Exit (Just 0))
    CS.AbortedResult _ (CS.AbortedExit (Exit.ExitFailure code) _) ->
      Just (Exit (Just code))
    CS.AbortedResult _ (CS.AbortedExec (CB.EarlyExit _loc) _gp) ->
      Just (Exit Nothing)
    CS.AbortedResult _ (CS.AbortedExec (CB.AssertionFailure (C.SimError _loc (C.ResourceExhausted msg))) _gp) ->
      Just (Exhausted msg)
    CS.AbortedResult _ (CS.AbortedExec (CB.AssertionFailure (C.SimError _loc (C.Unsupported _cs feat))) _gp) ->
      Just (Unsupported feat)
    CS.AbortedResult ctx (CS.AbortedBranch _loc _pred r1 r2) ->
      -- Taking the first 'CantRefine' is consistent with 'consumer'
      processExecResult (CS.AbortedResult ctx r1)
        <|> processExecResult (CS.AbortedResult ctx r2)
    _ -> Nothing


data RefinementSummary sym ext tys
  = RefinementSuccess (ArgShapes ext NoTag tys)
  | RefinementNoHeuristic (NE.NonEmpty (NoHeuristic sym ext tys))
  | RefinementItersExceeded
  | RefinementCantRefine CantRefine
  | RefinementBug Bug.BugInstance (ConcretizedData sym ext tys)


-- | Whether to follow the previously explored trace when refining.
newtype RefineReplay = RefineReplay {getRefineReplay :: Bool}
  deriving newtype (Read, Show)

-- | Take a fresh initial state (from a given refinement) and set it up for
-- replay if configured.
--
-- Returning this via 'CS.ExecutionFeatureNewState' is not ideal because the
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
-- Overall while this situation isn't ideal it is not super problematic because
-- straightline symbolic execution on the refined state until some split is
-- relatively cheap.
--
-- Panics if the given state is not an 'CS.InitialState'.
pauseLinearState ::
  CR.HasReplayState p p sym ext (CS.RegEntry sym ret) =>
  sym ->
  -- | The new init state
  CS.ExecState p sym ext rtp ->
  -- | The trace to replay when starting the new state
  CR.RecordedTrace sym ->
  -- | Whether to replay the trace after refinement
  RefineReplay ->
  IO (CS.ExecState p sym ext rtp)
pauseLinearState sym st trc (RefineReplay shouldReplay) =
  case st of
    CS.InitialState ctx glb ah tyRepr cont -> do
      empTrc <- CR.emptyRecordedTrace sym
      let toUseTrace = if shouldReplay then trc else empTrc
      let stWithTrace = ctx & CS.cruciblePersonality . CR.replayState . CR.initialTrace .~ toUseTrace
      pure $ CS.InitialState stWithTrace glb ah tyRepr cont
    _ -> panic "pauseLinearState" ["Expecting an initial state"]

-- | Get the recorded trace from an execution result.
--
-- Panics if path merging was used (i.e., if globals were merged or a
-- non-constant branch predicate is encountered). Callers should ensure
-- path splitting is used.
getRecordedTrace ::
  ( CB.IsSymBackend sym bak
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  ) =>
  bak ->
  CS.ExecResult p sym ext (CS.RegEntry sym ret) ->
  IO (CR.RecordedTrace sym)
getRecordedTrace bak result = do
  let mergeStates _simCtx _loc _pred _globs1 _globs2 =
        panic "getRecordedTrace" ["Unexpected merged globals"]
  globs <- execResultGlobals mergeStates result
  let sym = CB.backendGetSym bak
  let traceVar =
        execResultContext result
          Lens.^. CS.cruciblePersonality . CR.recordState
  CR.getRecordedTrace globs traceVar sym

-- | Combine two 'ProveRefineResult's, preferring actionable results.
combineResults ::
  ProveRefineResult sym ext tys ->
  ProveRefineResult sym ext tys ->
  ProveRefineResult sym ext tys
combineResults old new = case old of
  -- Can't refine further, no matter what the other result would be
  ProveCantRefine{} -> old
  -- If we find a refinement, don't bother with other goals
  ProveRefine{} -> old
  -- If this goal succeeded, continue with the others
  ProveSuccess -> new
  -- A bug is only reachable if there are no other errors
  ProveBug{} -> case new of
    ProveRefine{} -> new
    ProveCantRefine{} -> new
    ProveNoHeuristic{} -> new
    ProveBug{} -> old -- would be nice to combine old+new here
    ProveSuccess -> old
  ProveNoHeuristic errs1 -> case new of
    ProveCantRefine{} -> new
    -- If we manage to refine the second goal, use that
    ProveRefine{} -> new
    -- Otherwise, no heuristic propagates
    ProveBug{} -> old
    ProveSuccess -> old
    ProveNoHeuristic errs2 -> ProveNoHeuristic (errs1 <> errs2)

-- | A Crucible 'CSE.ExecutionFeature' that implements the refinement loop.
--
-- When symbolic execution completes (hits a 'CS.ResultState'), the feature
-- proves the collected obligations and either:
--
-- * Restarts execution with refined preconditions ('CSE.ExecutionFeatureNewState')
-- * Stores the final result and lets execution finish ('CSE.ExecutionFeatureNoChange')
--
-- Returns the feature and an IO action to read the final result after
-- 'CS.executeCrucible' completes.
refineFeature ::
  forall p ext solver sym bak t st fm argTys ret w rtp.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , 16 <= w
  , CLM.HasPtrWidth w
  , MC.MemWidth w
  , ToConc.HasToConcretize p
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , ExtShape ext ~ PtrShape ext w
  , CS.RegEntry sym ret ~ rtp
  , PrettyExt ext 'Precond NoTag
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  RefineReplay ->
  -- | Maximum number of refinement iterations ('Nothing' for unlimited)
  Maybe Int ->
  -- | Memory model global variable (for logging)
  CS.GlobalVar CLM.Mem ->
  -- | Argument names (for logging)
  Ctx.Assignment ValueName argTys ->
  -- | Initial refinement data for the first iteration
  RefinementData sym bak ext argTys w ->
  -- | Initial error map ref for the first iteration
  IORef (Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym)) ->
  -- | Callback to create a new execution state when refinement is needed.
  -- Takes the refined shapes and the shared error map ref.
  -- Returns the new 'CS.ExecState' and updated 'RefinementData'.
  ( ArgShapes ext NoTag argTys ->
    IORef (Map.Map (Nonce t WI.BaseBoolType) (ErrorDescription sym)) ->
    IO (CS.ExecState p sym ext rtp, RefinementData sym bak ext argTys w)
  ) ->
  IO
    ( CSE.ExecutionFeature p sym ext rtp
    , IO (RefinementSummary sym ext argTys)
    )
refineFeature bak la refineReplay maxIters memVar argNames initRefineData initErrMapRef mkIterState = do
  refineDataRef <- IORef.newIORef initRefineData
  errMapRefRef <- IORef.newIORef initErrMapRef
  resultRef <- IORef.newIORef (Nothing :: Maybe (ProveRefineResult sym ext argTys))
  iterRef <- IORef.newIORef (0 :: Int)

  let addrWidth0 = MC.addrWidthRepr (Proxy @w)
  doLog la (Diag.RefinementUsingPrecondition addrWidth0 argNames (refineArgShapes initRefineData))

  let
    -- Very short summary for single-line log message
    shortResult :: ProveRefineResult sym ext argTys -> Text
    shortResult =
      \case
        ProveSuccess -> "success"
        ProveBug{} -> "likely bug"
        ProveNoHeuristic{} -> "possible bug"
        ProveRefine{} -> "refined precondition"
        ProveCantRefine (Exhausted{}) -> "resource exhausted"
        ProveCantRefine (Exit (Just code)) -> "exited with " <> Text.pack (show code)
        ProveCantRefine (Exit Nothing) -> "exited"
        ProveCantRefine (MissingFunc (Just nm)) -> "missing function " <> Text.pack nm
        ProveCantRefine (MissingFunc{}) -> "missing function"
        ProveCantRefine (MissingSemantics{}) -> "missing semantics"
        ProveCantRefine (MutableGlobal{}) -> "load from mut global"
        ProveCantRefine (SolverTimeout{}) -> "solver timeout"
        ProveCantRefine (SolverUnknown{}) -> "solver unknown"
        ProveCantRefine (Timeout{}) -> "symex timeout"
        ProveCantRefine (Unsupported{}) -> "unsupported feature"

    storeResult :: ProveRefineResult sym ext argTys -> IO ()
    storeResult new = do
      old <- readIORef resultRef
      case old of
        Nothing -> writeIORef resultRef (Just new)
        Just prev -> writeIORef resultRef (Just (combineResults prev new))

    feature :: CSE.ExecutionFeature p sym ext rtp
    feature = CSE.ExecutionFeature $ \case
      CS.ResultState r -> do
        doLog la (Diag.ExecutionResult memVar r)
        -- Capture proof obligations, then clean up solver state.
        obls <- CB.getProofObligations bak
        CB.clearProofObligations bak
        CB.resetAssumptionState bak
        case processExecResult r of
          Just cantRefine -> do
            storeResult (ProveCantRefine cantRefine)
            pure CSE.ExecutionFeatureNoChange
          Nothing -> do
            currentErrMapRef <- readIORef errMapRefRef
            errMap <- readIORef currentErrMapRef
            currentRefineData <- readIORef refineDataRef
            proveResult <- proveAndRefine bak r la errMap currentRefineData obls
            -- Log path completion with result summary
            loc <- WI.getCurrentProgramLoc (CB.backendGetSym bak)
            doLog la (Diag.RefinementFinishedPath loc (shortResult proveResult))
            case proveResult of
              ProveRefine shp -> do
                iters <- readIORef iterRef
                if Maybe.maybe False (iters >=) maxIters
                  then do
                    doLog la Diag.RefinementLoopMaximumIterationsExceeded
                    storeResult (ProveRefine shp)
                    pure CSE.ExecutionFeatureNoChange
                  else do
                    IORef.modifyIORef' iterRef (+ 1)
                    let addrWidth = MC.addrWidthRepr (Proxy @w)
                    doLog la (Diag.RefinementUsingPrecondition addrWidth argNames shp)
                    doLog la Diag.RefinementLoopRetrying
                    -- Reset result for new iteration
                    writeIORef resultRef Nothing
                    (newState, newRefineData) <- mkIterState shp currentErrMapRef
                    writeIORef refineDataRef newRefineData
                    let sym = CB.backendGetSym bak
                    finalState <-
                      if getRefineReplay refineReplay
                        then do
                          trc <- getRecordedTrace bak r
                          pauseLinearState sym newState trc refineReplay
                        else pure newState
                    pure (CSE.ExecutionFeatureNewState finalState)
              other -> do
                storeResult other
                pure CSE.ExecutionFeatureNoChange
      _ -> pure CSE.ExecutionFeatureNoChange

  let
    readResult :: IO (RefinementSummary sym ext argTys)
    readResult = do
      mr <- readIORef resultRef
      currentRefineData <- readIORef refineDataRef
      let currentShapes = refineArgShapes currentRefineData
      case mr of
        Nothing -> do
          -- No result stored means execution completed without any ResultState
          -- being processed. This shouldn't normally happen.
          pure (RefinementSuccess currentShapes)
        Just ProveSuccess -> do
          doLog la Diag.RefinementLoopAllGoalsPassed
          let addrWidth = MC.addrWidthRepr (Proxy @w)
          doLog la (Diag.RefinementFinalPrecondition addrWidth argNames currentShapes)
          pure (RefinementSuccess currentShapes)
        Just (ProveBug b cData) ->
          pure (RefinementBug b cData)
        Just (ProveCantRefine cr) ->
          pure (RefinementCantRefine cr)
        Just (ProveNoHeuristic errs) -> do
          doLog la Diag.RefinementLoopNoHeuristic
          pure (RefinementNoHeuristic errs)
        Just (ProveRefine _) ->
          -- This only happens when maxIters was hit
          pure RefinementItersExceeded

  pure (feature, readResult)
