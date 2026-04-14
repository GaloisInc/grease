{-# LANGUAGE DataKinds #-}
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
  ProveRefineResult (..),
  NoHeuristic (..),
  proveAndRefine,
  ExecData (..),
  refineOnce,
  setupRefinement,
  RefinementSetup (..),
  execAndRefine,
  RefinementSummary (..),
  refinementLoop,
  buildErrMaps,
  ErrorCallbacks (..),
  withErrorCallbacks,
  -- The following are used by a downstream project

  -- * Implementation details
  processExecResult,
  shortResult,
  findPredAnnotations,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe qualified as X
import Control.Lens ((^.))
import Control.Monad qualified as Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce (Nonce)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Diagnostic (Diagnostic (RefineDiagnostic), GreaseLogAction)
import Grease.ErrorDescription (ErrorDescription)
import Grease.ExecutionFeatures (boundedExecFeats)
import Grease.Heuristic (HeuristicResult (CantRefine, PossibleBug, RefinedPrecondition, Unknown), OnlineSolverAndBackend, RefineHeuristic)
import Grease.Heuristic.Result (CantRefine (Exhausted, Exit, MissingFunc, MissingSemantics, MutableGlobal, SolverTimeout, SolverUnknown, Timeout, Unsupported))
import Grease.Options (BoundsOpts)
import Grease.Options qualified as Opts
import Grease.Personality qualified as GP
import Grease.Refine.Diagnostic qualified as Diag
import Grease.Refine.ErrorCallbacks (ErrorCallbacks (ErrorCallbacks, errorMap, llvmErrCallback, macawAssertionCallback), buildErrMaps, withErrorCallbacks)
import Grease.Refine.ErrorCallbacks qualified as EC
import Grease.Refine.RefinementData qualified as RD
import Grease.Scheduler qualified as Sched
import Grease.Setup (InitialMem)
import Grease.Setup qualified as Setup
import Grease.Shape (ArgShapes, ExtShape, PrettyExt)
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Pointer (PtrDataMode (Precond), PtrShape)
import Grease.Solver (solverAdapter)
import Grease.SymIO qualified as GSIO
import Grease.Utility (tshow)
import Grease.ValueName (ValueName)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Prove qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout (DataLayout)
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.MemModel.Partial qualified as Mem
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Simulator.SimError qualified as C
import Lang.Crucible.Utils.Timeout qualified as C
import Lumberjack qualified as LJ
import System.Exit qualified as Exit
import What4.Expr qualified as WE
import What4.Expr.App qualified as W4
import What4.FloatMode qualified as W4FM
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
combiner :: C.Combiner (ExceptT C.TimedOut IO) (ProveRefineResult sym ext argTys)
combiner = C.Combiner $ \mr1 mr2 -> do
  r1 <- mr1
  case C.subgoalResult r1 of
    -- can't refine further, no matter what the other result would be
    ProveCantRefine{} -> pure r1
    -- if we find a refinement, don't bother with other goals
    ProveRefine{} -> pure r1
    -- if this goal succeeded, continue with the others
    ProveSuccess -> mr2
    -- a bug is only reachable if there are no other errors
    ProveBug{} -> do
      r2 <- mr2
      case C.subgoalResult r2 of
        ProveRefine{} -> pure r2
        ProveCantRefine{} -> pure r2
        ProveNoHeuristic{} -> pure r2
        ProveBug{} -> pure r1 -- would be nice to combine r1+r2 here
        ProveSuccess -> pure r1
    ProveNoHeuristic errs1 -> do
      r2 <- mr2
      let failed = C.SubgoalResult False
      case C.subgoalResult r2 of
        ProveCantRefine{} -> pure r2
        -- if we manage to refine the second goal, use that
        ProveRefine{} -> pure r2
        -- otherwise, no heuristic propagates
        ProveBug{} -> pure r1
        ProveSuccess -> pure r1
        ProveNoHeuristic errs2 ->
          pure (failed (ProveNoHeuristic (errs1 <> errs2)))

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
  Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym) ->
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

-- | Extract the 'GP.Personality' from an 'CS.ExecResult'. Not exported.
execResultPersonality ::
  GP.HasPersonality p sym bak t cExt ext ret argTys wptr =>
  CS.ExecResult p sym ext rtp ->
  GP.Personality sym bak t cExt ext ret argTys wptr
execResultPersonality r = CS.execResultContext r ^. CS.cruciblePersonality . GP.personality

-- | How to consume the results of trying to prove a goal. Not exported.
consumer ::
  forall p ext ret solver sym bak t st cExt argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret argTys w
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  CS.ExecResult p sym ext (CS.RegEntry sym ret) ->
  GreaseLogAction ->
  C.ProofConsumer sym t (ProveRefineResult sym ext argTys)
consumer bak execResult la = do
  let RD.RefinementData
        { RD.refineInputs = inputs
        , RD.refineAnns = anns
        , RD.refineInitState = initState
        , RD.refineErrMap = errMapRef
        } = execResultPersonality execResult ^. GP.pRefinementData
  let RD.RefinementInputs
        { RD.refineInputArgNames = argNames
        , RD.refineInputArgShapes = argShapes
        , RD.refineInputHeuristics = heuristics
        } = inputs
  C.ProofConsumer $ \goal result -> do
    let sym = CB.backendGetSym bak
    let lp = CB.proofGoal goal
    let simErr = lp ^. W4.labeledPredMsg
    bbMap <- readIORef errMapRef
    minfo <- lookupErrorInfo sym la (lp ^. W4.labeledPred) simErr bbMap
    case result of
      C.Proved{} -> do
        doLog la (Diag.SolverGoalPassed (C.simErrorLoc simErr))
        pure ProveSuccess
      C.Disproved groundEvalFn _ -> do
        toConc <- ToConc.readToConcretize groundEvalFn execResult
        globs <- liftIO (ToConc.execResultGroundGlobals groundEvalFn execResult)
        let traceVar = CS.execResultContext execResult ^. CS.cruciblePersonality . CR.recordState
        cData <- Conc.makeConcretizedData bak groundEvalFn minfo initState toConc globs traceVar
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

-- | Data needed to execute Crucible
data ExecData p sym ext ret
  = ExecData
  { execFeats :: [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)]
  , execInitState :: CS.ExecState p sym ext (CS.RegEntry sym ret)
  , execPathStrat :: Opts.PathStrategy
  }

-- | Helper, not exported
execCfg ::
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  ExecData p sym ext ret ->
  -- | The result of a single execution (in 'Opts.Dfs' or 'Opts.Bfs' mode,
  -- of a single path), the proof obligations resulting from that execution,
  -- and any suspended paths.
  IO
    ( CS.ExecResult p sym ext (CS.RegEntry sym ret)
    , CB.ProofObligations sym
    , Seq (Sched.WorkItem p sym ext (CS.RegEntry sym ret))
    )
execCfg bak execData = do
  let ExecData
        { execFeats = feats
        , execInitState = initialState
        , execPathStrat = strat
        } = execData
  let withCleanup action = X.finally action $ do
        CB.resetAssumptionState bak
        CB.clearProofObligations bak
  withCleanup $
    case strat of
      Opts.Dfs -> do
        (bf, rf, wq) <- Sched.schedulerFeatures bak Sched.dfsPolicy (\_ -> pure Sched.StopExploring)
        r <- CS.executeCrucible (feats ++ [bf, rf]) initialState
        o <- CB.getProofObligations bak
        rest <- Sched.drainAll wq
        pure (r, o, rest)
      Opts.Bfs -> do
        (bf, rf, wq) <- Sched.schedulerFeatures bak Sched.bfsPolicy (\_ -> pure Sched.StopExploring)
        r <- CS.executeCrucible (feats ++ [bf, rf]) initialState
        o <- CB.getProofObligations bak
        rest <- Sched.drainAll wq
        pure (r, o, rest)
      Opts.Sse -> do
        r <- CS.executeCrucible feats initialState
        o <- CB.getProofObligations bak
        pure (r, o, Seq.empty)

-- | Helper, not exported
proveAndRefine ::
  forall p ext ret solver sym bak t st cExt argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret argTys w
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  CS.ExecResult p sym ext (CS.RegEntry sym ret) ->
  GreaseLogAction ->
  CB.ProofObligations sym ->
  IO (ProveRefineResult sym ext argTys)
proveAndRefine bak execResult la goals = do
  let sym = CB.backendGetSym bak
  let refineData = execResultPersonality execResult ^. GP.pRefinementData
  let solver = RD.refineInputSolver (RD.refineInputs refineData)
  let tout = RD.refineSolverTimeout refineData
  let prover = C.offlineProver tout sym W4.defaultLogData (solverAdapter solver)
  let strat = C.ProofStrategy prover combiner
  let cons = consumer bak execResult la
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

execAndRefine ::
  forall ext solver sym bak t st cExt argTys ret w m fm p.
  ( MonadIO m
  , C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , 16 C.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret argTys w
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  GreaseLogAction ->
  ExecData p sym ext ret ->
  m (ProveRefineResult sym ext argTys)
execAndRefine bak _fm la execData = do
  let memVar = GP.execStateMemVar (execInitState execData)
  let refineOne initSt = do
        let execData' = execData{execInitState = initSt}
        (execResult, goals, remaining) <- execCfg bak execData'
        doLog la (Diag.ExecutionResult memVar execResult)
        refineResult <-
          case processExecResult execResult of
            Just cantRefine -> pure (ProveCantRefine cantRefine)
            Nothing ->
              proveAndRefine bak execResult la goals
        case execPathStrat execData of
          Opts.Dfs -> do
            loc <- WI.getCurrentProgramLoc (CB.backendGetSym bak)
            doLog la (Diag.RefinementFinishedPath loc (shortResult refineResult))
          Opts.Bfs -> do
            loc <- WI.getCurrentProgramLoc (CB.backendGetSym bak)
            doLog la (Diag.RefinementFinishedPath loc (shortResult refineResult))
          Opts.Sse -> pure ()
        pure (refineResult, remaining)

  -- Process the state that was passed in
  let initialState = execInitState execData
  (refineResult, remainingPaths) <- liftIO (refineOne initialState)
  remainingRef <- liftIO (IORef.newIORef remainingPaths)

  -- Process new states that may have been generated during execution. Defer to
  -- `combiner` on how to combine the results from multiple states.
  let go ::
        ProveRefineResult sym ext argTys ->
        IO (ProveRefineResult sym ext argTys)
      go r = do
        remaining <- IORef.readIORef remainingRef
        case remaining of
          Seq.Empty -> pure r
          -- Note that we use `SubgoalResult True` because `combiner` doesn't
          -- actually examine the `Bool`, so it doesn't matter what it is.
          (next Seq.:<| rest) -> do
            let firstResult = C.SubgoalResult True r
            let computeNextResult = do
                  doLog la (Diag.ResumingFromBranch (Sched.workItemLoc next))
                  IORef.writeIORef remainingRef rest
                  initSt <- Sched.restoreWorkItem next
                  (nextRes, additionalPaths) <- refineOne initSt
                  IORef.modifyIORef remainingRef (<> additionalPaths)
                  C.SubgoalResult True <$> go nextRes
            let combine r1 r2 = runExceptT (C.getCombiner combiner r1 r2)
            mbRefineResult <-
              combine (pure firstResult) (liftIO computeNextResult)
            case mbRefineResult of
              Left C.TimedOut -> pure (ProveCantRefine SolverTimeout)
              Right combinedResult -> pure (C.subgoalResult combinedResult)
  liftIO (go refineResult)

-- | Very short summary of a 'ProveRefineResult' for single-line log messages.
shortResult :: ProveRefineResult sym ext argTys -> Text.Text
shortResult =
  \case
    ProveSuccess -> "success"
    ProveBug{} -> "likely bug"
    ProveNoHeuristic{} -> "possible bug"
    ProveRefine{} -> "refined precondition"
    ProveCantRefine (Exhausted{}) -> "resource exhausted"
    ProveCantRefine (Exit (Just code)) -> "exited with " <> tshow code
    ProveCantRefine (Exit Nothing) -> "exited"
    ProveCantRefine (MissingFunc (Just nm)) -> "missing function " <> Text.pack nm
    ProveCantRefine (MissingFunc{}) -> "missing function"
    ProveCantRefine (MissingSemantics{}) -> "missing semantics"
    ProveCantRefine (MutableGlobal{}) -> "load from mut global"
    ProveCantRefine (SolverTimeout{}) -> "solver timeout"
    ProveCantRefine (SolverUnknown{}) -> "solver unknown"
    ProveCantRefine (Timeout{}) -> "symex timeout"
    ProveCantRefine (Unsupported{}) -> "unsupported feature"

-- | The result of 'setupRefinement': all data needed to build the initial
-- Crucible exec state and drive refinement.
data RefinementSetup sym bak t ext argTys wptr
  = RefinementSetup
  { setupRefinementData :: RD.RefinementData sym bak t ext argTys wptr
  , setupToConcretize :: C.GlobalVar ToConc.ToConcretizeType
  , setupSetupMem :: Setup.SetupMem sym
  , setupInitializedFs :: GSIO.InitializedFs sym wptr
  , setupArgs :: Setup.Args sym ext argTys wptr
  , setupErrorCallbacks :: EC.ErrorCallbacks sym t
  }

-- | Set up the initial state for use in 'refineOnce' or trace replay.
-- Handles error-callback setup, argument allocation, and filesystem
-- initialization. Returns a 'RefinementSetup' containing all the data
-- needed to build the initial Crucible state.
--
-- Use 'withErrorCallbacks' with the 'setupErrorCallbacks' field to install
-- the implicit parameters @?recordLLVMAnnotation@ and @?processMacawAssert@
-- before creating memory configurations or other objects that capture them.
setupRefinement ::
  ( CLM.HasPtrWidth wptr
  , C.IsSyntaxExtension ext
  , CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder t st fs
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext wptr
  ) =>
  GreaseLogAction ->
  Opts.FsOpts ->
  C.Timeout ->
  C.HandleAllocator ->
  bak ->
  DataLayout ->
  InitialMem sym ->
  RD.RefinementInputs sym bak ext argTys ->
  IO (RefinementSetup sym bak t ext argTys wptr)
setupRefinement la fsOpts solverTimeout halloc bak dl initMem inputs = do
  let argShapes = RD.refineInputArgShapes inputs
  let valueNames = RD.refineInputArgNames inputs
  let argTys = RD.refineInputArgTypes inputs
  let sym = CB.backendGetSym bak
  callbacks <- EC.buildErrMaps
  EC.withErrorCallbacks callbacks $ do
    (args, setupMem, setupAnns) <-
      Setup.setup la bak dl valueNames argTys argShapes initMem
    initFs_ <- GSIO.initialLlvmFileSystem halloc sym fsOpts
    (toConc, globals1) <- liftIO $ ToConc.newToConcretize halloc (GSIO.initFsGlobals initFs_)
    let initFs = initFs_{GSIO.initFsGlobals = globals1}
    let concInitState =
          Conc.InitialState
            { Conc.initStateArgs = args
            , Conc.initStateFs = GSIO.initFsContents initFs
            , Conc.initStateMem = initMem
            }
    let refineData =
          RD.RefinementData
            { RD.refineInputs = inputs
            , RD.refineAnns = setupAnns
            , RD.refineInitState = concInitState
            , RD.refineSolverTimeout = solverTimeout
            , RD.refineErrMap = EC.errorMap callbacks
            }
    pure
      RefinementSetup
        { setupRefinementData = refineData
        , setupToConcretize = toConc
        , setupSetupMem = setupMem
        , setupInitializedFs = initFs
        , setupArgs = args
        , setupErrorCallbacks = callbacks
        }

-- | Run 'Setup.setup' then 'execAndRefine'. Usually passed to 'refinementLoop'.
refineOnce ::
  ( CLM.HasPtrWidth wptr
  , C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , ToConc.HasToConcretize p
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret argTys wptr
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext wptr
  ) =>
  GreaseLogAction ->
  Opts.SimOpts ->
  C.HandleAllocator ->
  bak ->
  W4FM.FloatModeRepr fm ->
  DataLayout ->
  InitialMem sym ->
  RD.RefinementInputs sym bak ext argTys ->
  [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)] ->
  ( ( MSM.MacawProcessAssertion sym
    , Mem.HasLLVMAnn sym
    ) =>
    RefinementSetup sym bak t ext argTys wptr ->
    IO (CS.ExecState p sym ext (CS.RegEntry sym ret))
  ) ->
  IO (ProveRefineResult sym ext argTys)
refineOnce la simOpts halloc bak fm dl initMem inputs execFeats mkInitState = do
  setup <-
    setupRefinement
      la
      (Opts.simFsOpts simOpts)
      (Opts.simSolverTimeout (Opts.simBoundsOpts simOpts))
      halloc
      bak
      dl
      initMem
      inputs
  st <- EC.withErrorCallbacks (setupErrorCallbacks setup) (mkInitState setup)
  boundsFeats_ <- boundedExecFeats (Opts.simBoundsOpts simOpts)
  let boundsFeats = List.map CS.genericToExecutionFeature boundsFeats_
  let execData =
        ExecData
          { execFeats = execFeats List.++ boundsFeats
          , execInitState = st
          , execPathStrat = Opts.simPathStrategy simOpts
          }
  execAndRefine bak fm la execData

data RefinementSummary sym ext tys
  = RefinementSuccess (ArgShapes ext NoTag tys)
  | RefinementNoHeuristic (NE.NonEmpty (NoHeuristic sym ext tys))
  | RefinementItersExceeded
  | RefinementCantRefine CantRefine
  | RefinementBug Bug.BugInstance (ConcretizedData sym ext tys)

refinementLoop ::
  forall sym ext argTys w.
  ( C.IsSyntaxExtension ext
  , 16 C.<= w
  , CLM.HasPtrWidth w
  , MC.MemWidth w
  , ExtShape ext ~ PtrShape ext w
  , PrettyExt ext 'Precond NoTag
  ) =>
  GreaseLogAction ->
  BoundsOpts ->
  Ctx.Assignment ValueName argTys ->
  ArgShapes ext NoTag argTys ->
  -- | This callback is usually 'refineOnce'
  (ArgShapes ext NoTag argTys -> IO (ProveRefineResult sym ext argTys)) ->
  IO (RefinementSummary sym ext argTys)
refinementLoop la boundsOpts argNames initArgShapes go = do
  let
    loop ::
      Int ->
      ArgShapes ext NoTag argTys ->
      IO (RefinementSummary sym ext argTys)
    loop iters argShapes = do
      if Maybe.maybe False (iters >=) (Opts.simMaxIters boundsOpts)
        then do
          doLog la Diag.RefinementLoopMaximumIterationsExceeded
          pure RefinementItersExceeded
        else do
          let addrWidth = MC.addrWidthRepr (Proxy @w)
          doLog la (Diag.RefinementUsingPrecondition addrWidth argNames argShapes)
          new <- go argShapes
          case new of
            ProveBug b cData -> pure (RefinementBug b cData)
            ProveCantRefine b -> pure (RefinementCantRefine b)
            ProveRefine argShapes' -> do
              doLog la Diag.RefinementLoopRetrying
              loop (iters + 1) argShapes'
            ProveSuccess -> do
              doLog la Diag.RefinementLoopAllGoalsPassed
              doLog la (Diag.RefinementFinalPrecondition addrWidth argNames argShapes)
              pure $ RefinementSuccess argShapes
            ProveNoHeuristic errs -> do
              doLog la Diag.RefinementLoopNoHeuristic
              pure $ RefinementNoHeuristic errs
  loop 0 initArgShapes
