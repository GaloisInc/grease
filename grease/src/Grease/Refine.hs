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
  RefinementData (..),
  execAndRefine,
  RefinementSummary (..),
  refinementLoop,
  buildErrMaps,
  ErrorCallbacks (..),
) where

import Control.Applicative (pure)
import Control.Exception.Safe (MonadThrow, throw)
import Control.Exception.Safe qualified as X
import Control.Lens ((^.))
import Control.Monad qualified as Monad
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Const (Const)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.IORef qualified as IORef
import Data.Int (Int)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (..), maybeToList)
import Data.Maybe qualified as Maybe
import Data.Monoid (mconcat)
import Data.Ord ((>=))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce (Nonce)
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (String)
import Data.Text qualified as Text
import Data.Type.Equality (type (~))
import GHC.IORef (newIORef)
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic
import Grease.Heuristic
import Grease.Options (BoundsOpts)
import Grease.Options qualified as Opts
import Grease.Panic (panic)
import Grease.Refine.Diagnostic qualified as Diag
import Grease.Setup.Annotations qualified as Anns
import Grease.Shape (ArgShapes, ExtShape, PrettyExt)
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Pointer (PtrShape)
import Grease.Solver (Solver, solverAdapter)
import Grease.Utility
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Prove qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.Partial qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.PathSplitting qualified as C
import Lang.Crucible.Simulator.SimError qualified as C
import Lang.Crucible.Utils.Timeout qualified as C
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import System.Exit qualified as Exit
import System.IO (IO)
import What4.Expr qualified as W4
import What4.Expr.App qualified as W4
import What4.FloatMode qualified as W4FM
import What4.Interface qualified as W4
import What4.LabeledPred qualified as W4
import What4.Solver qualified as W4
import Prelude (Num (..))

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (RefineDiagnostic diag)

-- | Find annotations on a term and its subterms, all of type 'W4.BaseBoolType'.
--
-- Similar to 'Grease.Setup.Annotations.findAnnotation', but only for
-- annotations on predicates.
findPredAnnotations ::
  forall sym brand st fs t.
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  ) =>
  sym ->
  W4.SymExpr sym t ->
  [W4.SymAnnotation sym W4.BaseBoolType]
findPredAnnotations sym e = case W4.asApp e of
  Just app -> do
    let anns :: [W4.SymAnnotation sym W4.BaseBoolType]
        anns = MC.foldMapFC (maybeToList . getPredAnn) app
    case getPredAnn e of
      Nothing -> anns
      Just ann -> ann : anns
  Nothing -> maybeToList (getPredAnn e)
 where
  getPredAnn ::
    forall tp.
    W4.Expr brand tp ->
    Maybe (W4.SymAnnotation sym W4.BaseBoolType)
  getPredAnn expr =
    case W4.exprType expr of
      W4.BaseBoolRepr -> W4.getAnnotation sym expr
      _ -> Nothing

-- | A proof obligation failed, and no heuristic identified a refinement
data NoHeuristic sym ext tys
  = NoHeuristic
  { noHeuristicGoal :: C.ProofObligation sym
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

data ErrorCallbacks sym t
  = ErrorCallbacks
  { llvmErrCallback :: LLCS.CallStack -> Mem.BoolAnn sym -> CLLVM.BadBehavior sym -> IO ()
  , macawAssertionCallback :: sym -> W4.Pred sym -> MSM.MacawError sym -> IO (W4.Pred sym)
  , errorMap :: IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym))
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
buildErrMaps :: sym ~ W4.ExprBuilder t st fs => IO (ErrorCallbacks sym t)
buildErrMaps = do
  bbMapRef <- newIORef Map.empty
  let recordLLVMAnnotation = \callStack (Mem.BoolAnn ann) bb ->
        modifyIORef bbMapRef $ Map.insert ann (CrucibleLLVMError bb callStack)
  let processMacawAssert = \sym p err -> do
        (ann, p') <- W4.annotateTerm sym p
        _ <- modifyIORef bbMapRef $ Map.insert ann (MacawMemError err)
        pure p'
  pure
    ErrorCallbacks
      { errorMap = bbMapRef
      , llvmErrCallback = recordLLVMAnnotation
      , macawAssertionCallback = processMacawAssert
      }

-- | How to consume the results of trying to prove a goal. Not exported.
consumer ::
  forall p ext r solver sym bak t st argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  Anns.Annotations sym ext argTys ->
  C.ExecResult p sym ext r ->
  GreaseLogAction ->
  [RefineHeuristic sym bak ext argTys] ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym) ->
  Conc.InitialState sym ext argTys ->
  C.ProofConsumer sym t (ProveRefineResult sym ext argTys)
consumer bak anns execResult la heuristics argNames argShapes bbMap initState =
  C.ProofConsumer $ \goal result -> do
    let sym = C.backendGetSym bak
    let lp = C.proofGoal goal
    let simErr = lp ^. W4.labeledPredMsg
    minfo <-
      case findPredAnnotations sym (lp ^. W4.labeledPred) of
        [] -> do
          -- See gitlab#139, this warning is not fixable by users and already
          -- has an associated issue, so we filter it out. See also
          -- https://github.com/GaloisInc/macaw/issues/429 for a proposal to
          -- improve macaw-symbolic's assertion tracking so that we can
          -- intercept this properly.
          Monad.unless
            ( "PointerRead outside of static memory range"
                `List.isPrefixOf` C.simErrorReasonMsg (C.simErrorReason simErr)
            )
            $ doLog la Diag.NoAnnotationOnPredicate
          pure Nothing
        (ann : _) -> case Map.lookup ann bbMap of
          Nothing ->
            throw . GreaseException $ "Predicate annotation was not found in bad behavior map: " <> tshow ann
          info -> pure info
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
      C.Unknown{} ->
        throw . GreaseException . tshow $
          mconcat
            [ "Received unknown result from solver for goal:"
            , PP.line
            , PP.indent 4 (C.ppSimError simErr)
            ]

-- | Helper, not exported
execCfg ::
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  Opts.PathStrategy ->
  [C.ExecutionFeature p sym ext (C.RegEntry sym ret)] ->
  C.ExecState p sym ext (C.RegEntry sym ret) ->
  -- | The result of a single execution (in 'Opts.Dfs' mode, of a single path),
  -- the proof obligations resulting from that execution, and any suspended
  -- paths.
  IO
    ( C.ExecResult p sym ext (C.RegEntry sym ret)
    , C.ProofObligations sym
    , Seq (C.WorkItem p sym ext (C.RegEntry sym ret))
    )
execCfg bak strat execFeats initialState = do
  let withCleanup action = X.finally action $ do
        C.resetAssumptionState bak
        C.clearProofObligations bak
  withCleanup $
    case strat of
      Opts.Dfs -> do
        resultRef <- IORef.newIORef Nothing
        (_n, rest) <- C.executeCrucibleDFSPaths execFeats initialState $ \r -> do
          IORef.writeIORef resultRef (Just r)
          pure False -- don't continue exploring other paths
        mbResult <- IORef.readIORef resultRef
        case mbResult of
          Nothing -> panic "execCfg" ["executeCrucibleDFSPaths didn't return a result"]
          Just r -> do
            o <- C.getProofObligations bak
            pure (r, o, rest)
      Opts.Sse -> do
        r <- C.executeCrucible execFeats initialState
        o <- C.getProofObligations bak
        pure (r, o, Seq.empty)

-- | Data needed for refinement
data RefinementData sym bak ext argTys
  = RefinementData
  { refineAnns :: Anns.Annotations sym ext argTys
  , refineArgNames :: Ctx.Assignment (Const String) argTys
  , refineArgShapes :: ArgShapes ext NoTag argTys
  , refineHeuristics :: [RefineHeuristic sym bak ext argTys]
  , refineInitState :: Conc.InitialState sym ext argTys
  }

-- | Helper, not exported
proveAndRefine ::
  forall p ext r solver sym bak t st argTys w fm.
  ( C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st fm
  , 16 C.<= w
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  Solver ->
  -- | Solver timeout
  C.Timeout ->
  C.ExecResult p sym ext r ->
  GreaseLogAction ->
  Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym) ->
  RefinementData sym bak ext argTys ->
  C.ProofObligations sym ->
  IO (ProveRefineResult sym ext argTys)
proveAndRefine bak solver tout execResult la bbMap refineData goals = do
  let RefinementData
        { refineAnns = anns
        , refineArgNames = argNames
        , refineArgShapes = argShapes
        , refineHeuristics = heuristics
        , refineInitState = initState
        } = refineData
  let sym = C.backendGetSym bak
  let prover = C.offlineProver tout sym W4.defaultLogData (solverAdapter solver)
  let strat = C.ProofStrategy prover combiner
  let cons = consumer bak anns execResult la heuristics argNames argShapes bbMap initState
  case goals of
    Nothing -> pure ProveSuccess
    Just goals' ->
      liftIO (runExceptT (C.proveGoals strat goals' cons))
        Monad.>>= \case
          Left C.TimedOut ->
            throw (GreaseException "Timeout when solving goal!")
          Right r -> pure r

execAndRefine ::
  forall ext solver sym bak t st argTys ret w m fm p.
  ( MonadIO m
  , MonadThrow m
  , C.IsSyntaxExtension ext
  , OnlineSolverAndBackend solver sym bak t st (W4.Flags fm)
  , 16 C.<= w
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , ExtShape ext ~ PtrShape ext w
  ) =>
  bak ->
  Solver ->
  W4FM.FloatModeRepr fm ->
  GreaseLogAction ->
  C.GlobalVar Mem.Mem ->
  RefinementData sym bak ext argTys ->
  IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym)) ->
  BoundsOpts ->
  Opts.PathStrategy ->
  [C.ExecutionFeature p sym ext (C.RegEntry sym ret)] ->
  C.ExecState p sym ext (C.RegEntry sym ret) ->
  m (ProveRefineResult sym ext argTys)
execAndRefine bak solver _fm la memVar refineData bbMapRef boundsOpts strat execFeats initialState = do
  let solverTimeout = Opts.simSolverTimeout boundsOpts
  let refineOne initSt = do
        (execResult, goals, remaining) <- execCfg bak strat execFeats initSt
        doLog la (Diag.ExecutionResult memVar execResult)
        refineResult <-
          case execResult of
            C.TimeoutResult _execState ->
              pure (ProveCantRefine Timeout)
            C.AbortedResult _ (C.AbortedExit Exit.ExitSuccess _) ->
              pure (ProveCantRefine (Exit (Just 0)))
            C.AbortedResult _ (C.AbortedExit (Exit.ExitFailure code) _) ->
              pure (ProveCantRefine (Exit (Just code)))
            C.AbortedResult _ (C.AbortedExec (C.EarlyExit _loc) _gp) ->
              pure (ProveCantRefine (Exit Nothing))
            C.AbortedResult _ (C.AbortedExec (C.AssertionFailure (C.SimError _loc (C.ResourceExhausted msg))) _gp) ->
              pure (ProveCantRefine (Exhausted msg))
            C.AbortedResult _ (C.AbortedExec (C.AssertionFailure (C.SimError _loc (C.Unsupported _cs feat))) _gp) ->
              pure (ProveCantRefine (Unsupported feat))
            _ -> do
              bbMap <- readIORef bbMapRef
              proveAndRefine bak solver solverTimeout execResult la bbMap refineData goals
        case strat of
          Opts.Dfs -> do
            loc <- W4.getCurrentProgramLoc (C.backendGetSym bak)
            doLog la (Diag.RefinementFinishedPath loc (shortResult refineResult))
          Opts.Sse -> pure ()
        pure (refineResult, remaining)

  -- Process the state that was passed in
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
                  doLog la (Diag.ResumingFromBranch (C.workItemLoc next))
                  IORef.writeIORef remainingRef rest
                  initSt <- C.restoreWorkItem next
                  (nextRes, additionalPaths) <- refineOne initSt
                  IORef.modifyIORef remainingRef (<> additionalPaths)
                  C.SubgoalResult True <$> go nextRes
            let combine r1 r2 = runExceptT (C.getCombiner combiner r1 r2)
            mbRefineResult <-
              combine (pure firstResult) (liftIO computeNextResult)
            case mbRefineResult of
              Left C.TimedOut ->
                throw (GreaseException "Timeout when solving goal!")
              Right combinedResult -> pure (C.subgoalResult combinedResult)
  liftIO (go refineResult)
 where
  -- Very short summary for single-line log message
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
      ProveCantRefine (Timeout{}) -> "symex timeout"
      ProveCantRefine (Unsupported{}) -> "unsupported feature"

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
  , Mem.HasPtrWidth w
  , MC.MemWidth w
  , ExtShape ext ~ PtrShape ext w
  , PrettyExt ext NoTag
  ) =>
  GreaseLogAction ->
  BoundsOpts ->
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
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
