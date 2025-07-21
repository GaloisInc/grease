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
  execAndRefine,
  RefinementSummary (..),
  refinementLoop,
  buildErrMaps,
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
import Data.Functor (fmap, (<$>))
import Data.Functor.Const (Const)
import Data.IORef (IORef, modifyIORef, readIORef)
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
import Data.String (String)
import Data.Tuple qualified as Tuple
import Data.Type.Equality (type (~))
import GHC.IORef (newIORef)
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic
import Grease.Heuristic
import Grease.Options (LoopBound (..), Milliseconds (..))
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
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.CallStack qualified as Mem
import Lang.Crucible.LLVM.MemModel.Partial qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.BoundedExec qualified as C
import Lang.Crucible.Simulator.BoundedRecursion qualified as C
import Lang.Crucible.Simulator.SimError qualified as C
import Lang.Crucible.Utils.Seconds qualified as C
import Lang.Crucible.Utils.Timeout qualified as C
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import System.IO (IO)
import System.Timeout (timeout)
import What4.Expr qualified as W4
import What4.Expr.App qualified as W4
import What4.FloatMode qualified as W4FM
import What4.Interface qualified as W4
import What4.LabeledPred qualified as W4
import What4.Solver qualified as W4
import Prelude (Num (..), undefined, (=<<))

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
  , noHeuristicError :: Maybe (Mem.BadBehavior sym)
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
    ProveBug{} -> pure r1
    ProveCantRefine{} -> pure r1
    -- if we find a refinement, don't bother with other goals
    ProveRefine{} -> pure r1
    -- if this goal succeeded, continue with the others
    ProveSuccess -> mr2
    ProveNoHeuristic errs1 -> do
      r2 <- mr2
      let failed = C.SubgoalResult False
      case C.subgoalResult r2 of
        -- can't refine further
        ProveBug{} -> pure r2
        ProveCantRefine{} -> pure r2
        -- if we manage to refine the second goal, use that
        ProveRefine{} -> pure r2
        -- otherwise, no heuristic propagates
        ProveSuccess -> pure (failed (ProveNoHeuristic errs1))
        ProveNoHeuristic errs2 ->
          pure (failed (ProveNoHeuristic (errs1 <> errs2)))

extractCLLVMError :: ErrorDescription sym -> Maybe (LLCS.CallStack, CLLVM.BadBehavior sym)
extractCLLVMError (CrucibleLLVMError behavior cs) = Just (cs, behavior)
extractCLLVMError _ = Nothing

type MacawAssertionCallback sym = sym -> W4.Pred sym -> MSM.MacawError sym -> IO (W4.Pred sym)
type LLVMMemModelCallback sym = LLCS.CallStack -> Mem.BoolAnn sym -> CLLVM.BadBehavior sym -> IO ()

buildErrMaps :: forall t st fs bldr. (bldr ~ (W4.ExprBuilder t st fs)) => IO (IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription bldr)), LLVMMemModelCallback bldr, MacawAssertionCallback bldr)
buildErrMaps = do
  bbMapRef <- newIORef Map.empty
  let recordLLVMAnnotation = \callStack (Mem.BoolAnn ann) bb ->
        modifyIORef bbMapRef $ Map.insert ann (CrucibleLLVMError bb callStack)
  let processMacawAssert = \sym p err -> do
        (ann, p') <- W4.annotateTerm sym p
        _ <- modifyIORef bbMapRef $ Map.insert ann (MacawMemError err)
        pure p'
  pure (bbMapRef, recordLLVMAnnotation, processMacawAssert)

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
          info -> pure $ (extractCLLVMError =<< info)
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
            let err = fmap Tuple.snd minfo
             in pure (ProveNoHeuristic (NE.singleton (NoHeuristic goal cData err)))
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
  LoopBound ->
  [C.ExecutionFeature p sym ext (C.RegEntry sym ret)] ->
  C.ExecState p sym ext (C.RegEntry sym ret) ->
  IO (C.ExecResult p sym ext (C.RegEntry sym ret), C.ProofObligations sym)
execCfg bak (LoopBound bound) execFeats initialState = do
  boundExecFeat <- C.boundedExecFeature (\_ -> pure (Just bound)) True
  boundRecFeat <- C.boundedRecursionFeature (\_ -> pure (Just bound)) True
  let execFeats' = List.map C.genericToExecutionFeature [boundExecFeat, boundRecFeat] List.++ execFeats
  X.finally
    ( do
        r <- C.executeCrucible execFeats' initialState
        o <- C.getProofObligations bak
        pure (r, o)
    )
    ( do
        C.resetAssumptionState bak
        C.clearProofObligations bak
    )

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
  Anns.Annotations sym ext argTys ->
  C.ExecResult p sym ext r ->
  GreaseLogAction ->
  [RefineHeuristic sym bak ext argTys] ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  Conc.InitialState sym ext argTys ->
  Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym) ->
  C.ProofObligations sym ->
  IO (ProveRefineResult sym ext argTys)
proveAndRefine bak solver anns execResult la heuristics argNames argShapes initState bbMap goals = do
  -- TODO: Make the timeout configurable at the CLI
  let tout = C.Timeout (C.secondsFromInt 5)
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
  Anns.Annotations sym ext argTys ->
  [RefineHeuristic sym bak ext argTys] ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  Conc.InitialState sym ext argTys ->
  IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym)) ->
  LoopBound ->
  [C.ExecutionFeature p sym ext (C.RegEntry sym ret)] ->
  C.ExecState p sym ext (C.RegEntry sym ret) ->
  m (ProveRefineResult sym ext argTys)
execAndRefine bak solver _fm la anns heuristics argNames argShapes initState bbMapRef (LoopBound bound) execFeats initialState = do
  (result, goals) <- liftIO (execCfg bak (LoopBound bound) execFeats initialState)
  doLog la (Diag.ExecutionResult result)
  bbMap <- liftIO (readIORef bbMapRef)
  liftIO (proveAndRefine bak solver anns result la heuristics argNames argShapes initState bbMap goals)

data RefinementSummary sym ext tys
  = RefinementSuccess (ArgShapes ext NoTag tys)
  | RefinementNoHeuristic (NE.NonEmpty (NoHeuristic sym ext tys))
  | RefinementItersExceeded
  | RefinementCantRefine CantRefine
  | RefinementBug Bug.BugInstance (ConcretizedData sym ext tys)
  | RefinementTimeout

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
  -- | Maximum iterations
  Maybe Int ->
  -- | Timeout
  Milliseconds ->
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  (ArgShapes ext NoTag argTys -> IO (ProveRefineResult sym ext argTys)) ->
  IO (RefinementSummary sym ext argTys)
refinementLoop la maxIters to argNames initArgShapes go = do
  let
    loop ::
      Int ->
      ArgShapes ext NoTag argTys ->
      IO (RefinementSummary sym ext argTys)
    loop iters argShapes = do
      if Maybe.maybe False (iters >=) maxIters
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
  let millisToMicros (Milliseconds millis) = 1000 * millis
  let microTimeout = millisToMicros to
  Maybe.fromMaybe RefinementTimeout
    <$> timeout microTimeout (loop 0 initArgShapes)
