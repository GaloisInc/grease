{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Screach.GoalEvaluator (
  goalEvaluatorExecFeature,
  TargetFunctionName (..),
  goalEvaluatorMacawExtension,
  TargetAddress (..),
  reachedTargetMsg,
  isReachedTargetError,
  deemPotentiallyReachable,
) where

import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as MS
import Grease.Utility qualified as GU
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.Operations qualified as C
import Lang.Crucible.Simulator.SimError qualified as C
import Lumberjack qualified as LJ
import Screach.AnalysisLoc (ResolvedTargetLoc)
import Screach.Diagnostic (Diagnostic (GoalEvaluatorDiagnostic), ScreachLogAction)
import Screach.GoalEvaluator.Diagnostic as Diag
import What4.Config qualified as W4C
import What4.FunctionName qualified as WFN
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL
import What4.Protocol.Online qualified as WPO
import What4.SatResult qualified as W4

doLog :: MonadIO m => ScreachLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (GoalEvaluatorDiagnostic diag)

-- | The name of the target function in a Crucible S-expression program.
newtype TargetFunctionName = TargetFunctionName WFN.FunctionName

-- | A 'C.GenericExecutionFeature' that intercepts Crucible function calls to
-- detect if a target function has been called. This is only ever needed when
-- analyzing targets with symbol names (e.g., with @--target-symbol@). When
-- analyzing targets with addresses (e.g., with @--target-addr@), @screach@ uses
-- 'goalEvaluatorMacawExtension' instead, which is address-oriented.
goalEvaluatorExecFeature ::
  forall sym bak solver scope st fs w.
  ( GU.OnlineSolverAndBackend solver sym bak scope st fs
  , MC.MemWidth w
  ) =>
  ScreachLogAction ->
  bak ->
  ResolvedTargetLoc w ->
  TargetFunctionName ->
  C.GenericExecutionFeature sym
goalEvaluatorExecFeature la bak rtLoc (TargetFunctionName tgtFnNm) =
  C.GenericExecutionFeature $
    \case
      C.CallState _ rc st
        | tgtFnNm == C.resolvedCallName rc ->
            deemCallPotentiallyReachable st
      C.TailCallState _ rc st
        | tgtFnNm == C.resolvedCallName rc ->
            deemCallPotentiallyReachable st
      _ ->
        pure C.ExecutionFeatureNoChange
 where
  deemCallPotentiallyReachable ::
    C.SimState p sym ext rtp f args ->
    IO (C.ExecutionFeatureResult p sym ext rtp)
  deemCallPotentiallyReachable st = do
    let sym = CB.backendGetSym bak
    let goal = WI.truePred sym
    mbAbortExecRsn <- deemPotentiallyReachable la bak rtLoc st goal
    pure $
      case mbAbortExecRsn of
        Just abortExecRsn ->
          C.ExecutionFeatureNewState $ C.AbortState abortExecRsn st
        Nothing -> C.ExecutionFeatureNoChange

-- | The address of a target location in a binary.
newtype TargetAddress w = TargetAddress {getTargetAddress :: MC.MemWord w}

-- | A custom 'C.ExtensionImpl' that augments the interpretation of
-- 'MS.MacawInstructionStart' to detect if a 'TargetAddress' has been reached.
-- This is only ever needed when analyzing targets with addresses (e.g., with
-- @--target-addr@). Targets with symbols (e.g., with @--target-symbol@) use a
-- different mechanism ('goalEvaluatorExecFeature') to detect target
-- reachability.
goalEvaluatorMacawExtension ::
  ( w ~ MC.ArchAddrWidth arch
  , MC.MemWidth w
  , 1 CCC.<= w
  , GU.OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  C.ExtensionImpl p sym (MS.MacawExt arch) ->
  ScreachLogAction ->
  bak ->
  MC.Memory w ->
  ResolvedTargetLoc w ->
  TargetAddress w ->
  C.ExtensionImpl p sym (MS.MacawExt arch)
goalEvaluatorMacawExtension ext la bak mem rtLoc tgtAddr =
  C.ExtensionImpl
    { C.extensionEval = C.extensionEval ext
    , C.extensionExec = goalEvaluatorExtensionExec (C.extensionExec ext) la bak mem rtLoc tgtAddr
    }

-- | See the Haddocks for 'goalEvaluatorMacawExtension'.
goalEvaluatorExtensionExec ::
  ( w ~ MC.ArchAddrWidth arch
  , MC.MemWidth w
  , GU.OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  C.EvalStmtFunc p sym (MS.MacawExt arch) ->
  ScreachLogAction ->
  bak ->
  MC.Memory w ->
  ResolvedTargetLoc w ->
  TargetAddress w ->
  C.EvalStmtFunc p sym (MS.MacawExt arch)
goalEvaluatorExtensionExec inner la bak mem rtLoc (TargetAddress tgtAddr) stmt st = do
  case stmt of
    MS.MacawInstructionStart segOff addr _asm -> do
      let instAddr = GU.segoffToAbsoluteAddr mem segOff + addr
      Monad.when (tgtAddr == instAddr) $ do
        let sym = CB.backendGetSym bak
        let goal = WI.truePred sym
        mbAbortExecRsn <- deemPotentiallyReachable la bak rtLoc st goal
        traverse_ @Maybe CB.abortExecBecause mbAbortExecRsn
    _ -> pure ()
  inner stmt st

-- | Deem this path as /potentially/ reachable by asserting a
-- reachability-related assertion. @screach@ will recognize the error message in
-- the assertion failure as having reached the target.
--
-- In addition, check whether this path is /definitely/ reachable by consulting
-- an online solver backend. If this path is definitely reachable, then call
-- sites should take measures to abort execution early (the precise mechanism
-- for doing so depends on the use case) by using the returned 'C.AbortReason'.
deemPotentiallyReachable ::
  forall w solver sym bak scope st fs p ext rtp f a.
  ( MM.MemWidth w
  , GU.OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  ScreachLogAction ->
  bak ->
  ResolvedTargetLoc w ->
  C.SimState p sym ext rtp f a ->
  -- | Predicate to assert, 'W4.truePred' is a good default
  WI.Pred sym ->
  IO (Maybe CB.AbortExecReason)
deemPotentiallyReachable la bak rtLoc st goal = do
  let sym = CB.backendGetSym bak
  let mbPloc = st Lens.^. C.stateLocation
  loc <- WI.getCurrentProgramLoc sym
  notGoal <- WI.notPred sym goal
  definitelyReachable <- CBO.withSolverProcess bak (pure False) $ \proc ->
    do
      let locDesc = case mbPloc of
            Just ploc -> show (WPL.plSourceLoc ploc)
            Nothing -> "(unknown location)"
      let rsn = "goal sat: " ++ locDesc
      result <- WPO.checkSatisfiable proc rsn goal
      doLog la (Diag.ReachedTarget rtLoc result)
      pure $ W4.isSat result
  let simError = C.GenericSimError reachedTargetMsg
  assertWithoutAssumptions sym notGoal simError
  pure $
    if definitelyReachable
      then Just $ CB.AssertionFailure $ C.SimError loc simError
      else Nothing
 where
  assertWithoutAssumptions :: sym -> WI.Pred sym -> C.SimErrorReason -> IO ()
  assertWithoutAssumptions sym notGoal simError = do
    -- TODO(internal#127): We need a better way to do this.. i dont see any way that doesnt involve a change
    -- to crucible. We can pass an explicit false here which if assumed results in a thrown exception
    -- so we dont do assumes for specifically this obligation.
    let cfg = WI.getConfiguration sym
    assertThenAssume <- W4C.getOptionSetting CB.assertThenAssumeConfigOption cfg
    orig <- W4C.getOpt assertThenAssume
    _ <- W4C.setOpt assertThenAssume False
    CB.assert bak notGoal simError
    _ <- W4C.setOpt assertThenAssume orig
    pure ()

-- | The message that is displayed when the target has been reached. This is put
-- into its own definition so that it can be referenced elsewhere.
reachedTargetMsg :: String
reachedTargetMsg = "reached target"

isReachedTargetError :: C.SimError -> Bool
isReachedTargetError simErr =
  reachedTargetMsg
    `List.isPrefixOf` C.simErrorReasonMsg (C.simErrorReason simErr)
