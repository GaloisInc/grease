{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reachability verification
--
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.Verify (verifyReachable) where

import Control.Lens ((&), (.~))
import Control.Monad qualified as Monad
import Data.Parameterized.TraversableFC qualified as TFC
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Diagnostic (Diagnostic (ReachabilityVerifyDiagnostic), GreaseLogAction)
import Grease.Reachability.GoalEvaluator (isReachedTargetError)
import Grease.Reachability.Verify.Diagnostic qualified as Diag
import Grease.Scheduler qualified as Sched
import Grease.Shape (ArgShapes (ArgShapes))
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrDataMode (Precond))
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Extension (IsSyntaxExtension)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lumberjack qualified as LJ
import What4.Interface qualified as WI
import What4.InterpretedFloatingPoint qualified as WIF

-- | Re-run simulation for each 'ConcretizedData', using the concrete argument
-- shapes and the recorded trace to confirm the target is reachable. Logs
-- verification outcome.
verifyReachable ::
  ( TFC.TraversableFC (Shape.ExtShape ext 'Precond)
  , IsSyntaxExtension ext
  , WI.IsExprBuilder sym
  , WIF.IsInterpretedFloatSymExprBuilder sym
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  ) =>
  GreaseLogAction ->
  [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)] ->
  -- | Create an initial execution state from concrete argument shapes.
  (Shape.ArgShapes ext NoTag argTys -> IO (CS.ExecState p sym ext (CS.RegEntry sym ret))) ->
  [ConcretizedData sym ext argTys] ->
  IO ()
verifyReachable la execFeats initShape cDatas =
  Monad.forM_ (zip [1 ..] cDatas) $ \(no, cData) -> do
    LJ.writeLog la (ReachabilityVerifyDiagnostic (Diag.VerifyReachable (length cDatas) no))
    let concArgShapes = Conc.concArgsShapes (Conc.concArgs cData)
        untagArgs = TFC.fmapFC (TFC.fmapFC (const NoTag)) concArgShapes
    st <- initShape (ArgShapes untagArgs)
    let trace = Conc.concTrace cData
        st' =
          st
            & Sched.execStateContextLens
              . CS.cruciblePersonality
              . CR.replayState
              . CR.initialTrace
            .~ trace
    -- TODO(#639): Incorporate the concretized filesystem.
    result <- CS.executeCrucible (CR.replayFeature True : execFeats) st'
    LJ.writeLog la $
      ReachabilityVerifyDiagnostic $
        if targetReached result then Diag.VerifySuccess else Diag.VerifyFailure
 where
  targetReached :: CS.ExecResult p sym ext (CS.RegEntry sym ret) -> Bool
  targetReached = \case
    CS.FinishedResult _ pr -> case pr of
      CS.PartialRes _ _ _ aborted -> goAborted aborted
      CS.TotalRes{} -> False
    CS.AbortedResult _ aborted -> goAborted aborted
    CS.TimeoutResult{} -> False

  goAborted :: CS.AbortedResult sym ext -> Bool
  goAborted = \case
    CS.AbortedExec (CB.AssertionFailure simErr) _ -> isReachedTargetError simErr
    CS.AbortedBranch _ _ r1 r2 -> goAborted r1 || goAborted r2
    _ -> False
