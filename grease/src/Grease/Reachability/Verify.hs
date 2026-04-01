{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

-- | Reachability verification
--
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.Verify (verifyReachable) where

import Control.Lens ((&), (.~))
import Control.Monad qualified as Monad
import Data.Parameterized.TraversableFC qualified as TFC
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (Diagnostic (ReachabilityVerifyDiagnostic), GreaseLogAction)
import Grease.Personality qualified as GP
import Grease.Reachability.Verify.Diagnostic qualified as Diag
import Grease.Refine qualified as GR
import Grease.Scheduler qualified as Sched
import Grease.Shape (ArgShapes (ArgShapes))
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrDataMode (Precond), PtrShape)
import Grease.Utility qualified as GU
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Extension qualified as CCE
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types qualified as CT
import Lumberjack qualified as LJ
import What4.Expr qualified as WE

-- | Re-run simulation for each 'ConcretizedData', using the concrete argument
-- shapes and the recorded trace to confirm the target is reachable. Logs
-- verification outcome.
verifyReachable ::
  forall p sym bak cExt ext t ret tys w solver st fm.
  ( TFC.TraversableFC (Shape.ExtShape ext 'Precond)
  , CCE.IsSyntaxExtension ext
  , GU.OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , 16 CT.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  , Shape.ExtShape ext ~ PtrShape ext w
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret tys w
  ) =>
  GreaseLogAction ->
  bak ->
  [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)] ->
  -- | Create an initial execution state from concrete argument shapes.
  (Shape.ArgShapes ext NoTag tys -> IO (CS.ExecState p sym ext (CS.RegEntry sym ret))) ->
  [ConcretizedData sym ext tys] ->
  IO ()
verifyReachable la bak execFeats initShape cDatas =
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
    result <- CS.executeCrucible (CR.replayFeature True : CR.recordFeature : execFeats) st'
    obls <- CB.getProofObligations bak
    refResult <- GR.proveAndRefine bak result la obls
    LJ.writeLog la $
      ReachabilityVerifyDiagnostic $
        case refResult of
          GR.ProveBug bugInstance _
            | Bug.bugType bugInstance == Bug.ReachedTarget ->
                Diag.VerifySuccess
          _ -> Diag.VerifyFailure
