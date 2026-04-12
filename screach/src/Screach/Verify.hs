{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

-- | Reachability verification
module Screach.Verify (verifyReachable) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad qualified as Monad
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.TraversableFC qualified as TFC
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (GreaseLogAction)
import Grease.Personality qualified as GP
import Grease.Refine qualified as GR
import Grease.Scheduler qualified as Sched
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag qualified as Shape
import Grease.Shape.Pointer qualified as Shape
import Grease.Utility qualified as GU
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Extension qualified as CCE
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.ExecutionTree qualified as CSE
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types qualified as CT
import Lumberjack qualified as LJ
import Screach.Diagnostic (Diagnostic (VerifyDiagnostic), ScreachLogAction)
import Screach.Heuristic (isReachedBug)
import Screach.Verify.Diagnostic qualified as Diag
import What4.Expr qualified as WE

doLog :: ScreachLogAction -> Diag.Diagnostic -> IO ()
doLog la diag = LJ.writeLog la (VerifyDiagnostic diag)

-- | Re-run simulation for each 'Conc.ConcretizedData', using the concrete
-- argument shapes and the recorded trace to confirm the target is reachable.
-- Logs verification outcome.
verifyReachable ::
  forall p sym bak cExt ext t ret tys w solver st fm.
  ( TFC.TraversableFC (Shape.ExtShape ext 'Shape.Precond)
  , CCE.IsSyntaxExtension ext
  , GU.OnlineSolverAndBackend solver sym bak t st (WE.Flags fm)
  , 16 CT.<= w
  , CLM.HasPtrWidth w
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  , Shape.ExtShape ext ~ Shape.PtrShape ext w
  , MC.MemWidth w
  , CR.HasReplayState p p sym ext (CS.RegEntry sym ret)
  , CR.HasRecordState p p sym ext (CS.RegEntry sym ret)
  , GP.HasPersonality p sym bak t cExt ext ret tys w
  ) =>
  ScreachLogAction ->
  GreaseLogAction ->
  bak ->
  (Shape.ArgShapes ext Shape.NoTag tys -> IO (CS.ExecState p sym ext (CS.RegEntry sym ret))) ->
  [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)] ->
  [Conc.ConcretizedData sym ext tys] ->
  IO ()
verifyReachable la gla bak initShape execFeats cDataList =
  Monad.forM_ (zip [1 ..] cDataList) $ \(no, cData) -> do
    doLog la (Diag.VerifyReachable (length cDataList) no)
    let concArgShapes = Conc.concArgsShapes (Conc.concArgs cData)
        untagArgs = TFC.fmapFC (TFC.fmapFC (const Shape.NoTag)) concArgShapes
    -- TODO(#640): Incorporate the concretized filesystem.
    st <- initShape (Shape.ArgShapes untagArgs)
    let trace = Conc.concTrace cData
        st' =
          st
            & Sched.execStateContextLens
              . CS.cruciblePersonality
              . CR.replayState
              . CR.initialTrace
              .~ trace
    result' <- CS.executeCrucible (CR.replayFeature True : CR.recordFeature : execFeats) st'
    let ctx = CSE.execResultContext result'
    let pers = ctx ^. CS.cruciblePersonality
    let refineData = pers ^. GP.personality . GP.pRefinementData
    obls <- CB.getProofObligations bak
    refResult <- GR.proveAndRefine bak result' gla refineData obls
    case refResult of
      GR.ProveBug bugInstance _
        | isReachedBug bugInstance ->
            doLog la Diag.VerifySuccess
      _ -> doLog la Diag.VerifyFailure
