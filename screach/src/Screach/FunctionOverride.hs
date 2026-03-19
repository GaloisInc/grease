{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Additional @screach@-specific function overrides to add the the default set
-- of overrides that @grease@ provides.
module Screach.FunctionOverride (customScreachOverrides) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as State
import Data.Foldable (traverse_)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as MS
import Data.Parameterized.Context qualified as Ctx
import Data.Sequence qualified as Seq
import Grease.Macaw.Arch qualified as Arch
import Grease.Utility qualified as GU
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as CT
import Screach.AnalysisLoc (ResolvedTargetLoc (ResolvedTargetLocSymbol))
import Screach.Diagnostic (ScreachLogAction)
import Screach.FunctionOverride.Sscanf qualified as SFS
import Screach.GoalEvaluator (deemPotentiallyReachable)
import Screach.Personality qualified as SP
import Stubs.FunctionOverride qualified as StubsF

-- | Additional @screach@-specific function overrides to add the the default set
-- of overrides that @grease@ provides.
customScreachOverrides ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  , p' ~ SP.ScreachSimulatorState p sym bak (MS.MacawExt arch) arch t ret aty w
  ) =>
  ScreachLogAction ->
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p' sym arch CLM.Mem ->
  Arch.ArchContext arch ->
  Seq.Seq (StubsF.SomeFunctionOverride p' sym arch)
customScreachOverrides la _fs memVar mmConf archCtx =
  SFS.sscanfFamilyOverrides memVar mmConf archCtx
    <> Seq.singleton (StubsF.SomeFunctionOverride (reachedOverride la))

-- | The @reached@ override
reachedOverride ::
  ( CLM.HasPtrWidth w
  , MC.MemWidth w
  ) =>
  ScreachLogAction ->
  StubsF.FunctionOverride p' sym (Ctx.EmptyCtx Ctx.::> CT.BoolType) arch CT.UnitType
reachedOverride la =
  StubsF.mkFunctionOverride "reached" $ \bak args ->
    Ctx.uncurryAssignment (reached la bak) args

-- | Implementation of the @reached@ override
reached ::
  forall sym bak w p ext r args ret solver scope st fs.
  ( GU.OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth w
  , MC.MemWidth w
  ) =>
  ScreachLogAction ->
  bak ->
  CS.RegEntry sym CT.BoolType ->
  CS.OverrideSim p sym ext r args ret ()
reached la bak regEnt = do
  let CS.RegEntry _ b = regEnt
  st <- State.get
  let tgtLoc = ResolvedTargetLocSymbol "reached" Nothing
  mbAbortExecRsn <- liftIO (deemPotentiallyReachable @w la bak tgtLoc st b)
  traverse_ @Maybe CS.overrideAbort mbAbortExecRsn
