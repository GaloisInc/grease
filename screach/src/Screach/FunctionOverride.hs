{-# LANGUAGE OverloadedStrings #-}

-- | Additional @screach@-specific function overrides to add the the default set
-- of overrides that @grease@ provides.
module Screach.FunctionOverride (customScreachOverrides) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as State
import Data.Foldable (traverse_)
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.Context qualified as Ctx
import Data.Sequence qualified as Seq
import Grease.Utility qualified as GU
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as CT
import Screach.AnalysisLoc (ResolvedTargetLoc (ResolvedTargetLocSymbol))
import Screach.Diagnostic (ScreachLogAction)
import Screach.GoalEvaluator (deemPotentiallyReachable)
import Stubs.FunctionOverride qualified as StubsF

-- | Additional @screach@-specific function overrides to add the the default set
-- of overrides that @grease@ provides.
customScreachOverrides ::
  ( CLM.HasPtrWidth w
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  ) =>
  ScreachLogAction ->
  Seq.Seq (StubsF.SomeFunctionOverride p' sym arch)
customScreachOverrides la =
  Seq.singleton (StubsF.SomeFunctionOverride (reachedOverride la))

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
