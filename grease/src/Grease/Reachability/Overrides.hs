{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reachability overrides
--
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.Overrides (
  reachabilityBuiltinOverrides,
  reachedOverride,
  reached,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State qualified as State
import Data.Foldable (traverse_)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Parameterized.Context qualified as Ctx
import Data.Sequence qualified as Seq
import Grease.Diagnostic (GreaseLogAction)
import Grease.Reachability.AnalysisLoc (ResolvedTargetLoc (ResolvedTargetLocSymbol))
import Grease.Reachability.GoalEvaluator (deemPotentiallyReachable)
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as CT
import Stubs.FunctionOverride qualified as Stubs

-- | Built-in stubs overrides for reachability analysis. This includes the
-- @\@reached@ override, which is called by target overrides to signal that
-- the target has been reached.
reachabilityBuiltinOverrides ::
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  GreaseLogAction ->
  Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
reachabilityBuiltinOverrides la =
  Seq.singleton (Stubs.SomeFunctionOverride (reachedOverride la))

-- | The @reached@ override.
--
-- This override is called by target overrides to declare a path as potentially
-- reachable. If the @Bool@ argument is @true@, then the current path is deemed
-- reachable.
reachedOverride ::
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  GreaseLogAction ->
  Stubs.FunctionOverride p sym (Ctx.EmptyCtx Ctx.::> CT.BoolType) arch CT.UnitType
reachedOverride la =
  Stubs.mkFunctionOverride "reached" $ \bak args ->
    Ctx.uncurryAssignment (reached la bak) args

-- | Implementation of the @reached@ override.
reached ::
  forall sym bak w p ext r args ret solver scope st fs.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth w
  , MM.MemWidth w
  ) =>
  GreaseLogAction ->
  bak ->
  CS.RegEntry sym CT.BoolType ->
  CS.OverrideSim p sym ext r args ret ()
reached la bak regEnt = do
  let CS.RegEntry _ b = regEnt
  st <- State.get
  let tgtLoc = ResolvedTargetLocSymbol "reached" Nothing
  mbAbortExecRsn <- liftIO (deemPotentiallyReachable @w la bak tgtLoc st b)
  traverse_ @Maybe CS.overrideAbort mbAbortExecRsn
