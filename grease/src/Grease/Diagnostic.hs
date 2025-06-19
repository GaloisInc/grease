{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}

-- | Functionality for logging diagnostic messages in @grease@.
module Grease.Diagnostic
  ( Diagnostic(..)
  , GreaseLogAction
  , Grease.Diagnostic.log
  , severity
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Grease.BranchTracer.Diagnostic qualified as BranchTracer
import Grease.Diagnostic.Severity (Severity)
import Grease.Heuristic.Diagnostic qualified as Heuristic
import Grease.LLVM.Overrides.Diagnostic qualified as LLVMOverrides
import Grease.LLVM.SetupHook.Diagnostic qualified as LLVMSetupHook
import Grease.LLVM.SimulatorHooks.Diagnostic qualified as LLVMSimulatorHooks
import Grease.Macaw.Load.Diagnostic qualified as Load
import Grease.Macaw.ResolveCall.Diagnostic qualified as ResolveCall
import Grease.Macaw.SimulatorHooks.Diagnostic qualified as SimulatorHooks
import Grease.Main.Diagnostic qualified as Main
import Grease.Refine.Diagnostic qualified as Refine
import Grease.Setup.Diagnostic qualified as Setup
import Grease.Skip.Diagnostic qualified as Skip
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.IO (stderr)

-- | A diagnostic message that @grease@ can emit.
--
-- Diagnostics for each module are defined in a sub-module. Each diagnostic
-- type implements 'PP.Pretty'.
data Diagnostic where
  BranchTracerDiagnostic :: BranchTracer.Diagnostic -> Diagnostic
  HeuristicDiagnostic :: Heuristic.Diagnostic -> Diagnostic
  LLVMOverridesDiagnostic :: LLVMOverrides.Diagnostic -> Diagnostic
  LLVMSetupHookDiagnostic :: LLVMSetupHook.Diagnostic -> Diagnostic
  LLVMSimulatorHooksDiagnostic :: LLVMSimulatorHooks.Diagnostic -> Diagnostic
  LoadDiagnostic :: Load.Diagnostic -> Diagnostic
  MainDiagnostic :: Main.Diagnostic -> Diagnostic
  RefineDiagnostic :: Refine.Diagnostic -> Diagnostic
  ResolveCallDiagnostic :: ResolveCall.Diagnostic -> Diagnostic
  SetupDiagnostic :: Setup.Diagnostic -> Diagnostic
  SimulatorHooksDiagnostic :: SimulatorHooks.Diagnostic -> Diagnostic
  SkipDiagnostic :: Skip.Diagnostic -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      BranchTracerDiagnostic diag -> PP.pretty diag
      HeuristicDiagnostic diag -> PP.pretty diag
      LLVMOverridesDiagnostic diag -> PP.pretty diag
      LLVMSetupHookDiagnostic diag -> PP.pretty diag
      LLVMSimulatorHooksDiagnostic diag -> PP.pretty diag
      LoadDiagnostic diag -> PP.pretty diag
      MainDiagnostic diag -> PP.pretty diag
      RefineDiagnostic diag -> PP.pretty diag
      ResolveCallDiagnostic diag -> PP.pretty diag
      SetupDiagnostic diag -> PP.pretty diag
      SimulatorHooksDiagnostic diag -> PP.pretty diag
      SkipDiagnostic diag -> PP.pretty diag

severity :: Diagnostic -> Severity
severity =
  \case
    BranchTracerDiagnostic diag -> BranchTracer.severity diag
    HeuristicDiagnostic diag -> Heuristic.severity diag
    LLVMOverridesDiagnostic diag -> LLVMOverrides.severity diag
    LLVMSetupHookDiagnostic diag -> LLVMSetupHook.severity diag
    LLVMSimulatorHooksDiagnostic diag -> LLVMSimulatorHooks.severity diag
    LoadDiagnostic diag -> Load.severity diag
    MainDiagnostic diag -> Main.severity diag
    RefineDiagnostic diag -> Refine.severity diag
    ResolveCallDiagnostic diag -> ResolveCall.severity diag
    SetupDiagnostic diag -> Setup.severity diag
    SimulatorHooksDiagnostic diag -> SimulatorHooks.severity diag
    SkipDiagnostic diag -> Skip.severity diag

-- | The type of @grease@ 'LJ.LogAction's. This must work over any 'MonadIO'
-- instance to ensure that we can log messages in multiple monads, including
-- 'IO' and @'StateT' s 'IO'@.
type GreaseLogAction = forall m. MonadIO m => LJ.LogAction m Diagnostic

-- | Log a message to 'stderr' along with the current time.
log :: MonadIO m => PP.Doc a -> m ()
log msg = do
  liftIO $ PP.hPutDoc stderr msg
  liftIO $ PP.hPutDoc stderr PP.line
