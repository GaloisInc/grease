{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.SimulatorHooks (
  greaseLlvmExtImpl,
) where

import Control.Lens (set, (^.))
import Control.Monad.IO.Class (MonadIO)
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Grease.Diagnostic (Diagnostic (..), GreaseLogAction)
import Grease.LLVM.SimulatorHooks.Diagnostic qualified as Diag
import Grease.Options (ErrorSymbolicFunCalls (..))
import Grease.Panic (panic)
import Grease.Skip (createSkipOverride)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout qualified as CLLVM
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.Extension qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lumberjack qualified as LJ
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LLVMSimulatorHooksDiagnostic diag)

-- | An 'C.ExtensionImpl' with overrides for the semantics of some
-- @crucible-llvm@ operations.
greaseLlvmExtImpl ::
  ( C.IsSymInterface sym
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  CLLVM.DataLayout ->
  ErrorSymbolicFunCalls ->
  C.ExtensionImpl p sym LLVM ->
  C.ExtensionImpl p sym LLVM
greaseLlvmExtImpl la halloc dl errorSymbolicFunCalls llvmExtImpl =
  llvmExtImpl
    { C.extensionExec =
        extensionExec la halloc dl errorSymbolicFunCalls llvmExtImpl
    }

-- | This evaluates an LLVM statement extension in the simulator, but with
-- overrides for the semantics of certain statements.
extensionExec ::
  ( C.IsSymInterface sym
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  CLLVM.DataLayout ->
  ErrorSymbolicFunCalls ->
  C.ExtensionImpl p sym LLVM ->
  C.EvalStmtFunc p sym LLVM
extensionExec la halloc dl errorSymbolicFunCalls baseExt stmt st =
  case stmt of
    -- LLVM_LoadHandle: This statement is invoked any time a function handle
    -- is resolved before calling the function. We override this because
    -- crucible-llvm's default behavior when resolving a symbolic function
    -- handle is to throw an exception. For consistency with grease's Macaw
    -- frontend, however, we override this so that calling a symbolic function
    -- handle causes the function to be skipped (unless the user overrides
    -- this with --error-symbolic-fun-calls, in which case we fall back to
    -- crucible-llvm's default behavior).
    CLLVM.LLVM_LoadHandle mvar _ltp ptrReg args ret
      | let ptr = C.regValue ptrReg
      , not (getErrorSymbolicFunCalls errorSymbolicFunCalls)
      , Nothing <- W4.asNat (Mem.llvmPointerBlock ptr) -> do
          let ptrWidth = Mem.ptrWidth ptr
          Refl <-
            -- LLVM_LoadHandle binds an existentially quantified type variable
            -- representing the pointer width, but grease's `ExtShape LLVM`
            -- instance only works if the pointer width is 64 bits in
            -- particular. As such, the most direct way to make this code
            -- typecheck is to check if the LLVM_LoadHandle's pointer width is
            -- 64 at runtime, which we do with `testEquality` below.
            case testEquality ptrWidth (NatRepr.knownNat @64) of
              Just r ->
                pure r
              Nothing ->
                panic
                  "extensionExec"
                  [ "LLVM frontend with non-64-bit pointer size"
                  , show ptrWidth
                  ]
          let funcName = W4.functionNameFromText "_grease_symbolic_fn"
          hdl <- C.mkHandle' halloc funcName args ret
          case createSkipOverride la dl mvar funcName ret of
            Just ov -> do
              doLog la Diag.SkippedSymbolicFnHandleCall
              pure
                ( C.HandleFnVal hdl
                , insertFunctionHandle st hdl (C.UseOverride ov)
                )
            -- If we cannot create an LLVM skip override for the given types,
            -- then fall back on the default implementation of
            -- LLVM_LoadHandle. This will ultimately fail when it encounters
            -- the symbolic function pointer.
            Nothing ->
              defaultExec
    _ ->
      defaultExec
 where
  defaultExec = C.extensionExec baseExt stmt st

-- Helper, not exported
--
-- Insert a function handle into a state's function bindings
insertFunctionHandle ::
  -- | State to update
  C.SimState p sym ext r f a ->
  -- | Handle to bind and insert
  C.FnHandle args ret ->
  -- | Function state to bind handle to
  C.FnState p sym ext args ret ->
  C.SimState p sym ext r f a
insertFunctionHandle state handle fnState =
  let C.FnBindings curHandles = state ^. C.stateContext ^. C.functionBindings
   in let newHandles =
            C.FnBindings $
              C.insertHandleMap handle fnState curHandles
       in set (C.stateContext . C.functionBindings) newHandles state
