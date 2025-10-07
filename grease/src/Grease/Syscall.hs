{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Syscall (
  builtinGenericSyscalls,

  -- * Override implementations
  callGetppid,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as C
import Stubs.Syscall qualified as Stubs
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4

-- | All of the overrides that work across all supported configurations.
--
-- This list is documented in @doc/syscalls.md@.
builtinGenericSyscalls ::
  CLM.HasPtrWidth w =>
  Map.Map W4.FunctionName (Stubs.SomeSyscall p sym arch)
builtinGenericSyscalls =
  Map.fromList $
    map
      ( \someSyscallOv@(Stubs.SomeSyscall syscallOv) ->
          (Stubs.syscallName syscallOv, someSyscallOv)
      )
      [ Stubs.SomeSyscall buildGetppidOverride
      ]

-----
-- Scaffolding to turn override implementations into Syscalls
-----

buildGetppidOverride ::
  CLM.HasPtrWidth w =>
  Stubs.Syscall p sym Ctx.EmptyCtx ext (CLM.LLVMPointerType w)
buildGetppidOverride =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkSyscall "getppid" $
      \bak _args -> callGetppid bak

-----
-- The implementations of the overrides themselves
-----

-- | Override for the @getppid(2)@ system call.
--
-- The behavior of this override is documented in @doc/syscalls.md@.
callGetppid ::
  ( C.IsSymBackend sym bak
  , CLM.HasPtrWidth w
  ) =>
  bak ->
  C.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callGetppid bak = liftIO $ do
  let sym = C.backendGetSym bak
  -- The parent PID can change at any time due to reparenting, so this override
  -- always returns a new fresh value.
  symbolicResult <-
    W4.freshConstant sym (W4.safeSymbol "getppid") (W4.BaseBVRepr ?ptrWidth)
  CLM.llvmPointer_bv sym symbolicResult
