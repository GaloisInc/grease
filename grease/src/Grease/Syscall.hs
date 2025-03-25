{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Grease.Syscall
  ( builtinGenericSyscalls
    -- * Override implementations
  , callGetppid
  ) where

import Prelude (($), map)

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Map.Strict as Map

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx

-- what4
import qualified What4.FunctionName as W4
import qualified What4.Interface as W4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.MemModel as Mem

-- stubs
import qualified Stubs.Syscall as Stubs

-- | All of the overrides that work across all supported configurations.
builtinGenericSyscalls ::
  Mem.HasPtrWidth w =>
  Map.Map W4.FunctionName (Stubs.SomeSyscall p sym arch)
builtinGenericSyscalls =
  Map.fromList $ map
    (\someSyscallOv@(Stubs.SomeSyscall syscallOv) ->
      (Stubs.syscallName syscallOv, someSyscallOv))
    [ Stubs.SomeSyscall buildGetppidOverride
    ]

-----
-- Scaffolding to turn override implementations into Syscalls
-----

buildGetppidOverride ::
  Mem.HasPtrWidth w =>
  Stubs.Syscall p sym Ctx.EmptyCtx ext (Mem.LLVMPointerType w)
buildGetppidOverride =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkSyscall "getppid" $ \bak _args -> callGetppid bak

-----
-- The implementations of the overrides themselves
-----

-- | Override for the @getppid(2)@ system call.
callGetppid ::
  ( C.IsSymBackend sym bak
  , Mem.HasPtrWidth w
  ) =>
  bak ->
  C.OverrideSim p sym ext r args ret (Mem.LLVMPtr sym w)
callGetppid bak = liftIO $ do
  let sym = C.backendGetSym bak
  -- The parent PID can change at any time due to reparenting, so this override
  -- always returns a new fresh value.
  symbolicResult <-
    W4.freshConstant sym (W4.safeSymbol "getppid") (W4.BaseBVRepr ?ptrWidth)
  Mem.llvmPointer_bv sym symbolicResult
