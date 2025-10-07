{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Custom LLVM overrides.
module Grease.LLVM.Overrides.Defs (
  customLLVMOverrides,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.BitVector.Sized qualified as BV
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.QQ (llvmOvr)
import Lang.Crucible.Simulator.OverrideSim qualified as C
import What4.Interface qualified as W4

-- | Custom libc overrides.
customLLVMOverrides ::
  ( C.IsSymInterface sym
  , ?memOpts :: CLM.MemOptions
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth wptr
  ) =>
  [CLLVM.SomeLLVMOverride p sym ext]
customLLVMOverrides =
  [ CLLVM.SomeLLVMOverride getoptLong
  ]

-- | Override for @getopt_long@ that always returns @-1@, indicating that the
-- arguments have been exhausted.
getoptLong ::
  ( C.IsSymInterface sym
  , CLM.HasPtrWidth wptr
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  CLLVM.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> C.BVType 32
        Ctx.::> CLM.LLVMPointerType wptr
        Ctx.::> CLM.LLVMPointerType wptr
        Ctx.::> CLM.LLVMPointerType wptr
        Ctx.::> CLM.LLVMPointerType wptr
    )
    (C.BVType 32)
getoptLong =
  [llvmOvr| i32 @getopt_long( i32, ptr, ptr, ptr, ptr ) |]
    ( \_memOps _args -> do
        sym <- C.getSymInterface
        let i32 = knownNat @32
        liftIO (W4.bvLit sym i32 (BV.mkBV i32 (-1)))
    )
