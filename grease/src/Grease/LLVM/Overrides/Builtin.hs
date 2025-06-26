{-# LANGUAGE ImplicitParams #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides.Builtin (
  libcOverrides,
  basicLLVMOverrides,
  builtinLLVMOverrides,
) where

import Data.List qualified as List
import Data.Sequence qualified as Seq
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as Mem
import Lang.Crucible.LLVM.Intrinsics.LLVM qualified as LLVM
import Lang.Crucible.LLVM.Intrinsics.Libc qualified as Libc
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.TypeContext qualified as TCtx

-- | LLVM overrides that are unconditionally made available to all LLVM programs
-- (S-expression or bitcode module), including overrides.
--
-- This does not include \"polymorphic\" overrides, see 'builtinLLVMOverrides'
-- for those.
basicLLVMOverrides ::
  forall p sym ext w.
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (Mem.SomeLLVMOverride p sym ext)
basicLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = Mem.defaultIntrinsicsOptions
   in Seq.fromList $
        List.concat @[]
          [ libcOverrides fs
          , LLVM.basic_llvm_overrides
          ]

-- | Helper, not exported
--
-- LLVM overrides corresponding to functions defined in @libc@.
libcOverrides ::
  forall p sym ext w.
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , ?intrinsicsOpts :: Mem.IntrinsicsOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  [Mem.SomeLLVMOverride p sym ext]
libcOverrides fs =
  List.concat @[]
    [ Libc.libc_overrides
    , symioLlvmOverrides
    ]
 where
  symioLlvmOverrides :: [Mem.SomeLLVMOverride p sym ext]
  symioLlvmOverrides =
    [ Mem.SomeLLVMOverride $ SymIO.openFile fs
    , Mem.SomeLLVMOverride $ SymIO.closeFile fs
    , Mem.SomeLLVMOverride $ SymIO.readFileHandle fs
    , Mem.SomeLLVMOverride $ SymIO.writeFileHandle fs
    ]

-- | All of the @crucible-llvm@ overrides that work across all supported
-- configurations.
--
-- c.f. 'builtinStubsOverrides', which includes a subset of the functions here
-- (i.e., the ones from libc, but not the LLVM intrinsics).
builtinLLVMOverrides ::
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (Mem.OverrideTemplate p sym ext arch)
builtinLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = Mem.defaultIntrinsicsOptions
   in fmap (\(Mem.SomeLLVMOverride ov) -> Mem.basic_llvm_override ov) (basicLLVMOverrides fs)
        <> Seq.fromList (List.map (\(pfx, LLVM.Poly1LLVMOverride ov) -> Mem.polymorphic1_llvm_override pfx ov) LLVM.poly1_llvm_overrides)
