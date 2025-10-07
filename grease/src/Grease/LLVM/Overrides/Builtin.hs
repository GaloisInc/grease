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
import Grease.LLVM.Overrides.Defs (customLLVMOverrides)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.Intrinsics.LLVM qualified as LLVM
import Lang.Crucible.LLVM.Intrinsics.Libc qualified as Libc
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.TypeContext qualified as TCtx

-- | LLVM overrides that are unconditionally made available to all LLVM programs
-- (S-expression or bitcode module), including overrides.
--
-- This does not include \"polymorphic\" overrides, see 'builtinLLVMOverrides'
-- for those.
basicLLVMOverrides ::
  forall p sym ext w.
  ( CB.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: CLM.MemOptions
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (CLI.SomeLLVMOverride p sym ext)
basicLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = CLI.defaultIntrinsicsOptions
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
  ( CB.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: CLM.MemOptions
  , ?intrinsicsOpts :: CLI.IntrinsicsOptions
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  [CLI.SomeLLVMOverride p sym ext]
libcOverrides fs =
  List.concat @[]
    [ Libc.libc_overrides
    , symioLlvmOverrides
    , customLLVMOverrides
    ]
 where
  symioLlvmOverrides :: [CLI.SomeLLVMOverride p sym ext]
  symioLlvmOverrides =
    [ CLI.SomeLLVMOverride $ SymIO.openFile fs
    , CLI.SomeLLVMOverride $ SymIO.closeFile fs
    , CLI.SomeLLVMOverride $ SymIO.readFileHandle fs
    , CLI.SomeLLVMOverride $ SymIO.writeFileHandle fs
    ]

-- | All of the @crucible-llvm@ overrides that work across all supported
-- configurations.
--
-- c.f. 'builtinStubsOverrides', which includes a subset of the functions here
-- (i.e., the ones from libc, but not the LLVM intrinsics).
builtinLLVMOverrides ::
  ( CB.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: CLM.MemOptions
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (CLI.OverrideTemplate p sym ext arch)
builtinLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = CLI.defaultIntrinsicsOptions
   in fmap (\(CLI.SomeLLVMOverride ov) -> CLI.basic_llvm_override ov) (basicLLVMOverrides fs)
        <> Seq.fromList (List.map (\(pfx, LLVM.Poly1LLVMOverride ov) -> CLI.polymorphic1_llvm_override pfx ov) LLVM.poly1_llvm_overrides)
