{-# LANGUAGE ImplicitParams #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Overrides.Libc (
  libcOverrides,
) where

import Data.List qualified as List
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.Intrinsics.Libc qualified as Libc
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.LLVM.TypeContext qualified as CLTC

-- | LLVM overrides corresponding to functions defined in @libc@.
libcOverrides ::
  forall p sym ext w.
  ( CB.IsSymInterface sym
  , ?lc :: CLTC.TypeContext
  , ?memOpts :: CLM.MemOptions
  , ?intrinsicsOpts :: CLI.IntrinsicsOptions
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  CLSIO.LLVMFileSystem w ->
  [CLI.SomeLLVMOverride p sym ext]
libcOverrides fs =
  List.concat @[]
    [ Libc.libc_overrides
    , symioLlvmOverrides
    ]
 where
  symioLlvmOverrides :: [CLI.SomeLLVMOverride p sym ext]
  symioLlvmOverrides =
    [ CLI.SomeLLVMOverride $ CLSIO.openFile fs
    , CLI.SomeLLVMOverride $ CLSIO.closeFile fs
    , CLI.SomeLLVMOverride $ CLSIO.readFileHandle fs
    , CLI.SomeLLVMOverride $ CLSIO.writeFileHandle fs
    ]
