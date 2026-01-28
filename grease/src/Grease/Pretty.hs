{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Pretty (
  prettyPtrFn,
  prettyPtrFnMap,
) where

import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Grease.Panic (panic)
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Pretty as P (
  IntrinsicPrettyFn (IntrinsicPrettyFn),
  IntrinsicPrinters (IntrinsicPrinters),
 )
import Lang.Crucible.Types qualified as C
import What4.Interface qualified as WI

-- | A 'IntrinsicPrettyFn' for LLVM pointers
--
-- TODO: Upstream to Crucible-LLVM
prettyPtrFn :: WI.IsExpr (WI.SymExpr sym) => IntrinsicPrettyFn sym "LLVM_pointer"
prettyPtrFn = IntrinsicPrettyFn $ \tyCtx ptr ->
  case Ctx.viewAssign tyCtx of
    Ctx.AssignExtend (Ctx.viewAssign -> Ctx.AssignEmpty) (C.BVRepr _) ->
      CLM.ppPtr ptr
    -- These are impossible by the definition of LLVMPointerImpl
    Ctx.AssignEmpty ->
      panic
        "LLVM.MemModel.Pointer.concToSymPtrFn"
        ["Impossible: LLVMPointerType empty context"]
    Ctx.AssignExtend _ _ ->
      panic
        "LLVM.MemModel.Pointer.concToSymPtrFn"
        ["Impossible: LLVMPointerType ill-formed context"]

-- | A singleton map suitable for use in 'ppRegVal' if LLVM pointers are the
-- only intrinsic type in use
--
-- TODO: Upstream to Crucible-LLVM
prettyPtrFnMap :: WI.IsExpr (WI.SymExpr sym) => IntrinsicPrinters sym
prettyPtrFnMap = IntrinsicPrinters (MapF.singleton (C.knownSymbol @"LLVM_pointer") prettyPtrFn)
