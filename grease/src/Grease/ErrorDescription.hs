{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Grease.ErrorDescription (ErrorDescription (..), concretizeErrorDescription, ppMacawError, ppErrorDesc) where

import Data.List qualified as List
import Data.Macaw.Symbolic.Memory (MacawError (UnmappedGlobalMemoryAccess))
import Data.Macaw.Symbolic.Memory qualified as MSM
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as CLLVM
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.CallStack qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Prettyprinter qualified as PP
import What4.Expr qualified as W4
import What4.Interface qualified as W4
import What4.Interface qualified as WI

concretizeErrorDescription ::
  (WI.IsExprBuilder sym, sym ~ W4.ExprBuilder t st fs) =>
  sym ->
  W4.GroundEvalFn t ->
  ErrorDescription sym ->
  IO (ErrorDescription sym)
concretizeErrorDescription sym (W4.GroundEvalFn gFn) (CrucibleLLVMError bb cs) = do
  bb' <- Mem.concBadBehavior sym gFn bb
  pure (CrucibleLLVMError bb' cs)
concretizeErrorDescription sym (W4.GroundEvalFn gFn) (MacawMemError memerr) = do
  (UnmappedGlobalMemoryAccess ptrVal) <- pure memerr
  cptr <- Mem.concPtr sym gFn ptrVal
  pure $ MacawMemError (UnmappedGlobalMemoryAccess cptr)

-- | An error either from the underlying LLVM memory model or
-- from Macaw.
data ErrorDescription sym
  = CrucibleLLVMError (CLLVM.BadBehavior sym) LLCS.CallStack
  | MacawMemError (MSM.MacawError sym)

ppDelimitedObj :: [PP.Doc a] -> PP.Doc a
ppDelimitedObj lst = PP.sep $ "(" : lst ++ [")"]

-- TODO(#310) Pretty print macaw error using a macaw implementation
ppMacawError :: W4.IsExpr (W4.SymExpr sym) => MSM.MacawError sym -> PP.Doc a
ppMacawError (MSM.UnmappedGlobalMemoryAccess ptrVal) = PP.sep ["(", "UnmappedGlobalMemoryAccess", CLLVM.ppPtr ptrVal, ")"]

ppErrorDesc :: W4.IsExpr (W4.SymExpr sym) => ErrorDescription sym -> PP.Doc a
ppErrorDesc =
  \case
    (MacawMemError mmErr) -> ppDelimitedObj ["MacawMemErr", ppMacawError mmErr]
    (CrucibleLLVMError bb callStack) ->
      let ppCs = Mem.ppCallStack callStack
       in PP.vcat $
            [PP.indent 4 (Mem.ppBB bb)]
              List.++
              -- HACK(crucible#1112): No Eq on Doc, no access to cs
              if Mem.null callStack
                then []
                else ["in context:", PP.indent 2 ppCs]

instance W4.IsExpr (W4.SymExpr sym) => Show (ErrorDescription sym) where
  show :: WI.IsExpr (WI.SymExpr sym) => ErrorDescription sym -> String
  show = show . ppErrorDesc

instance W4.IsExpr (W4.SymExpr sym) => PP.Pretty (ErrorDescription sym) where
  pretty :: WI.IsExpr (WI.SymExpr sym) => ErrorDescription sym -> PP.Doc ann
  pretty = PP.viaShow