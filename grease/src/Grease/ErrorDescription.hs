module Grease.ErrorDescription (ErrorDescription (..), concretizeErrorDescription) where

import Data.Macaw.Symbolic.Memory (MacawError (UnmappedGlobalMemoryAccess))
import Data.Macaw.Symbolic.Memory qualified as MSM
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import What4.Expr qualified as W4
import What4.Interface qualified as WI

concretizeErrorDescription :: (WI.IsExprBuilder sym, sym ~ W4.ExprBuilder t st fs) => sym -> W4.GroundEvalFn t -> ErrorDescription sym -> IO (ErrorDescription sym)
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
