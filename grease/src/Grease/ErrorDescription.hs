module Grease.ErrorDescription (ErrorDescription (..)) where

import Data.Macaw.Symbolic.Memory qualified as MSM
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS

data ErrorDescription sym
  = CrucibleLLVMError (CLLVM.BadBehavior sym) LLCS.CallStack
  | MacawMemError (MSM.MacawError sym)
