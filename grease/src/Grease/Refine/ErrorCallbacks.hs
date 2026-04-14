{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright   : (c) Galois, Inc. 2027
-- Maintainer  : GREASE Maintainers <grease@galois.com>
-- Module      : Grease.Refine.ErrorCallbacks
-- Description : Error callbacks for refinement
module Grease.Refine.ErrorCallbacks (
  ErrorCallbacks (..),
  buildErrMaps,
  withErrorCallbacks,
) where

import Data.IORef (IORef, modifyIORef)
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Map.Strict qualified as Map
import Data.Parameterized.Nonce (Nonce)
import GHC.IORef (newIORef)
import Grease.ErrorDescription (ErrorDescription (CrucibleLLVMError, MacawMemError))
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Errors qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.MemModel.CallStack qualified as LLCS
import Lang.Crucible.LLVM.MemModel.Partial qualified as Mem
import What4.Expr qualified as WE
import What4.Interface qualified as WI

-- | Callbacks that record error information during symbolic execution.
--
-- Type parameters:
--
-- - @sym@: instance of 'Lang.Crucible.Backend.IsSymInterface'
-- - @t@: nonce generator scope
data ErrorCallbacks sym t
  = ErrorCallbacks
  { llvmErrCallback :: LLCS.CallStack -> Mem.BoolAnn sym -> CLLVM.BadBehavior sym -> IO ()
  -- ^ Callback to pass to @recordLLVMAnnotation@
  , macawAssertionCallback :: sym -> WI.Pred sym -> MSM.MacawError sym -> IO (WI.Pred sym)
  -- ^ Callback to pass to @processMacawAssert@
  , errorMap :: IORef (Map.Map (Nonce t C.BaseBoolType) (ErrorDescription sym))
  -- ^ Map populated by the callbacks above, keyed by the annotation nonce
  }

-- | Build an 'ErrorCallbacks' that records memory errors into a fresh map.
--
-- There are two types of errors: 'CLLVM.BadBehavior' (from the LLVM memory
-- model/semantics) and 'MSM.MacawError' (Macaw-specific errors). Pass
-- 'llvmErrCallback' to @recordLLVMAnnotation@ and 'macawAssertionCallback'
-- to @processMacawAssert@.
buildErrMaps ::
  sym ~ WE.ExprBuilder t st fs =>
  IO (ErrorCallbacks sym t)
buildErrMaps = do
  bbMapRef <- newIORef Map.empty
  let recordLLVMAnnotation callStack (Mem.BoolAnn ann) bb =
        modifyIORef bbMapRef $
          Map.insert ann (CrucibleLLVMError bb callStack)
  let processMacawAssert sym p err = do
        (ann, p') <- WI.annotateTerm sym p
        _ <- modifyIORef bbMapRef $ Map.insert ann (MacawMemError err)
        pure p'
  pure
    ErrorCallbacks
      { errorMap = bbMapRef
      , llvmErrCallback = recordLLVMAnnotation
      , macawAssertionCallback = processMacawAssert
      }

-- | Establish the implicit parameters @?recordLLVMAnnotation@ and
-- @?processMacawAssert@ from an 'ErrorCallbacks' value, then run @action@.
withErrorCallbacks ::
  ErrorCallbacks sym t ->
  ( ( MSM.MacawProcessAssertion sym
    , CLM.HasLLVMAnn sym
    ) =>
    IO r
  ) ->
  IO r
withErrorCallbacks ErrorCallbacks{llvmErrCallback, macawAssertionCallback} action =
  let ?recordLLVMAnnotation = llvmErrCallback
      ?processMacawAssert = macawAssertionCallback
   in action
