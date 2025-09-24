{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.SymIO (
  InitializedFs (..),
  initialLlvmFileSystem,
) where

import Control.Monad qualified as Monad
import Data.Map.Strict qualified as Map
import Data.Parameterized.NatRepr (knownNat)
import Grease.Options qualified as GO
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as CLLVM.SymIO
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.SymIO qualified as SymIO
import Lang.Crucible.SymIO.Loader qualified as SymIO.Loader
import What4.Interface qualified as W4

data InitializedFs sym ptrW
  = InitializedFs
  { initFsContents :: SymIO.InitialFileSystemContents sym
  , initFs :: CLLVM.SymIO.LLVMFileSystem ptrW
  , initFsGlobals :: C.SymGlobalState sym
  , initFsOverride :: CLLVM.SymIO.SomeOverrideSim sym ()
  }

-- | Initialize the symbolic file system.
initialLlvmFileSystem ::
  ( C.IsSymInterface sym
  , Mem.HasPtrWidth ptrW
  ) =>
  C.HandleAllocator ->
  sym ->
  GO.FsOpts ->
  IO (InitializedFs sym ptrW)
initialLlvmFileSystem halloc sym simOpts = do
  fileContents_ <-
    case GO.fsRoot simOpts of
      Nothing -> pure SymIO.emptyInitialFileSystemContents
      Just rootDir -> SymIO.Loader.loadInitialFiles sym rootDir
  fileContents <- withSymStdin (GO.fsStdin simOpts) fileContents_
  -- We currently don't mirror stdout or stderr
  let mirroredOutputs = []
  (fs, gs, ov) <- CLLVM.SymIO.initialLLVMFileSystem halloc sym ?ptrWidth fileContents mirroredOutputs C.emptyGlobals
  pure $
    InitializedFs
      { initFsContents = fileContents
      , initFs = fs
      , initFsGlobals = gs
      , initFsOverride = ov
      }
 where
  withSymStdin nBytes fs = do
    symStdin <-
      let mkByte = W4.freshConstant sym (W4.safeSymbol "stdin") (W4.BaseBVRepr (knownNat @8))
       in Monad.replicateM (fromIntegral nBytes) mkByte
    let symFiles_ = SymIO.symbolicFiles fs
    let symFiles = Map.insert SymIO.StdinTarget symStdin symFiles_
    pure (fs{SymIO.symbolicFiles = symFiles})
