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
import Data.Text qualified as Text
import Grease.Options qualified as GO
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.SymIO qualified as SymIO
import Lang.Crucible.SymIO.Loader qualified as SymIO.Loader
import What4.Interface qualified as WI

data InitializedFs sym ptrW
  = InitializedFs
  { initFsContents :: SymIO.InitialFileSystemContents sym
  , initFs :: CLSIO.LLVMFileSystem ptrW
  , initFsGlobals :: CS.SymGlobalState sym
  , initFsOverride :: CLSIO.SomeOverrideSim sym ()
  }

-- | Initialize the symbolic file system.
initialLlvmFileSystem ::
  ( CB.IsSymInterface sym
  , CLM.HasPtrWidth ptrW
  ) =>
  C.HandleAllocator ->
  sym ->
  GO.FsOpts ->
  IO (InitializedFs sym ptrW)
initialLlvmFileSystem halloc sym fsOpts = do
  fileContents_ <-
    case GO.fsRoot fsOpts of
      Nothing -> pure SymIO.emptyInitialFileSystemContents
      Just rootDir -> SymIO.Loader.loadInitialFiles sym rootDir
  fileContentsWithStdin <- withSymStdin (GO.fsStdin fsOpts) fileContents_
  fileContentsWithFiles <-
    Monad.foldM withSymFile fileContentsWithStdin (Map.toList (GO.fsSymFiles fsOpts))

  -- We currently don't mirror stdout or stderr
  let mirroredOutputs = []
  (fs, gs, ov) <-
    CLSIO.initialLLVMFileSystem
      halloc
      sym
      ?ptrWidth
      fileContentsWithFiles
      mirroredOutputs
      CS.emptyGlobals
  pure $
    InitializedFs
      { initFsContents = fileContentsWithFiles
      , initFs = fs
      , initFsGlobals = gs
      , initFsOverride = ov
      }
 where
  mkBytes symb nBytes =
    let mkByte = WI.freshConstant sym symb (WI.BaseBVRepr (knownNat @8))
     in Monad.replicateM (fromIntegral nBytes) mkByte

  withSymFile fs (path, nBytes) = do
    let pathStr = Text.unpack path
    symFile <- mkBytes (WI.safeSymbol pathStr) nBytes
    let symFiles_ = SymIO.symbolicFiles fs
    let symFiles = Map.insert (SymIO.FileTarget pathStr) symFile symFiles_
    pure (fs{SymIO.symbolicFiles = symFiles})

  withSymStdin nBytes fs = do
    symStdin <- mkBytes (WI.safeSymbol "stdin") nBytes
    let symFiles_ = SymIO.symbolicFiles fs
    let symFiles = Map.insert SymIO.StdinTarget symStdin symFiles_
    pure (fs{SymIO.symbolicFiles = symFiles})
