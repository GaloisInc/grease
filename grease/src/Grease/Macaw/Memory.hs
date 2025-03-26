{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>

Various utilities for loading strings from memory. Strictly speaking, nothing
about this functionality is @grease@-specific, but there are some opinionated
design choices being made here that warrant a closer look before being
considered for inclusion upstream in @macaw@:

* We currently require all strings to be null-terminated. There are some other
  functions that only load a specific number of characters (e.g., @strncpy@),
  however, and if we wanted to support overriding those functions, we would need
  to change the API to allow users to configure the maximum number of characters
  to load.

* We check if the last character is a null terminator by consuling an SMT
  solver. This is useful (and necessary) for some use cases, but it does require
  adding 'OnlineSolverAndBackend' constraints to support it.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Grease.Macaw.Memory
  ( loadString
  , loadConcreteString
  ) where

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as BS
import Data.Eq (Eq(..))
import Data.Function (($), id)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Traversable (Traversable(..))
import Data.Word (Word8)
import Prelude (Num(..))
import qualified GHC.Stack as Stack
import System.IO (IO)

-- bv-sized
import qualified Data.BitVector.Sized as BV

-- what4
import qualified What4.Interface as W4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.MemModel as Mem

-- macaw-base
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Memory as MM

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic
import qualified Data.Macaw.Symbolic.Backend as Symbolic
import qualified Data.Macaw.Symbolic.Concretize as Symbolic

import Grease.Utility (OnlineSolverAndBackend)

-- | Load a null-terminated string from memory.
--
-- The pointer to read from must be concrete and nonnull. We allow symbolic
-- characters, but we require that the string end with a concrete null
-- terminator character.
loadString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  -- | The global variable to the LLVM memory.
  C.GlobalVar Mem.Mem ->
  -- | The @macaw-symbolic@ memory model configuration.
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  -- | The endianness of the architecture.
  MC.Endianness ->
  -- | The pointer to the string.
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  -- | The initial Crucible state.
  C.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  -- | The loaded bytes from the string (excluding the null terminator) and the
  -- updated Crucible state.
  IO ( [W4.SymBV sym 8]
     , C.SimState p sym (Symbolic.MacawExt arch) rtp f args
     )
loadString bak mvar mmConf endian ptr0 st = do
  memImpl <- Symbolic.getMem st mvar
  let sym = C.backendGetSym bak
  -- Normally, the lazy `macaw-symbolic` memory model would resolve all pointer
  -- reads, which would avoid the need to do this ourselves here. Nevertheless,
  -- we intentionally disable this pointer-resolving in our instantiation of the
  -- memory model (see gitlab#143), so we have to do this manually. Without
  -- this, the `tests/refine/pos/excluded_overrides/test.ppc32.elf` test case
  -- would not succeed, as `grease` would be unable to conclude that the loaded
  -- string ends with a concrete null terminator character.
  ptr1 <- Symbolic.resolveLLVMPtr bak (C.regValue ptr0)
  let ptrEntry1 = C.RegEntry Mem.PtrRepr ptr1

  let go ::
        ([W4.SymBV sym 8] -> [W4.SymBV sym 8]) ->
        C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
        C.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
        IO ( [W4.SymBV sym 8]
           , C.SimState p sym (Symbolic.MacawExt arch) rtp f args
           )
      go f p st0 = do
        let addrWidth = MC.addrWidthRepr ?ptrWidth
        let readInfo = MC.BVMemRepr (W4.knownNat @1) endian
        (v, st1) <-
          liftIO $ Symbolic.doReadMemModel mvar mmConf addrWidth readInfo p st0
        x <- liftIO $ Mem.projectLLVM_bv bak v
        if (BV.asUnsigned <$> W4.asBV x) == Just 0
          then pure (f [], st1) -- We have encountered a null terminator, so stop.
          else do
            one <- liftIO $ W4.bvOne sym Mem.PtrWidth
            p' <- liftIO $ Mem.doPtrAddOffset bak memImpl (C.regValue p) one
            let pEntry' = C.RegEntry Mem.PtrRepr p'
            go (\xs -> f (x:xs)) pEntry' st1

  go id ptrEntry1 st

-- | Like 'loadString', except that each character read is asserted to be
-- concrete. If a symbolic character is encountered, this function will
-- generate a failing assertion. The loaded characters are then packed into a
-- 'BS.ByteString' for convenience.
loadConcreteString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  MC.Endianness ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  IO ( BS.ByteString
     , C.SimState p sym (Symbolic.MacawExt arch) rtp f args
     )
loadConcreteString bak mvar mmConf endian ptr st0 = do
  (symBytes, st1) <- loadString bak mvar mmConf endian ptr st0
  bytes <- traverse concretizeByte symBytes
  pure (BS.pack bytes, st1)
  where
    concretizeByte :: W4.SymBV sym 8 -> IO Word8
    concretizeByte symByte =
      case BV.asUnsigned <$> W4.asBV symByte of
        Just byte -> pure $ fromInteger byte
        Nothing ->
          liftIO $ C.addFailedAssertion bak
                 $ C.Unsupported Stack.callStack
                     "Symbolic value encountered when loading a string"
