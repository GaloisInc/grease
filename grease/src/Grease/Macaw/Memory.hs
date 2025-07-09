{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Various utilities for loading strings from memory. Strictly speaking, nothing
-- about this functionality is @grease@-specific, but we make a somewhat
-- opinionated design choice: we attempt to concretize the pointer to read from
-- using an SMT solver. This is useful (and necessary) for some use cases, but it
-- does require adding 'OnlineSolverAndBackend' constraints to support it. This
-- design choice warrants a closer look before being considered for inclusion
-- upstream in @macaw@.
module Grease.Macaw.Memory (
  loadString,
  loadConcreteString,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Concretize qualified as Symbolic
import Data.Macaw.Symbolic.Memory.Strings qualified as DMSMS
import Data.Word (Word8)
import GHC.Stack qualified as Stack
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Simulator qualified as C
import What4.Interface qualified as W4

-- | Load a null-terminated string from memory.
--
-- The pointer to read from must be concrete and nonnull. We allow symbolic
-- characters, but if the @'Maybe' 'Int'@ argument is 'Nothing', then we require
-- that the string end with a concrete null terminator character.
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
  -- | The pointer to the string.
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  -- | If @'Just' n@, read a maximum of @n@ characters. If 'Nothing', read until
  -- a concrete null terminator character is encountered.
  Maybe Int ->
  -- | The initial Crucible state.
  C.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  -- | The loaded bytes from the string (excluding the null terminator) and the
  -- updated Crucible state.
  IO
    ( [W4.SymBV sym 8]
    , C.SimState p sym (Symbolic.MacawExt arch) rtp f args
    )
loadString bak mvar mmConf ptr0 maxChars st = do
  -- Normally, the lazy `macaw-symbolic` memory model would resolve all pointer
  -- reads, which would avoid the need to do this ourselves here. Nevertheless,
  -- we intentionally disable this pointer-resolving in our instantiation of the
  -- memory model (see gitlab#143), so we have to do this manually. Without
  -- this, the `tests/refine/pos/excluded_overrides/test.ppc32.elf` test case
  -- would not succeed, as `grease` would be unable to conclude that the loaded
  -- string ends with a concrete null terminator character.
  ptr1 <- Symbolic.resolveLLVMPtr bak (C.regValue ptr0)
  DMSMS.loadConcretelyNullTerminatedString mvar mmConf st ptr1 maxChars

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
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  Maybe Int ->
  C.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  IO
    ( BS.ByteString
    , C.SimState p sym (Symbolic.MacawExt arch) rtp f args
    )
loadConcreteString bak mvar mmConf ptr maxChars st0 = do
  (symBytes, st1) <- loadString bak mvar mmConf ptr maxChars st0
  bytes <- traverse concretizeByte symBytes
  pure (BS.pack bytes, st1)
 where
  concretizeByte :: W4.SymBV sym 8 -> IO Word8
  concretizeByte symByte =
    case BV.asUnsigned <$> W4.asBV symByte of
      Just byte -> pure $ fromInteger byte
      Nothing ->
        liftIO $
          C.addFailedAssertion bak $
            C.Unsupported
              Stack.callStack
              "Symbolic value encountered when loading a string"
