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

import Control.Monad.IO.Class (MonadIO (liftIO))
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
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import What4.Interface qualified as WI

-- | Load a null-terminated string from memory.
--
-- The pointer to read from must be concrete and nonnull. We allow symbolic
-- characters, but if the @'Maybe' 'Int'@ argument is 'Nothing', then we require
-- that the string end with a concrete null terminator character.
loadString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  -- | The global variable to the LLVM memory.
  C.GlobalVar CLM.Mem ->
  -- | The @macaw-symbolic@ memory model configuration.
  Symbolic.MemModelConfig p sym arch CLM.Mem ->
  -- | The pointer to the string.
  CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  -- | If @'Just' n@, read a maximum of @n@ characters. If 'Nothing', read until
  -- a concrete null terminator character is encountered.
  Maybe Int ->
  -- | The initial Crucible state.
  CS.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  -- | The loaded bytes from the string (excluding the null terminator) and the
  -- updated Crucible state.
  IO
    ( [WI.SymBV sym 8]
    , CS.SimState p sym (Symbolic.MacawExt arch) rtp f args
    )
loadString bak mvar mmConf ptr0 maxChars st = do
  -- Normally, the lazy `macaw-symbolic` memory model would resolve all pointer
  -- reads, which would avoid the need to do this ourselves here. Nevertheless,
  -- we intentionally disable this pointer-resolving in our instantiation of the
  -- memory model (see gitlab#143), so we have to do this manually. Without
  -- this, the `tests/refine/pos/excluded_overrides/test.ppc32.elf` test case
  -- would not succeed, as `grease` would be unable to conclude that the loaded
  -- string ends with a concrete null terminator character.
  ptr1 <- Symbolic.resolveLLVMPtr bak (CS.regValue ptr0)
  DMSMS.loadConcretelyNullTerminatedString mvar mmConf st ptr1 maxChars

-- | Like 'loadString', except that each character read is asserted to be
-- concrete. If a symbolic character is encountered, this function will
-- generate a failing assertion. The loaded characters are then packed into a
-- 'BS.ByteString' for convenience.
loadConcreteString ::
  forall sym bak p arch rtp f args solver t st fm.
  ( OnlineSolverAndBackend solver sym bak t st fm
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  C.GlobalVar CLM.Mem ->
  Symbolic.MemModelConfig p sym arch CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  Maybe Int ->
  CS.SimState p sym (Symbolic.MacawExt arch) rtp f args ->
  IO
    ( BS.ByteString
    , CS.SimState p sym (Symbolic.MacawExt arch) rtp f args
    )
loadConcreteString bak mvar mmConf ptr maxChars st0 = do
  (symBytes, st1) <- loadString bak mvar mmConf ptr maxChars st0
  bytes <- traverse concretizeByte symBytes
  pure (BS.pack bytes, st1)
 where
  concretizeByte :: WI.SymBV sym 8 -> IO Word8
  concretizeByte symByte =
    case BV.asUnsigned <$> WI.asBV symByte of
      Just byte -> pure $ fromInteger byte
      Nothing ->
        liftIO $
          CB.addFailedAssertion bak $
            CS.Unsupported
              Stack.callStack
              "Symbolic value encountered when loading a string"
