{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Due to the orphan ArchReloc instance below
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.X86 (x86Ctx) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.BitVector.Sized qualified as BV
import Data.ElfEdit qualified as EE
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.X86 qualified as X86
import Data.Macaw.X86.X86Reg qualified as X86
import Data.Map qualified as Map
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Grease.Macaw.Arch (ArchContext (..), ArchRegs, ArchReloc)
import Grease.Macaw.Arch.X86.Reg (getX86Reg, modifyX86Reg)
import Grease.Macaw.Load.Relocation (RelocType (..))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (x64StackPtrShape)
import Grease.Utility (bytes64LE)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Stubs.FunctionOverride.X86_64.Linux qualified as Stubs
import Stubs.Memory.X86_64.Linux qualified as Stubs
import Stubs.Syscall.Names.X86_64.Linux qualified as Stubs
import Stubs.Syscall.X86_64.Linux qualified as Stubs
import What4.Interface qualified as W4

type instance ArchReloc X86.X86_64 = EE.X86_64_RelocationType

-- | x64 System-V assumed
regList :: [Some.Some X86.X86Reg]
regList = Some.Some <$> X86.x86ArgumentRegs

x86Ctx ::
  (?memOpts :: Mem.MemOptions) =>
  C.HandleAllocator ->
  -- | Initialize the end of the stack to a 'Word64' value (which is split into
  -- a little-endian sequence of 8 concrete bytes) if the value is @Just@.
  -- Otherwise, initialize the end of the stack to 8 fresh, symbolic bytes.
  Maybe Word64 ->
  ExtraStackSlots ->
  IO (ArchContext X86.X86_64)
x86Ctx halloc mbReturnAddr stackArgSlots = do
  -- We want to initialize the %fs and %gs segment registers with symbolic
  -- arrays so that they do not crash when they are accessed (e.g., as part of
  -- checks inserted by -fstack-protector). We piggyback on top of `stubs` to
  -- accomplish this. See also Note [Coping with stack protection] in
  -- Grease.Macaw.Arch.
  fsbaseGlob <- Stubs.freshFSBaseGlobalVar halloc
  gsbaseGlob <- Stubs.freshGSBaseGlobalVar halloc
  let extOverride =
        Stubs.x86_64LinuxStmtExtensionOverride fsbaseGlob gsbaseGlob
  avals <- case Symbolic.genArchVals Proxy Proxy (Just extOverride) of
    Nothing ->
      -- `genArchVals` is total: https://github.com/GaloisInc/macaw/issues/231
      panic "armCtx" ["Failed to generate architecture-specific values"]
    Just avals -> pure avals
  return
    ArchContext
      { _archInfo = X86.x86_64_linux_info
      , _archVals = avals
      , _archRelocSupported = x64RelocSupported
      , _archGetIP = \regs -> do
          C.RV (Mem.LLVMPointer _base off) <- getX86Reg X86.X86_IP regs
          pure off
      , _archPcReg = X86.X86_IP
      , _archIntegerArguments = \bak ->
          Stubs.x86_64LinuxIntegerArguments bak avals
      , _archIntegerReturnRegisters = Stubs.x86_64LinuxIntegerReturnRegisters
      , _archFunctionReturnAddr = Stubs.x86_64LinuxReturnAddr
      , _archSyscallArgumentRegisters = Stubs.x86_64LinuxSyscallArgumentRegisters
      , _archSyscallNumberRegister = Stubs.x86_64LinuxSyscallNumberRegister
      , _archSyscallReturnRegisters = Stubs.x86_64LinuxSyscallReturnRegisters
      , _archSyscallCodeMapping = Stubs.syscallMap
      , _archStackPtrShape = x64StackPtrShape (bytes64LE <$> mbReturnAddr) stackArgSlots
      , _archInitGlobals = Stubs.x86_64LinuxInitGlobals fsbaseGlob gsbaseGlob
      , -- NB: x86-64 does not have a link register, so we don't need to
        -- override it.
        _archRegOverrides = Map.empty
      , _archOffsetStackPointerPostCall = x64FixupStackPointer
      , _archABIParams = regList
      }

x64RelocSupported :: EE.X86_64_RelocationType -> Maybe RelocType
x64RelocSupported EE.R_X86_64_RELATIVE = Just RelativeReloc
x64RelocSupported EE.R_X86_64_GLOB_DAT = Just SymbolReloc
x64RelocSupported EE.R_X86_64_64 = Just SymbolReloc
x64RelocSupported _ = Nothing

-- | On x86, the @call@ instruction pushes the return address onto the stack.
-- When skipping a function or using an override, no @ret@ instruction will
-- pop the return address (and increment the stack pointer accordingly), so we
-- simulate that effect here.
x64FixupStackPointer ::
  C.IsSymInterface sym =>
  ArchRegs sym X86.X86_64 ->
  C.OverrideSim p sym ext rtp a r (ArchRegs sym X86.X86_64)
x64FixupStackPointer regs = do
  sym <- C.getSymInterface
  modifyX86Reg regs X86.RSP $ \(C.RV rsp) -> liftIO $ do
    let widthRepr = NatRepr.knownNat @64
    eight <- W4.bvLit sym widthRepr (BV.mkBV widthRepr 8)
    C.RV <$> Mem.ptrAdd sym widthRepr rsp eight
