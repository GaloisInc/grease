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

import Control.Lens ((.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.BitVector.Sized qualified as BV
import Data.ElfEdit qualified as EE
import Data.Function ((&))
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.X86 qualified as X86
import Data.Macaw.X86.Symbolic.Regs qualified as X86SymRegs
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)
import Grease.Macaw.Arch (ArchRegs, ArchReloc, defaultPCFixup)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Load.Relocation (RelocType (RelativeReloc, SymbolReloc))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (x64StackPtrShape)
import Grease.Utility (bytes64LE)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Stubs.FunctionOverride.X86_64.Linux qualified as Stubs
import Stubs.Memory.X86_64.Linux qualified as Stubs
import Stubs.Syscall.Names.X86_64.Linux qualified as Stubs
import Stubs.Syscall.X86_64.Linux qualified as Stubs
import What4.Interface qualified as WI

type instance ArchReloc X86.X86_64 = EE.X86_64_RelocationType

-- | x64 System-V assumed
regList :: [Some.Some X86.X86Reg]
regList = Some.Some <$> X86.x86ArgumentRegs

x86Ctx ::
  (?memOpts :: CLM.MemOptions) =>
  C.HandleAllocator ->
  -- | Initialize the end of the stack to a 'Word64' value (which is split into
  -- a little-endian sequence of 8 concrete bytes) if the value is @Just@.
  -- Otherwise, initialize the end of the stack to 8 fresh, symbolic bytes.
  Maybe Word64 ->
  ExtraStackSlots ->
  IO (Arch.ArchContext X86.X86_64)
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
    Arch.ArchContext
      { Arch._archInfo = X86.x86_64_linux_info
      , Arch._archVals = avals
      , Arch._archRelocSupported = x64RelocSupported
      , Arch._archGetIP = \regs -> do
          let CS.RV (CLM.LLVMPointer _base off) = regs ^. ixF' X86SymRegs.rip
          pure off
      , Arch._archPcReg = X86.X86_IP
      , Arch._archIntegerArguments = \bak ->
          Stubs.x86_64LinuxIntegerArguments bak avals
      , Arch._archIntegerReturnRegisters = Stubs.x86_64LinuxIntegerReturnRegisters
      , Arch._archFunctionReturnAddr = Stubs.x86_64LinuxReturnAddr
      , Arch._archSyscallArgumentRegisters = Stubs.x86_64LinuxSyscallArgumentRegisters
      , Arch._archSyscallNumberRegister = Stubs.x86_64LinuxSyscallNumberRegister
      , Arch._archSyscallReturnRegisters = Stubs.x86_64LinuxSyscallReturnRegisters
      , Arch._archSyscallCodeMapping = Stubs.syscallMap
      , Arch._archStackPtrShape = x64StackPtrShape (bytes64LE <$> mbReturnAddr) stackArgSlots
      , Arch._archInitGlobals = Stubs.x86_64LinuxInitGlobals fsbaseGlob gsbaseGlob
      , -- NB: x86-64 does not have a link register, so we don't need to
        -- override it.
        Arch._archRegOverrides = Map.empty
      , Arch._archOffsetStackPointerPostCall = x64FixupStackPointer
      , Arch._archABIParams = regList
      , Arch._archPCFixup = defaultPCFixup @X86.X86_64 Proxy
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
  CB.IsSymInterface sym =>
  ArchRegs sym X86.X86_64 ->
  CS.OverrideSim p sym ext rtp a r (ArchRegs sym X86.X86_64)
x64FixupStackPointer regs = do
  sym <- CS.getSymInterface
  liftIO $ do
    let CS.RV rsp = regs ^. ixF' X86SymRegs.rsp
    let widthRepr = NatRepr.knownNat @64
    eight <- WI.bvLit sym widthRepr (BV.mkBV widthRepr 8)
    rsp' <- CS.RV <$> CLM.ptrAdd sym widthRepr rsp eight
    pure (regs & ixF' X86SymRegs.rsp .~ rsp')
