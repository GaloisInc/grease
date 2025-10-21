{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Due to the orphan ArchReloc instance below
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.AArch32 (armCtx) where

import Control.Lens ((^.))
import Data.BitVector.Sized qualified as BV
import Data.ElfEdit qualified as EE
import Data.Macaw.AArch32.Symbolic.Regs qualified as ARM.Symbolic.Regs
import Data.Macaw.ARM qualified as ARM
import Data.Macaw.ARM.ARMReg ()
import Data.Macaw.ARM.ARMReg qualified as ARM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Grease.Macaw.Arch (ArchContext (..), ArchReloc, defaultPCFixup)
import Grease.Macaw.Load.Relocation (RelocType (..))
import Grease.Macaw.RegName (RegName (..))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (armStackPtrShape)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator.RegValue qualified as C
import Stubs.FunctionOverride.AArch32.Linux qualified as Stubs
import Stubs.Memory.AArch32.Linux qualified as Stubs
import Stubs.Syscall.AArch32.Linux qualified as Stubs
import Stubs.Syscall.Names.AArch32.Linux qualified as Stubs

type instance ArchReloc ARM.ARM = EE.ARM32_RelocationType

armCtx ::
  (?memOpts :: CLM.MemOptions) =>
  C.HandleAllocator ->
  -- | If 'Just', use the specified 'Word32' value to override the initial
  -- value of the link register just before starting simulation.
  Maybe Word32 ->
  ExtraStackSlots ->
  IO (ArchContext ARM.ARM)
armCtx halloc mbReturnAddr stackArgSlots = do
  tlsGlob <- Stubs.freshTLSGlobalVar halloc
  let extOverride = Stubs.aarch32LinuxStmtExtensionOverride
  avals <- case Symbolic.genArchVals Proxy Proxy (Just extOverride) of
    Nothing ->
      -- `genArchVals` is total: https://github.com/GaloisInc/macaw/issues/231
      panic "armCtx" ["Failed to generate architecture-specific values"]
    Just avals -> pure avals
  let regOverrides =
        case mbReturnAddr of
          Just returnAddr ->
            Map.fromList
              [(RegName "R14", BV.mkBV knownNat (fromIntegral returnAddr))]
          Nothing ->
            Map.empty
  return
    ArchContext
      { _archInfo = ARM.arm_linux_info
      , _archGetIP = \regs -> do
          let C.RV (CLM.LLVMPointer _base off) = regs ^. ixF' ARM.Symbolic.Regs.pc
          pure off
      , _archPcReg = ARM.pc
      , _archVals = avals
      , _archRelocSupported = armRelocSupported
      , _archIntegerArguments = \bak ->
          Stubs.aarch32LinuxIntegerArguments bak avals
      , _archIntegerReturnRegisters = Stubs.aarch32LinuxIntegerReturnRegisters
      , _archFunctionReturnAddr = Stubs.aarch32LinuxReturnAddr
      , _archSyscallArgumentRegisters = Stubs.aarch32LinuxSyscallArgumentRegisters
      , _archSyscallNumberRegister = Stubs.aarch32LinuxSyscallNumberRegister
      , _archSyscallReturnRegisters = Stubs.aarch32LinuxSyscallReturnRegisters
      , _archSyscallCodeMapping = Stubs.syscallMap
      , _archStackPtrShape = armStackPtrShape stackArgSlots
      , _archInitGlobals = Stubs.aarch32LinuxInitGlobals tlsGlob
      , _archRegOverrides = regOverrides
      , _archOffsetStackPointerPostCall = pure
      , -- assumes AAPCS32 https://github.com/ARM-software/abi-aa/blob/main/aapcs32/aapcs32.rst#parameter-passing
        _archABIParams = Some.Some <$> [ARM.r0, ARM.r1, ARM.r2, ARM.r3]
      , _archPCFixup = defaultPCFixup @ARM.ARM Proxy
      }

armRelocSupported :: EE.ARM32_RelocationType -> Maybe RelocType
armRelocSupported EE.R_ARM_RELATIVE = Just RelativeReloc
armRelocSupported EE.R_ARM_GLOB_DAT = Just SymbolReloc
armRelocSupported EE.R_ARM_ABS32 = Just SymbolReloc
armRelocSupported _ = Nothing
