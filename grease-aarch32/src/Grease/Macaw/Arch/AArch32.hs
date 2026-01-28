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
import Data.Macaw.CFG.Core qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Concretize qualified as Symbolic
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word32)
import Grease.Macaw.Arch (ArchReloc)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Load.Relocation (RelocType (RelativeReloc, SymbolReloc))
import Grease.Macaw.RegName (RegName (RegName))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (armStackPtrShape)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RegValue qualified as C
import Stubs.FunctionOverride.AArch32.Linux qualified as Stubs
import Stubs.Memory.AArch32.Linux qualified as Stubs
import Stubs.Syscall.AArch32.Linux qualified as Stubs
import Stubs.Syscall.Names.AArch32.Linux qualified as Stubs
import What4.Expr.Builder qualified as WEB
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

type instance ArchReloc ARM.ARM = EE.ARM32_RelocationType

armCtx ::
  (?memOpts :: CLM.MemOptions) =>
  C.HandleAllocator ->
  -- | If 'Just', use the specified 'Word32' value to override the initial
  -- value of the link register just before starting simulation.
  Maybe Word32 ->
  ExtraStackSlots ->
  IO (Arch.ArchContext ARM.ARM)
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
    Arch.ArchContext
      { Arch._archInfo = ARM.arm_linux_info
      , Arch._archGetIP = \regs -> do
          let C.RV (CLM.LLVMPointer _base off) = regs ^. ixF' ARM.Symbolic.Regs.pc
          pure off
      , Arch._archPcReg = ARM.pc
      , Arch._archVals = avals
      , Arch._archRelocSupported = armRelocSupported
      , Arch._archIntegerArguments = \bak ->
          Stubs.aarch32LinuxIntegerArguments bak avals
      , Arch._archIntegerReturnRegisters = Stubs.aarch32LinuxIntegerReturnRegisters
      , Arch._archFunctionReturnAddr = Stubs.aarch32LinuxReturnAddr
      , Arch._archSyscallArgumentRegisters = Stubs.aarch32LinuxSyscallArgumentRegisters
      , Arch._archSyscallNumberRegister = Stubs.aarch32LinuxSyscallNumberRegister
      , Arch._archSyscallReturnRegisters = Stubs.aarch32LinuxSyscallReturnRegisters
      , Arch._archSyscallCodeMapping = Stubs.syscallMap
      , Arch._archStackPtrShape = armStackPtrShape stackArgSlots
      , Arch._archInitGlobals = Stubs.aarch32LinuxInitGlobals tlsGlob
      , Arch._archRegOverrides = regOverrides
      , Arch._archOffsetStackPointerPostCall = pure
      , -- assumes AAPCS32 https://github.com/ARM-software/abi-aa/blob/main/aapcs32/aapcs32.rst#parameter-passing
        Arch._archABIParams = Some.Some <$> [ARM.r0, ARM.r1, ARM.r2, ARM.r3]
      , Arch._archPCFixup = armArchPCFixup
      }

-- | This PC fixup fixes call addresses to set the low bit of the address
-- if the CPU is in thumb mode so that Macaw knows to disassemble the
-- callee in thumb mode.
armArchPCFixup ::
  forall sym bak solver scope st fs arch.
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder scope st fs
  , WPO.OnlineSolver solver
  , bak ~ C.OnlineBackend solver scope st fs
  , arch ~ ARM.ARM
  ) =>
  bak ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
  MC.ArchSegmentOff arch ->
  IO (MC.ArchSegmentOff arch)
armArchPCFixup bak regs origAddr = do
  let C.RV (CLM.LLVMPointer _base off) = regs ^. ixF' ARM.Symbolic.Regs.pstateT
  -- TODO(#391): What4 is adding direct concretize functionality.
  -- We should replace the use of macaw here.
  raddr <- Symbolic.resolveSymBV bak C.knownNat off
  pure $ case WI.asBV raddr of
    Nothing -> panic "armArchPCFixup" ["PSTATE_T should always be concrete"]
    Just bv ->
      if BV.testBit' 0 bv
        then case MM.incSegmentOff origAddr 1 of
          Nothing -> panic "armArchPCFixup" ["attempted to call last byte of segment in thumb mode"]
          Just naddr -> naddr
        else origAddr

armRelocSupported :: EE.ARM32_RelocationType -> Maybe RelocType
armRelocSupported EE.R_ARM_RELATIVE = Just RelativeReloc
armRelocSupported EE.R_ARM_GLOB_DAT = Just SymbolReloc
armRelocSupported EE.R_ARM_ABS32 = Just SymbolReloc
armRelocSupported _ = Nothing
