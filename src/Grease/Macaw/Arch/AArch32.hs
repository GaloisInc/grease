{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Due to the orphan ArchReloc instance below

module Grease.Macaw.Arch.AArch32 (armCtx) where

import Control.Exception.Safe (throw)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Word (Word32)

-- bv-sized
import qualified Data.BitVector.Sized as BV

-- parameterized-utils
import Data.Parameterized.NatRepr (knownNat)

-- crucible
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator.RegValue as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.MemModel as Mem
import qualified Lang.Crucible.LLVM.DataLayout as Mem

-- elf-edit
import qualified Data.ElfEdit as EE

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- macaw-aarch32
import qualified Data.Macaw.ARM as ARM
import qualified Data.Macaw.ARM.ARMReg as ARM
import Data.Macaw.ARM.ARMReg ()

-- macaw-aarch32-symbolic
import qualified Data.Macaw.AArch32.Symbolic as ARM.Symbolic

-- stubs
import qualified Stubs.FunctionOverride.AArch32.Linux as Stubs
import qualified Stubs.Memory.AArch32.Linux as Stubs
import qualified Stubs.Syscall.AArch32.Linux as Stubs
import qualified Stubs.Syscall.Names.AArch32.Linux as Stubs

import Grease.Macaw.Arch (ArchContext(..), ArchReloc, ArchRepr(ARMRepr))
import Grease.Macaw.Load.Relocation (RelocType(..))
import Grease.Macaw.RegName (RegName(..))
import Grease.Options (ExtraStackSlots)
import Grease.Shape.Pointer (armStackPtrShape)
import Grease.Utility (GreaseException(..))

type instance ArchReloc ARM.ARM = EE.ARM32_RelocationType

armCtx ::
  (?memOpts :: Mem.MemOptions) =>
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
    Nothing -> throw $ GreaseException "Failed to generate architecture-specific values"
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
      { _archRepr = ARMRepr
      , _archInfo = ARM.arm_linux_info
      , _archEndianness = Mem.LittleEndian
      , _archGetIP = \regs -> do
          let C.RV (Mem.LLVMPointer _base off) = ARM.Symbolic.lookupReg ARM.pc regs
          pure off
      , _archPcReg = ARM.pc
      , _archVals = avals
      , _archRelocSupported = armRelocSupported
      , _archIsGlobDatReloc = (== EE.R_ARM_GLOB_DAT)
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
      }

armRelocSupported :: EE.ARM32_RelocationType -> Maybe RelocType
armRelocSupported EE.R_ARM_RELATIVE = Just RelativeReloc
armRelocSupported EE.R_ARM_GLOB_DAT = Just SymbolReloc
armRelocSupported _ = Nothing
