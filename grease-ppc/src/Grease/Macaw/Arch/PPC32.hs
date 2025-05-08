{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Due to the orphan ArchReloc instance below

module Grease.Macaw.Arch.PPC32 (ppc32Ctx) where

import Control.Exception.Safe (throw)
import Data.Map qualified as Map
import Data.Proxy (Proxy(..))
import Data.Word (Word32)

-- bv-sized
import Data.BitVector.Sized qualified as BV

-- parameterized-utils
import Data.Parameterized.NatRepr (knownNat)

-- crucible
import Lang.Crucible.Simulator.RegValue qualified as C

-- crucible-llvm
import Lang.Crucible.LLVM.MemModel qualified as Mem

-- elf-edit
import Data.ElfEdit qualified as EE

-- macaw-symbolic
import Data.Macaw.Symbolic qualified as Symbolic

-- macaw-ppc
import Data.Macaw.PPC qualified as PPC

-- macaw-ppc-symbolic
import Data.Macaw.PPC.Symbolic.Regs qualified as PPC.Symbolic.Regs

-- stubs
import Stubs.FunctionOverride.PPC.Linux qualified as Stubs
import Stubs.Memory.PPC.Linux qualified as Stubs
import Stubs.Syscall.PPC.Linux qualified as Stubs
import Stubs.Syscall.Names.PPC32.Linux qualified as Stubs

import Grease.Macaw.Arch (ArchContext(..), ArchReloc)
import Grease.Macaw.Load.Relocation (RelocType(..))
import Grease.Macaw.RegName (RegName(..))
import Grease.Options (ExtraStackSlots)
import Grease.Shape.Pointer (ppcStackPtrShape)
import Grease.Utility (GreaseException(..), bytes32LE)

type instance ArchReloc PPC.PPC32 = EE.PPC32_RelocationType

ppc32Ctx ::
  ( ?memOpts :: Mem.MemOptions
  , Mem.HasPtrWidth 32
  ) =>
  -- | Initialize the end of the stack to a 'Word32' value (which is split into
  -- a little-endian sequence of 4 concrete bytes) if the value is @Just@.
  -- Otherwise, initialize the end of the stack to 4 fresh, symbolic bytes.
  --
  -- If the value is 'Just', also use the 'Word32' to override the initial value
  -- of the link register just before starting simulation.
  Maybe Word32 ->
  ExtraStackSlots ->
  IO (ArchContext PPC.PPC32)
ppc32Ctx mbReturnAddr stackArgSlots = do
  let extOverride = Stubs.ppcLinuxStmtExtensionOverride
  avals <- case Symbolic.genArchVals Proxy Proxy (Just extOverride) of
    Nothing -> throw $ GreaseException "Failed to generate architecture-specific values"
    Just avals -> pure avals
  let regOverrides =
        case mbReturnAddr of
          Just returnAddr ->
            Map.fromList
              [(RegName "lnk", BV.mkBV knownNat (fromIntegral returnAddr))]
          Nothing ->
            Map.empty
  return
    ArchContext
      { _archInfo = PPC.ppc32_linux_info
      , _archGetIP = \regs -> do
          C.RV (Mem.LLVMPointer _base off) <- PPC.Symbolic.Regs.lookupReg PPC.PPC_IP regs
          pure off
      , _archPcReg = PPC.PPC_IP
      , _archVals = avals
      , _archRelocSupported = ppc32RelocSupported
      , _archIntegerArguments = \bak ->
          Stubs.ppcLinuxIntegerArguments bak avals
      , _archIntegerReturnRegisters = Stubs.ppcLinuxIntegerReturnRegisters
      , _archFunctionReturnAddr = Stubs.ppcLinuxReturnAddr
      , _archSyscallArgumentRegisters = Stubs.ppcLinuxSyscallArgumentRegisters PPC.V32Repr
      , _archSyscallNumberRegister = Stubs.ppcLinuxSyscallNumberRegister
      , _archSyscallReturnRegisters = Stubs.ppcLinuxSyscallReturnRegisters PPC.V32Repr
      , _archSyscallCodeMapping = Stubs.syscallMap
      , _archStackPtrShape = ppcStackPtrShape (bytes32LE <$> mbReturnAddr) stackArgSlots
      , _archInitGlobals = \_ mem globals -> pure (mem, globals)
      , _archRegOverrides = regOverrides
      , _archOffsetStackPointerPostCall = pure
      }

ppc32RelocSupported :: EE.PPC32_RelocationType -> Maybe RelocType
ppc32RelocSupported EE.R_PPC_RELATIVE = Just RelativeReloc
ppc32RelocSupported EE.R_PPC_GLOB_DAT = Just SymbolReloc
ppc32RelocSupported EE.R_PPC_ADDR32 = Just SymbolReloc
ppc32RelocSupported _ = Nothing
