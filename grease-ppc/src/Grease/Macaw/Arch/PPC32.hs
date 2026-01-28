{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Due to the orphan ArchReloc instance below
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.PPC32 (ppc32Ctx) where

import Control.Lens ((^.))
import Data.BitVector.Sized qualified as BV
import Data.ElfEdit qualified as EE
import Data.Macaw.PPC qualified as PPC
import Data.Macaw.PPC.Symbolic.Regs qualified as PPC.Symbolic.Regs
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word32)
import Dismantle.PPC qualified as D
import Grease.Macaw.Arch (ArchReloc, defaultPCFixup)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Load.Relocation (RelocType (RelativeReloc, SymbolReloc))
import Grease.Macaw.RegName (RegName (RegName))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (ppcStackPtrShape)
import Grease.Utility (bytes32LE)
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator.RegValue qualified as C
import Stubs.FunctionOverride.PPC.Linux qualified as Stubs
import Stubs.Memory.PPC.Linux qualified as Stubs
import Stubs.Syscall.Names.PPC32.Linux qualified as Stubs
import Stubs.Syscall.PPC.Linux qualified as Stubs

type instance ArchReloc PPC.PPC32 = EE.PPC32_RelocationType

ppc32Ctx ::
  ( ?memOpts :: CLM.MemOptions
  , CLM.HasPtrWidth 32
  ) =>
  -- | Initialize the end of the stack to a 'Word32' value (which is split into
  -- a little-endian sequence of 4 concrete bytes) if the value is @Just@.
  -- Otherwise, initialize the end of the stack to 4 fresh, symbolic bytes.
  --
  -- If the value is 'Just', also use the 'Word32' to override the initial value
  -- of the link register just before starting simulation.
  Maybe Word32 ->
  ExtraStackSlots ->
  IO (Arch.ArchContext PPC.PPC32)
ppc32Ctx mbReturnAddr stackArgSlots = do
  let extOverride = Stubs.ppcLinuxStmtExtensionOverride
  avals <- case Symbolic.genArchVals Proxy Proxy (Just extOverride) of
    Nothing ->
      -- `genArchVals` is total: https://github.com/GaloisInc/macaw/issues/231
      panic "ppc32Ctx" ["Failed to generate architecture-specific values"]
    Just avals -> pure avals
  let regOverrides =
        case mbReturnAddr of
          Just returnAddr ->
            Map.fromList
              [(RegName "lnk", BV.mkBV knownNat (fromIntegral returnAddr))]
          Nothing ->
            Map.empty
  return
    Arch.ArchContext
      { Arch._archInfo = PPC.ppc32_linux_info
      , Arch._archGetIP = \regs -> do
          let C.RV (CLM.LLVMPointer _base off) = regs ^. ixF' PPC.Symbolic.Regs.ip
          pure off
      , Arch._archPcReg = PPC.PPC_IP
      , Arch._archVals = avals
      , Arch._archRelocSupported = ppc32RelocSupported
      , Arch._archIntegerArguments = \bak ->
          Stubs.ppcLinuxIntegerArguments bak avals
      , Arch._archIntegerReturnRegisters = Stubs.ppcLinuxIntegerReturnRegisters
      , Arch._archFunctionReturnAddr = Stubs.ppcLinuxReturnAddr
      , Arch._archSyscallArgumentRegisters = Stubs.ppcLinuxSyscallArgumentRegisters PPC.V32Repr
      , Arch._archSyscallNumberRegister = Stubs.ppcLinuxSyscallNumberRegister
      , Arch._archSyscallReturnRegisters = Stubs.ppcLinuxSyscallReturnRegisters PPC.V32Repr
      , Arch._archSyscallCodeMapping = Stubs.syscallMap
      , Arch._archStackPtrShape = ppcStackPtrShape (bytes32LE <$> mbReturnAddr) stackArgSlots
      , Arch._archInitGlobals = \_ mem globals -> pure (mem, globals)
      , Arch._archRegOverrides = regOverrides
      , Arch._archOffsetStackPointerPostCall = pure
      , Arch._archABIParams =
          -- IBM Docs https://www.ibm.com/docs/en/aix/7.1.0?topic=overview-register-usage-conventions
          [ Some.Some (PPC.PPC_GP (D.GPR rnum))
          | rnum <- [3 .. 10]
          ]
      , Arch._archPCFixup = defaultPCFixup @PPC.PPC32 Proxy
      }

ppc32RelocSupported :: EE.PPC32_RelocationType -> Maybe RelocType
ppc32RelocSupported EE.R_PPC_RELATIVE = Just RelativeReloc
ppc32RelocSupported EE.R_PPC_GLOB_DAT = Just SymbolReloc
ppc32RelocSupported EE.R_PPC_ADDR32 = Just SymbolReloc
ppc32RelocSupported _ = Nothing
