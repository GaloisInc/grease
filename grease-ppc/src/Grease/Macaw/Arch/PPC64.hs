{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Due to the orphan ArchReloc instance below
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.PPC64 (ppc64Ctx) where

import Control.Lens ((^.))
import Data.BitVector.Sized qualified as BV
import Data.ElfEdit qualified as EE
import Data.Macaw.BinaryLoader qualified as Loader
import Data.Macaw.PPC qualified as PPC
import Data.Macaw.PPC.Symbolic.Regs qualified as PPC.Symbolic.Regs
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some qualified as Some
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import Dismantle.PPC qualified as D
import Grease.Macaw.Arch (ArchContext (..), ArchReloc)
import Grease.Macaw.Load.Relocation (RelocType (..))
import Grease.Macaw.RegName (RegName (..))
import Grease.Options (ExtraStackSlots)
import Grease.Panic (panic)
import Grease.Shape.Pointer (ppcStackPtrShape)
import Grease.Utility (bytes64LE)
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Simulator.RegValue qualified as C
import Stubs.FunctionOverride.PPC.Linux qualified as Stubs
import Stubs.Memory.PPC.Linux qualified as Stubs
import Stubs.Syscall.Names.PPC64.Linux qualified as Stubs
import Stubs.Syscall.PPC.Linux qualified as Stubs

type instance ArchReloc PPC.PPC64 = EE.PPC64_RelocationType

-- | Note that unlike @ppc32Ctx@ in "Grease.Macaw.Arch.PPC32", 'ppc64Ctx'
-- requires a 'Loader.LoadedBinary' argument in order to create its 'archInfo'.
-- As such, it is not possible to create a 'ppc64Ctx' value without a binary,
-- making it unsuited for Crucible S-expression contexts. (See
-- <https://github.com/GaloisInc/macaw/issues/415>.)
ppc64Ctx ::
  ( ?memOpts :: Mem.MemOptions
  , Mem.HasPtrWidth 64
  ) =>
  -- | Initialize the end of the stack to a 'Word64' value (which is split into
  -- a little-endian sequence of 8 concrete bytes) if the value is @Just@.
  -- Otherwise, initialize the end of the stack to 8 fresh, symbolic bytes.
  --
  -- If the value is 'Just', also use the 'Word64' to override the initial value
  -- of the link register just before starting simulation.
  Maybe Word64 ->
  ExtraStackSlots ->
  Loader.LoadedBinary PPC.PPC64 (EE.ElfHeaderInfo 64) ->
  IO (ArchContext PPC.PPC64)
ppc64Ctx mbReturnAddr stackArgSlots loadedBinary = do
  let extOverride = Stubs.ppcLinuxStmtExtensionOverride
  avals <- case Symbolic.genArchVals Proxy Proxy (Just extOverride) of
    Nothing ->
      -- `genArchVals` is total: https://github.com/GaloisInc/macaw/issues/231
      panic "ppc64Ctx" ["Failed to generate architecture-specific values"]
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
      { _archInfo = PPC.ppc64_linux_info loadedBinary
      , _archGetIP = \regs -> do
          let C.RV (Mem.LLVMPointer _base off) = regs ^. ixF' PPC.Symbolic.Regs.ip
          pure off
      , _archPcReg = PPC.PPC_IP
      , _archVals = avals
      , _archRelocSupported = ppc64RelocSupported
      , _archIntegerArguments = \bak ->
          Stubs.ppcLinuxIntegerArguments bak avals
      , _archIntegerReturnRegisters = Stubs.ppcLinuxIntegerReturnRegisters
      , _archFunctionReturnAddr = Stubs.ppcLinuxReturnAddr
      , _archSyscallArgumentRegisters = Stubs.ppcLinuxSyscallArgumentRegisters PPC.V64Repr
      , _archSyscallNumberRegister = Stubs.ppcLinuxSyscallNumberRegister
      , _archSyscallReturnRegisters = Stubs.ppcLinuxSyscallReturnRegisters PPC.V64Repr
      , _archSyscallCodeMapping = Stubs.syscallMap
      , _archStackPtrShape = ppcStackPtrShape (bytes64LE <$> mbReturnAddr) stackArgSlots
      , _archInitGlobals = \_ mem globals -> pure (mem, globals)
      , _archRegOverrides = regOverrides
      , _archOffsetStackPointerPostCall = pure
      , _archABIParams =
          -- IBM Docs https://www.ibm.com/docs/en/aix/7.1.0?topic=overview-register-usage-conventions
          [ Some.Some (PPC.PPC_GP (D.GPR rnum))
          | rnum <- [3 .. 10]
          ]
      }

ppc64RelocSupported :: EE.PPC64_RelocationType -> Maybe RelocType
ppc64RelocSupported EE.R_PPC64_RELATIVE = Just RelativeReloc
ppc64RelocSupported EE.R_PPC64_GLOB_DAT = Just SymbolReloc
ppc64RelocSupported EE.R_PPC64_ADDR64 = Just SymbolReloc
ppc64RelocSupported _ = Nothing
