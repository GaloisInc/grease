{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Due to the orphan ArchReloc instance below

module Grease.Macaw.Arch.PPC64 (ppc64Ctx) where

import Control.Exception.Safe (throw)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Word (Word64)

-- bv-sized
import qualified Data.BitVector.Sized as BV

-- parameterized-utils
import Data.Parameterized.NatRepr (knownNat)

-- crucible
import qualified Lang.Crucible.Simulator.GlobalState as C
import qualified Lang.Crucible.Simulator.RegValue as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.MemModel as Mem
import qualified Lang.Crucible.LLVM.DataLayout as Mem

-- elf-edit
import qualified Data.ElfEdit as EE

-- macaw-loader
import qualified Data.Macaw.BinaryLoader as Loader

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- macaw-ppc
import qualified Data.Macaw.PPC as PPC

-- macaw-ppc-symbolic
import qualified Data.Macaw.PPC.Symbolic.Regs as PPC.Symbolic.Regs

-- stubs
import qualified Stubs.FunctionOverride.PPC.Linux as Stubs
import qualified Stubs.Memory.PPC.Linux as Stubs
import qualified Stubs.Syscall.PPC.Linux as Stubs
import qualified Stubs.Syscall.Names.PPC64.Linux as Stubs

import Grease.Macaw.Arch (ArchContext(..), ArchReloc, ArchRepr(PPC64Repr))
import Grease.Macaw.Load.Relocation (RelocType(..))
import Grease.Macaw.RegName (RegName(..))
import Grease.Options (ExtraStackSlots)
import Grease.Shape.Pointer (ppcStackPtrShape)
import Grease.Utility (GreaseException(..), bytes64LE)

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
      { _archRepr = PPC64Repr
      , _archInfo = PPC.ppc64_linux_info loadedBinary
      , _archEndianness = Mem.BigEndian
      , _archGetIP = \regs -> do
          C.RV (Mem.LLVMPointer _base off) <- PPC.Symbolic.Regs.lookupReg PPC.PPC_IP regs
          pure off
      , _archPcReg = PPC.PPC_IP
      , _archVals = avals
      , _archRelocSupported = ppc64RelocSupported
      , _archIsGlobDatReloc = (== EE.R_PPC64_GLOB_DAT)
      , _archIntegerArguments = \bak ->
          Stubs.ppcLinuxIntegerArguments bak avals
      , _archIntegerReturnRegisters = Stubs.ppcLinuxIntegerReturnRegisters
      , _archFunctionReturnAddr = Stubs.ppcLinuxReturnAddr
      , _archSyscallArgumentRegisters = Stubs.ppcLinuxSyscallArgumentRegisters PPC.V64Repr
      , _archSyscallNumberRegister = Stubs.ppcLinuxSyscallNumberRegister
      , _archSyscallReturnRegisters = Stubs.ppcLinuxSyscallReturnRegisters PPC.V64Repr
      , _archSyscallCodeMapping = Stubs.syscallMap
      , _archStackPtrShape = ppcStackPtrShape (bytes64LE <$> mbReturnAddr) stackArgSlots
      , _archInitGlobals = \_ mem -> pure (mem, C.emptyGlobals)
      , _archRegOverrides = regOverrides
      , _archOffsetStackPointerPostCall = pure
      }

ppc64RelocSupported :: EE.PPC64_RelocationType -> Maybe RelocType
ppc64RelocSupported EE.R_PPC64_RELATIVE = Just RelativeReloc
ppc64RelocSupported EE.R_PPC64_GLOB_DAT = Just SymbolReloc
ppc64RelocSupported _ = Nothing
