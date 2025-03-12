{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Due to the orphan ArchReloc instance below

module Grease.Macaw.Arch.X86 (x86Ctx) where

import Control.Exception.Safe (throw)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Word (Word64)

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

-- macaw-x86
import qualified Data.Macaw.X86 as X86

-- stubs
import qualified Stubs.FunctionOverride.X86_64.Linux as Stubs
import qualified Stubs.Memory.X86_64.Linux as Stubs
import qualified Stubs.Syscall.X86_64.Linux as Stubs
import qualified Stubs.Syscall.Names.X86_64.Linux as Stubs

import Grease.Macaw.Arch (ArchContext(..), ArchReloc, ArchRepr(X86Repr))
import Grease.Macaw.Arch.X86.Reg (getX86Reg)
import Grease.Macaw.Load.Relocation (RelocType(..))
import Grease.Options (ExtraStackSlots)
import Grease.Shape.Pointer (x64StackPtrShape)
import Grease.Utility (GreaseException(..), bytes64LE)

type instance ArchReloc X86.X86_64 = EE.X86_64_RelocationType

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
    Nothing -> throw $ GreaseException "Failed to generate architecture-specific values"
    Just avals -> pure avals
  return
    ArchContext
      { _archRepr = X86Repr
      , _archInfo = X86.x86_64_linux_info
      , _archEndianness = Mem.LittleEndian
      , _archVals = avals
      , _archRelocSupported = x64RelocSupported
      , _archIsGlobDatReloc = (== EE.R_X86_64_GLOB_DAT)
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
         -- NB: x86-64 does not have a link register, so we don't need to
         -- override it.
      , _archRegOverrides = Map.empty
      }

x64RelocSupported :: EE.X86_64_RelocationType -> Maybe RelocType
x64RelocSupported EE.R_X86_64_RELATIVE = Just RelativeReloc
x64RelocSupported EE.R_X86_64_GLOB_DAT = Just SymbolReloc
x64RelocSupported _ = Nothing
