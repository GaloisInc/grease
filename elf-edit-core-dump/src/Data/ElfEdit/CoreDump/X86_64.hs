{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | ELF core note information that is specific to x86-64. Much of this
-- information was inspired by the @elfutils@ source code:
-- <https://github.com/kushaldas/elfutils/blob/0b72b650b173fdf5467b337ff26e97e69daed869/backends/x86_64_corenote.c>
--
-- This module is meant to be imported qualified.
module Data.ElfEdit.CoreDump.X86_64
  ( X86_64UserRegs(..)
  , getX86_64UserRegs
  , x86_64UserRegsSize
  , x86_64PrRegOffset
  ) where

import Data.Binary.Get qualified as Get
import Data.Word (Word64)

import Data.ElfEdit qualified as Elf

-- | General-purpose x86-64 user registers.
data X86_64UserRegs = X86_64UserRegs
  { r15     :: !Word64
  , r14     :: !Word64
  , r13     :: !Word64
  , r12     :: !Word64
  , rbp     :: !Word64
  , rbx     :: !Word64
  , r11     :: !Word64
  , r10     :: !Word64
  , r9      :: !Word64
  , r8      :: !Word64
  , rax     :: !Word64
  , rcx     :: !Word64
  , rdx     :: !Word64
  , rsi     :: !Word64
  , rdi     :: !Word64
  , origRax :: !Word64
  , rip     :: !Word64
  , cs      :: !Word64
  , eflags  :: !Word64
  , rsp     :: !Word64
  , ss      :: !Word64
  , fsBase  :: !Word64
  , gsBase  :: !Word64
  , ds      :: !Word64
  , es      :: !Word64
  , fs      :: !Word64
  , gs      :: !Word64
  } deriving Show

-- | Parse an 'X64UserRegs' value.
getX86_64UserRegs :: Elf.ElfData -> Get.Get X86_64UserRegs
getX86_64UserRegs d = do
  r15     <- Elf.getWord64 d
  r14     <- Elf.getWord64 d
  r13     <- Elf.getWord64 d
  r12     <- Elf.getWord64 d
  rbp     <- Elf.getWord64 d
  rbx     <- Elf.getWord64 d
  r11     <- Elf.getWord64 d
  r10     <- Elf.getWord64 d
  r9      <- Elf.getWord64 d
  r8      <- Elf.getWord64 d
  rax     <- Elf.getWord64 d
  rcx     <- Elf.getWord64 d
  rdx     <- Elf.getWord64 d
  rsi     <- Elf.getWord64 d
  rdi     <- Elf.getWord64 d
  origRax <- Elf.getWord64 d
  rip     <- Elf.getWord64 d
  cs      <- Elf.getWord64 d
  eflags  <- Elf.getWord64 d
  rsp     <- Elf.getWord64 d
  ss      <- Elf.getWord64 d
  fsBase  <- Elf.getWord64 d
  gsBase  <- Elf.getWord64 d
  ds      <- Elf.getWord64 d
  es      <- Elf.getWord64 d
  fs      <- Elf.getWord64 d
  gs      <- Elf.getWord64 d
  pure X86_64UserRegs{..}

-- | The size of an 'X86_64UserRegs' value in bytes. An 'X86_64UserRegs' value
-- consists of twenty-seven 'Word64' values, so twenty-seven 8-byte values.
x86_64UserRegsSize :: Word64
x86_64UserRegsSize = 27 * 8

-- | The offset of the @pr_reg@ field (in bytes) in @struct elf_prstatus@ on an
-- x86-64 machine.
x86_64PrRegOffset :: Word64
x86_64PrRegOffset = 112
