{-# LANGUAGE RecordWildCards #-}

-- | ELF core note information that is specific to ARM. Much of this
-- information was inspired by the @elfutils@ source code:
-- <https://github.com/kushaldas/elfutils/blob/0b72b650b173fdf5467b337ff26e97e69daed869/backends/arm_corenote.c>
--
-- This module is meant to be imported qualified.
module Data.ElfEdit.CoreDump.ARM
  ( ArmUserRegs(..)
  , getArmUserRegs
  , armUserRegsSize
  , armPrRegOffset
  ) where

import qualified Data.Binary.Get as Get
import Data.Word (Word32)

import qualified Data.ElfEdit as Elf

-- | General-purpose 32-bit ARM user registers.
data ArmUserRegs = ArmUserRegs
  { r0     :: !Word32
  , r1     :: !Word32
  , r2     :: !Word32
  , r3     :: !Word32
  , r4     :: !Word32
  , r5     :: !Word32
  , r6     :: !Word32
  , r7     :: !Word32
  , r8     :: !Word32
  , r9     :: !Word32
  , r10    :: !Word32
  , r11    :: !Word32
  , r12    :: !Word32
  , r13    :: !Word32
  , r14    :: !Word32
  , r15    :: !Word32
  , cpsr   :: !Word32
  , origR0 :: !Word32
  } deriving Show

-- | Parse an 'X64UserRegs' value.
getArmUserRegs :: Elf.ElfData -> Get.Get ArmUserRegs
getArmUserRegs d = do
  r0     <- Elf.getWord32 d
  r1     <- Elf.getWord32 d
  r2     <- Elf.getWord32 d
  r3     <- Elf.getWord32 d
  r4     <- Elf.getWord32 d
  r5     <- Elf.getWord32 d
  r6     <- Elf.getWord32 d
  r7     <- Elf.getWord32 d
  r8     <- Elf.getWord32 d
  r9     <- Elf.getWord32 d
  r10    <- Elf.getWord32 d
  r11    <- Elf.getWord32 d
  r12    <- Elf.getWord32 d
  r13    <- Elf.getWord32 d
  r14    <- Elf.getWord32 d
  r15    <- Elf.getWord32 d
  cpsr   <- Elf.getWord32 d
  origR0 <- Elf.getWord32 d
  pure ArmUserRegs{..}

-- | The size of an 'ArmUserRegs' value in bytes. An 'ArmUserRegs' value
-- consists of eighteen 'Word32' values, so eighteen 4-byte values.
armUserRegsSize :: Word32
armUserRegsSize = 18 * 4

-- | The offset of the @pr_reg@ field (in bytes) in @struct elf_prstatus@ on a
-- 32-bit ARM machine.
armPrRegOffset :: Word32
armPrRegOffset = 72
