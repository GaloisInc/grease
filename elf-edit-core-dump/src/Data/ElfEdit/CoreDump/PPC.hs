{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ELF core note information that is specific to PPC. Much of this
-- information was inspired by the @elfutils@ source code:
-- <https://github.com/kushaldas/elfutils/blob/0b72b650b173fdf5467b337ff26e97e69daed869/backends/ppc_corenote.c>
--
-- This module is meant to be imported qualified.
module Data.ElfEdit.CoreDump.PPC
  ( PpcUserRegs(..)
  , getPpcUserRegs
  , ppcUserRegsSize
  , ppcPrRegOffset
  ) where

import qualified Data.Binary.Get as Get

import qualified Data.ElfEdit as Elf

-- | General-purpose PPC user registers.
data PpcUserRegs w = PpcUserRegs
  { r0        :: !(Elf.ElfWordType w)
  , r1        :: !(Elf.ElfWordType w)
  , r2        :: !(Elf.ElfWordType w)
  , r3        :: !(Elf.ElfWordType w)
  , r4        :: !(Elf.ElfWordType w)
  , r5        :: !(Elf.ElfWordType w)
  , r6        :: !(Elf.ElfWordType w)
  , r7        :: !(Elf.ElfWordType w)
  , r8        :: !(Elf.ElfWordType w)
  , r9        :: !(Elf.ElfWordType w)
  , r10       :: !(Elf.ElfWordType w)
  , r11       :: !(Elf.ElfWordType w)
  , r12       :: !(Elf.ElfWordType w)
  , r13       :: !(Elf.ElfWordType w)
  , r14       :: !(Elf.ElfWordType w)
  , r15       :: !(Elf.ElfWordType w)
  , r16       :: !(Elf.ElfWordType w)
  , r17       :: !(Elf.ElfWordType w)
  , r18       :: !(Elf.ElfWordType w)
  , r19       :: !(Elf.ElfWordType w)
  , r20       :: !(Elf.ElfWordType w)
  , r21       :: !(Elf.ElfWordType w)
  , r22       :: !(Elf.ElfWordType w)
  , r23       :: !(Elf.ElfWordType w)
  , r24       :: !(Elf.ElfWordType w)
  , r25       :: !(Elf.ElfWordType w)
  , r26       :: !(Elf.ElfWordType w)
  , r27       :: !(Elf.ElfWordType w)
  , r28       :: !(Elf.ElfWordType w)
  , r29       :: !(Elf.ElfWordType w)
  , r30       :: !(Elf.ElfWordType w)
  , r31       :: !(Elf.ElfWordType w)
  , nip       :: !(Elf.ElfWordType w)
  , msr       :: !(Elf.ElfWordType w)
  , orig_gpr3 :: !(Elf.ElfWordType w)
  , ctr       :: !(Elf.ElfWordType w)
  , lr        :: !(Elf.ElfWordType w)
  , xer       :: !(Elf.ElfWordType w)
  , cr        :: !(Elf.ElfWordType w)
  , mq        :: !(Elf.ElfWordType w)
  , trap      :: !(Elf.ElfWordType w)
  , dar       :: !(Elf.ElfWordType w)
  , dsisr     :: !(Elf.ElfWordType w)
  }
deriving instance Show (Elf.ElfWordType w) => Show (PpcUserRegs w)

-- | Parse a 'PpcUserRegs' value.
getPpcUserRegs ::
  forall w.
  Elf.ElfClass w ->
  Elf.ElfData ->
  Get.Get (PpcUserRegs w)
getPpcUserRegs cl d = do
  r0        <- getElfWord
  r1        <- getElfWord
  r2        <- getElfWord
  r3        <- getElfWord
  r4        <- getElfWord
  r5        <- getElfWord
  r6        <- getElfWord
  r7        <- getElfWord
  r8        <- getElfWord
  r9        <- getElfWord
  r10       <- getElfWord
  r11       <- getElfWord
  r12       <- getElfWord
  r13       <- getElfWord
  r14       <- getElfWord
  r15       <- getElfWord
  r16       <- getElfWord
  r17       <- getElfWord
  r18       <- getElfWord
  r19       <- getElfWord
  r20       <- getElfWord
  r21       <- getElfWord
  r22       <- getElfWord
  r23       <- getElfWord
  r24       <- getElfWord
  r25       <- getElfWord
  r26       <- getElfWord
  r27       <- getElfWord
  r28       <- getElfWord
  r29       <- getElfWord
  r30       <- getElfWord
  r31       <- getElfWord
  nip       <- getElfWord
  msr       <- getElfWord
  orig_gpr3 <- getElfWord
  ctr       <- getElfWord
  lr        <- getElfWord
  xer       <- getElfWord
  cr        <- getElfWord
  mq        <- getElfWord
  trap      <- getElfWord
  dar       <- getElfWord
  dsisr     <- getElfWord
  pure PpcUserRegs{..}
  where
    getElfWord :: Get.Get (Elf.ElfWordType w)
    getElfWord =
      case cl of
        Elf.ELFCLASS32 -> Elf.getWord32 d
        Elf.ELFCLASS64 -> Elf.getWord64 d

-- | The size of a 'PpcUserRegs' value in bytes. (There are fewer than 48
-- fields, but I believe ELF rounds it up to 48 to satisfy alignment
-- requirements.)
ppcUserRegsSize :: Elf.ElfClass w -> Elf.ElfWordType w
ppcUserRegsSize cl = Elf.elfClassInstances cl 48

-- | The offset of the @pr_reg@ field (in bytes) in @struct elf_prstatus@ on a
-- PPC machine.
ppcPrRegOffset :: forall w. Elf.ElfClass w -> Elf.ElfWordType w
ppcPrRegOffset cl =
  Elf.elfClassInstances cl $
  let longBytes :: Elf.ElfWordType w
      longBytes =
        case cl of
          Elf.ELFCLASS32 -> 4
          Elf.ELFCLASS64 -> 8 in
  32 + 8 * longBytes
