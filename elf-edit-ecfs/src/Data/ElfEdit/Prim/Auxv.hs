{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The auxiliary vector.
module Data.ElfEdit.Prim.Auxv (
  -- * Auxiliary vectory entries
  AuxvEntry (..),
  auxvEntrySize,

  -- * Auxiliary vector types
  AuxvType (..),
  pattern AT_NULL,
  pattern AT_IGNORE,
  pattern AT_EXECFD,
  pattern AT_PHDR,
  pattern AT_PHENT,
  pattern AT_PHNUM,
  pattern AT_PAGESZ,
  pattern AT_BASE,
  pattern AT_FLAGS,
  pattern AT_ENTRY,
  pattern AT_UID,
  pattern AT_EUID,
  pattern AT_GID,
) where

import qualified Data.ElfEdit as Elf
import qualified Data.Map as Map
import Numeric (showHex)

-- | An entry in the auxiliary vector. This corresponds to the
-- @Elf{32,64}_auxv_t@ structs, as defined here in @libelf@:
-- <https://github.com/kushaldas/elfutils/blob/0b72b650b173fdf5467b337ff26e97e69daed869/libelf/elf.h#L913-L944>
data AuxvEntry w = AuxvEntry
  { auxvEntryType :: !(AuxvType w)
  -- ^ The entry type.
  , auxvEntryVal :: !(Elf.ElfWordType w)
  -- ^ An integer value of the appropriate 'AuxvType'.
  }

deriving instance Elf.ElfWidthConstraints w => Show (AuxvEntry w)

-- | The size of an 'Elf.Prim.AuxvEntry' value in bytes. An 'Elf.Prim.AuxvEntry'
-- value consists of two ELF words.
auxvEntrySize :: Elf.ElfClass w -> Int
auxvEntrySize cl = 2 * Elf.elfClassByteWidth cl

-- | The type of an entry in the auxiliary vector.
newtype AuxvType w = AuxvType {fromAuxvType :: Elf.ElfWordType w}

deriving instance Eq (Elf.ElfWordType w) => Eq (AuxvType w)
deriving instance Ord (Elf.ElfWordType w) => Ord (AuxvType w)

-- The numbers below were derived from linux/auxvec.h:
-- https://github.com/torvalds/linux/blob/0af2f6be1b4281385b618cb86ad946eded089ac8/include/uapi/linux/auxvec.h#L7-L42

-- | End of vector
pattern AT_NULL ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_NULL = AuxvType 0

-- | Entry should be ignored
pattern AT_IGNORE ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_IGNORE = AuxvType 1

-- | File descriptor of program
pattern AT_EXECFD ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_EXECFD = AuxvType 2

-- | Program headers for program
pattern AT_PHDR ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_PHDR = AuxvType 3

-- | Size of program header entry
pattern AT_PHENT ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_PHENT = AuxvType 4

-- | Number of program headers
pattern AT_PHNUM ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_PHNUM = AuxvType 5

-- | System page size
pattern AT_PAGESZ ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_PAGESZ = AuxvType 6

-- | Base address of interpreter
pattern AT_BASE ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_BASE = AuxvType 7

-- | Flags
pattern AT_FLAGS ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_FLAGS = AuxvType 8

-- | Entry point of program
pattern AT_ENTRY ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_ENTRY = AuxvType 9

-- | Real uid
pattern AT_UID ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_UID = AuxvType 11

-- | Effective uid
pattern AT_EUID ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_EUID = AuxvType 12

-- | Real gid
pattern AT_GID ::
  (Eq (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
  AuxvType w
pattern AT_GID = AuxvType 13

-- There are many more AuxvType values, but we won't bother with them for now

auxvTypeNameMap :: Elf.ElfWidthConstraints w => Map.Map (AuxvType w) String
auxvTypeNameMap =
  Map.fromList
    [ (AT_NULL, "NULL")
    , (AT_IGNORE, "IGNORE")
    , (AT_EXECFD, "EXECFD")
    , (AT_PHDR, "PHDR")
    , (AT_PHENT, "PHENT")
    , (AT_PHNUM, "PHNUM")
    , (AT_PAGESZ, "PAGESZ")
    , (AT_BASE, "BASE")
    , (AT_FLAGS, "FLAGS")
    , (AT_ENTRY, "ENTRY")
    , (AT_UID, "UID")
    , (AT_EUID, "EUID")
    , (AT_GID, "GID")
    ]

instance Elf.ElfWidthConstraints w => Show (AuxvType w) where
  show tp =
    case Map.lookup tp auxvTypeNameMap of
      Just s -> "AT_" ++ s
      Nothing -> "0x" ++ showHex (fromAuxvType tp) ""
