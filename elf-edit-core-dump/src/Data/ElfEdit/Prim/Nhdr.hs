{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Note headers.
module Data.ElfEdit.Prim.Nhdr
  ( -- * Note headers
    Nhdr(..)
  , nhdrSize
    -- * Note header type
  , NhdrType(..)
  , pattern NT_PRSTATUS
  , pattern NT_FPREGSET
  , pattern NT_PRPSINFO
  ) where

import Data.Map qualified as Map
import Data.Word (Word32)
import Numeric (showHex)

-- | Note header information. This corresponds to the @Elf{32,64}_Nhdr@ structs.
-- See also the following documentation about note sections, on which these
-- Haddocks are based:
-- <https://refspecs.linuxbase.org/elf/gabi4+/ch5.pheader.html#note_section>
data Nhdr = Nhdr
  { nhdrNameSize :: !Word32
    -- ^ The number of bytes used to store a note's name (including a byte for a
    -- null terminator). This does /not/ include the size of padding bytes
    -- needed ensure that the name uses 4-byte alignment (if using
    -- 'Elf.ELFCLASS32') or 8-byte alignment (if using 'Elf.ELFCLASS64').
  , nhdrDescSize :: !Word32
    -- ^ The number of bytes used to store a note's descriptor. This does /not/
    -- include the size of padding bytes needed ensure that the name uses 4-byte
    -- alignment (on 32-bit ELF objects) or 8-byte alignment (on 64-bit ELF
    -- objects).
  , nhdrType :: !NhdrType
    -- ^ The interpretation of the descriptor. Note that multiple
    -- interpretations of a single 'NhdrType' value may exist. Thus, a program
    -- must recognize both the 'nhdrNameSize' and the 'nhdrType' to recognize a
    -- descriptor.
  } deriving Show

-- | The size of an 'Nhdr' value in bytes. An 'Nhdr' value consists of three
-- 'Word32' values, so three 4-byte values.
nhdrSize :: Int
nhdrSize = 3 * 4

-- | The type of an ELF note.
newtype NhdrType = NhdrType { fromNhdrType :: Word32 }
  deriving (Eq, Ord)

-- The numbers below were derived from linux/elf.h:
-- https://github.com/torvalds/linux/blob/69b8923f5003664e3ffef102e73333edfa2abdcf/include/uapi/linux/elf.h#L371-L464

-- | Contains copy of @prstatus@ struct
pattern NT_PRSTATUS :: NhdrType
pattern NT_PRSTATUS = NhdrType 1

-- | Contains copy of @fpregset@ struct
pattern NT_FPREGSET :: NhdrType
pattern NT_FPREGSET = NhdrType 2

-- | Contains copy of @prpsinfo@ struct
pattern NT_PRPSINFO :: NhdrType
pattern NT_PRPSINFO = NhdrType 3

-- There are many more NhdrType values, but we won't bother with them for now

nhdrTypeNameMap :: Map.Map NhdrType String
nhdrTypeNameMap = Map.fromList
  [ (NT_PRSTATUS, "PRSTATUS")
  , (NT_FPREGSET, "FPREGSET")
  , (NT_PRPSINFO, "PRPSINFO")
  ]

instance Show NhdrType where
  show tp =
    case Map.lookup tp nhdrTypeNameMap of
      Just s -> "NT_" ++ s
      Nothing -> "0x" ++ showHex (fromNhdrType tp) ""
