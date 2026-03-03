-- | Custom information found in the @.personality@ section of an ECFS file.
module Data.ElfEdit.Ecfs.Personality (
  -- * The @Personality@ type
  Personality (..),
  defaultPersonality,

  -- * 'Personality' queries
  isStatic,
  isPie,
  hasLocalSymtab,
  hasMalwareHeuristics,
  hasStrippedShdrs,
) where

import Data.Bits (Bits (testBit))
import Data.Word (Word32)

-- | Custom information found in the @.personality@ section of an ECFS file.
newtype Personality = Personality {fromPersonality :: Word32}

-- | A default 'Personality' value where all queries return 'False'.
defaultPersonality :: Personality
defaultPersonality = Personality 0

-- | Return 'True' if the ELF file corresponding to the given 'Personality' is
-- statically linked (instead of dynamically).
isStatic :: Personality -> Bool
isStatic (Personality p) = p `testBit` 1

-- | Return 'True' if the ELF file corresponding to the given 'Personality' is a
-- position-independent executable (PIE).
isPie :: Personality -> Bool
isPie (Personality p) = p `testBit` 2

-- | Return 'True' if the ELF file corresponding to the given 'Personality' has
-- a local symbol table.
hasLocalSymtab :: Personality -> Bool
hasLocalSymtab (Personality p) = p `testBit` 3

-- | Return 'True' if the ELF file corresponding to the given 'Personality' uses
-- ECFS malware heuristics.
hasMalwareHeuristics :: Personality -> Bool
hasMalwareHeuristics (Personality p) = p `testBit` 4

-- | Return 'True' if the ELF file corresponding to the given 'Personality' has
-- stripped section headers.
hasStrippedShdrs :: Personality -> Bool
hasStrippedShdrs (Personality p) = p `testBit` 8
