{-# LANGUAGE DataKinds #-}

module Screach.LoadedELF (LoadedELF (..)) where

import Data.ElfEdit qualified as Elf
-- BinaryLoader instance
import Data.Macaw.BinaryLoader.X86 ()
import Data.Macaw.Memory qualified as MM
import Data.Macaw.X86 qualified as MX86
import Data.Macaw.X86.Symbolic ()
import Grease.Macaw.Load (LoadedProgram)

-- TODO we shouldnt bake MX86.X86_64
data LoadedELF = LoadedELF
  { loadedElf :: Elf.ElfHeaderInfo 64
  , loadedProgram :: LoadedProgram MX86.X86_64
  , entrypointAddr :: MM.MemSegmentOff 64
  , isECFS :: Bool
  }
