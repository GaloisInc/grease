{-# LANGUAGE DataKinds #-}

module Screach.LoadedELF (LoadedELF (..)) where

import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
-- BinaryLoader instance
import Data.Macaw.BinaryLoader.X86 ()
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Macaw.X86 qualified as MX86
import Data.Macaw.X86.Symbolic ()
import Data.Map.Strict qualified as Map
import What4.FunctionName as WFN

-- TODO we shouldnt bake MX86.X86_64
data LoadedELF w = LoadedELF
  { loadedElf :: Elf.ElfHeaderInfo 64
  , mem :: MM.Memory w
  , symMap :: Map.Map (MC.ArchSegmentOff MX86.X86_64) BS.ByteString
  , pltStubs :: Map.Map (MM.MemSegmentOff 64) FunctionName
  , dynFunMap :: Map.Map FunctionName (MM.MemSegmentOff 64)
  , entrypointAddr :: MM.MemSegmentOff 64
  , isECFS :: Bool
  , loadOptions :: LC.LoadOptions
  , relocs :: Map.Map (MM.MemWord 64) (Elf.X86_64_RelocationType, BS.ByteString)
  }
