{-# LANGUAGE DataKinds #-}

module Grease.Macaw.BinMd (
  BinMd (..),
  emptyBinMd,
) where

import Data.ByteString qualified as BS
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Grease.Entrypoint (Entrypoint)
import Grease.Macaw.Arch qualified as Arch
import What4.FunctionName qualified as WFN

-- | Binary metadata: all per-binary maps and settings, but not memory
-- (which can be recovered via 'Loader.memoryImage' from 'LoadedProgram').
data BinMd arch
  = BinMd
  { binLoadOptions :: LC.LoadOptions
  -- ^ The load options used to load addresses in the binary.
  , binSymMap :: Map (MC.ArchSegmentOff arch) BS.ByteString
  -- ^ A map of all function addresses to their symbol names. Note that it
  -- is possible for a single function address to have multiple function
  -- symbols (https://github.com/GaloisInc/macaw-loader/issues/25), so this
  -- map will arbitrarily pick one of the symbol names.
  , binPltStubs :: Map (MC.ArchSegmentOff arch) WFN.FunctionName
  -- ^ Map of PLT stub addresses to their function names.
  , binDynFunMap :: Map WFN.FunctionName (MC.ArchSegmentOff arch)
  -- ^ A map of visible, dynamic function symbol names (UTF-8–encoded) to
  -- their corresponding function addresses.
  , binRelocs :: Map (MM.MemWord (MC.ArchAddrWidth arch)) (Arch.ArchReloc arch)
  -- ^ Map of relocation addresses to their types.
  , binEntrypointAddrs :: Map Entrypoint (MC.ArchSegmentOff arch)
  -- ^ The entrypoint addresses after resolving the user-supplied
  -- 'Entrypoint'.
  }

-- | An empty 'BinMd' for use in paths that don't load a binary
-- (e.g., Crucible S-expression programs).
emptyBinMd :: BinMd arch
emptyBinMd =
  BinMd
    { binLoadOptions = LC.defaultLoadOptions
    , binSymMap = Map.empty
    , binPltStubs = Map.empty
    , binDynFunMap = Map.empty
    , binRelocs = Map.empty
    , binEntrypointAddrs = Map.empty
    }
