{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Support for ECFS (Extended Core File Snapshot) coredump files
module Grease.Macaw.Ecfs (
  hasEcfsMagic,
  findEcfsPltStubs,
  findEcfsDynFunAddrs,
  EcfsError (..),
) where

import Control.Exception qualified as X
import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.ElfEdit.Ecfs qualified as Ecfs
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vec
import Data.Word (Word64)
import Grease.Macaw.PLT qualified as GMP
import Numeric (showHex)
import Prettyprinter qualified as PP

-- | Check whether a 'BS.ByteString' has the ECFS magic bytes at the @EI_PAD@
-- offset (9) in the ELF header. This is a cheap O(1) check that does not do
-- a full parse, suitable for dispatch before committing to a decode path.
hasEcfsMagic :: BS.ByteString -> Bool
hasEcfsMagic bs = BS.take 4 (BS.drop 9 bs) == "ECFS"

-- | Error type for ECFS PLT stub discovery
data EcfsError
  = UnknownPltStub
      Word64 -- The PLT stub's address
      Word64 -- The expected address of the PLT stub's entry in .dynsym

instance X.Exception EcfsError

instance Show EcfsError where
  show = show . PP.pretty

instance PP.Pretty EcfsError where
  pretty =
    \case
      UnknownPltStub pltEntryAddr pltShlAddr ->
        PP.vcat
          [ "Could not find ECFS PLT stub in .dynsym"
          , "PLT stub address:"
              PP.<+> PP.pretty (showHex pltEntryAddr "")
          , "Expected .dynsym entry address:"
              PP.<+> PP.pretty (showHex pltShlAddr "")
          ]

-- | Find all of the PLT stubs in an ECFS file. Unlike for raw ELF binaries,
-- where we rely on heuristics to discover PLT stubs, ECFS files store the
-- locations of all PLT stubs as metadata, which we consult in the
-- implementation of this function.
--
-- If an ECFS file lacks PLT information or a dynamic symbol table, then this
-- function returns an empty list.
findEcfsPltStubs :: forall w. MML.LoadOptions -> Ecfs.Ecfs w -> [GMP.PltStub]
findEcfsPltStubs loadOpts ecfs =
  case (Ecfs.decodePltGotInfo ecfs, Ecfs.ecfsDynsym ecfs) of
    (Just pltGotInfos, Just dynsym) ->
      Vec.toList $
        Vec.map (lookupDynsym dynsym) pltGotInfos
    (_, _) ->
      []
 where
  ehi = Ecfs.ecfsElfHeaderInfo ecfs
  eh = Elf.header ehi
  cl = Elf.headerClass eh

  -- Look up a PltGotInfo's PLT stub name in the dynamic symbol table and
  -- return a PltStub with the name and address of the PLT stub. This assumes
  -- the precondition that the PLT stub name is located in the dynamic symbol
  -- table at the expected address, and this function will raise an error if
  -- this is not the case.
  lookupDynsym :: Elf.Symtab w -> Ecfs.PltGotInfo -> GMP.PltStub
  lookupDynsym dynsym pltGotInfo =
    let loadOffset = fromMaybe 0 (MML.loadOffset loadOpts)
        pltEntryAddr = Ecfs.pltEntryVirtAddr pltGotInfo
        loadedPltEntryAddr = pltEntryAddr + loadOffset
        pltShlAddr = Ecfs.shlEntryVirtAddr pltGotInfo
        loadedPltShlAddr = pltShlAddr + loadOffset
        mbPltDynsymEntry =
          Elf.elfClassInstances cl $
            Vec.find
              ( \dynsymEntry ->
                  fromIntegral @(Elf.ElfWordType w) @Word64 (Elf.steValue dynsymEntry) == loadedPltShlAddr
              )
              (Elf.symtabEntries dynsym)
     in case mbPltDynsymEntry of
          Just pltDynsymEntry ->
            GMP.PltStub
              loadedPltEntryAddr
              (Text.decodeUtf8Lenient (Elf.steName pltDynsymEntry))
          Nothing ->
            X.throw $ UnknownPltStub loadedPltEntryAddr loadedPltShlAddr

-- | Find the shared library function addresses for all PLT stubs in an ECFS
-- file. Returns a list of @(function name, shared library virtual address)@
-- pairs. The shared library virtual address is the runtime address of the
-- function in its shared library (not the PLT stub address in the main binary).
--
-- This is needed to populate 'binDynFunMap' for ECFS coredumps. Unlike raw ELF
-- binaries, imported function symbols in an ECFS @.dynsym@ have
-- @SHN_UNDEF@ section indices but non-zero @steValue@ fields (set to the
-- runtime addresses). The normal 'dynamicFunAddrs' filter excludes
-- @SHN_UNDEF@ symbols, so we use the ECFS PLT/GOT metadata instead.
--
-- If an ECFS file lacks PLT information or a dynamic symbol table, returns
-- an empty list.
findEcfsDynFunAddrs :: forall w. MML.LoadOptions -> Ecfs.Ecfs w -> [(Text, Word64)]
findEcfsDynFunAddrs loadOpts ecfs =
  case (Ecfs.decodePltGotInfo ecfs, Ecfs.ecfsDynsym ecfs) of
    (Just pltGotInfos, Just dynsym) ->
      Vec.toList $ Vec.mapMaybe (lookupShlDynsym dynsym) pltGotInfos
    _ ->
      []
 where
  ehi = Ecfs.ecfsElfHeaderInfo ecfs
  cl = Elf.headerClass (Elf.header ehi)
  loadOffset = fromMaybe 0 (MML.loadOffset loadOpts)

  lookupShlDynsym :: Elf.Symtab w -> Ecfs.PltGotInfo -> Maybe (Text, Word64)
  lookupShlDynsym dynsym pltGotInfo =
    let shlAddr = Ecfs.shlEntryVirtAddr pltGotInfo + loadOffset
        mbEntry =
          Elf.elfClassInstances cl $
            Vec.find
              (\e -> fromIntegral @(Elf.ElfWordType w) @Word64 (Elf.steValue e) == shlAddr)
              (Elf.symtabEntries dynsym)
     in fmap (\e -> (Text.decodeUtf8Lenient (Elf.steName e), shlAddr)) mbEntry
