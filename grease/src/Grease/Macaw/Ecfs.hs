{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Support for ECFS (Extended Core File Snapshot) coredump files
module Grease.Macaw.Ecfs (
  BinaryType (..),
  readBinaryWithEcfs,
  binaryHeaderInfo,
  isEcfsBinary,
  findEcfsPltStubs,
  EcfsError (..),
) where

import Control.Exception qualified as X
import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.ElfEdit.Ecfs qualified as Ecfs
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vec
import Data.Word (Word64)
import Grease.Macaw.PLT qualified as GMP
import Numeric (showHex)
import Prettyprinter qualified as PP

-- | A union type representing either a raw ELF binary or an ECFS coredump file.
-- This allows us to distinguish between the two at the type level.
data BinaryType w
  = RawElfBinary (Elf.ElfHeaderInfo w)
  | EcfsBinary (Ecfs.Ecfs w)

-- | Extract the underlying ELF header information from either binary type.
binaryHeaderInfo :: BinaryType w -> Elf.ElfHeaderInfo w
binaryHeaderInfo (RawElfBinary ehi) = ehi
binaryHeaderInfo (EcfsBinary ecfs) = Ecfs.ecfsElfHeaderInfo ecfs

-- | Check if a binary is an ECFS file.
isEcfsBinary :: BinaryType w -> Bool
isEcfsBinary (RawElfBinary _) = False
isEcfsBinary (EcfsBinary _) = True

-- | Read a binary file and attempt to decode it as ECFS first, falling back to
-- raw ELF if it's not an ECFS file. This implements the lazy detection pattern
-- used in screach.
readBinaryWithEcfs :: BS.ByteString -> Either Ecfs.EcfsDecodeError (Elf.SomeElf BinaryType)
readBinaryWithEcfs bs =
  case Ecfs.decodeEcfs bs of
    Right (Elf.SomeElf ecfs) ->
      -- Successfully decoded as ECFS
      Right $ Elf.SomeElf (EcfsBinary ecfs)
    Left (Ecfs.InvalidEcfsMagic _ _) ->
      -- Not an ECFS file (missing magic bytes), fall back to raw ELF
      case Elf.decodeElfHeaderInfo bs of
        Right (Elf.SomeElf ehi) ->
          Right $ Elf.SomeElf (RawElfBinary ehi)
        Left (off, err) ->
          -- If ELF parsing also fails, report the ELF error
          Left $ Ecfs.DecodeElfHeaderInfoError off err
    Left err ->
      -- ECFS file with other decoding errors (corrupted ECFS file)
      Left err

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
