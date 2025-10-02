{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Load.Relocation (
  RelocType (..),
  RelocationError (..),
  elfRelocationMap,
) where

import Data.ByteString qualified as BS
import Data.ElfEdit qualified as EE
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector qualified as Vec
import Data.Word (Word32, Word64)
import Prettyprinter qualified as PP

-- | An architecture-independent description of the type of a relocation. When
-- appropriate, this groups together certain relocation types that have
-- identical treatment.
data RelocType
  = -- | A @RELATIVE@ relocation (e.g., @R_ARM_RELATIVE@). These reference an
    -- address relative to the load address of a shared library.
    RelativeReloc
  | -- | A relocation that references the address of a symbol, plus an offset.
    -- These include absolute references to symbols in the same shared library
    -- (e.g., @R_ARM_ABS32@) and global variables defined in separate shared
    -- libraries (e.g., @R_ARM_GLOB_DAT@).
    SymbolReloc
  deriving Eq

-- | Error type for 'elfRelocationMap'
data RelocationError
  = RelocationSymbolNotFound
  { relocType :: String
  , relocAddr :: Word64
  }

instance PP.Pretty RelocationError where
  pretty (RelocationSymbolNotFound ty addr) =
    PP.hcat
      [ "Cannot find "
      , PP.pretty ty
      , " relocation (at address "
      , PP.pretty addr
      , ") in the dynamic symbol table"
      ]

-- | Read the @.rela.dyn@ and @.rel.dyn@ sections of an ELF binary (if they
-- exist) to construct a map of dynamic relocation addresses to their
-- corresponding relocation types.
--
-- Most of this code was cargo-culted from an existing implementation in
-- @ambient-verifier@:
-- https://github.com/GaloisInc/ambient-verifier/blob/eab04abb9750825a25ec0cbe0379add63f05f6c6/src/Ambient/Loader/ELF/Symbols/AArch32.hs#L23-L90
elfRelocationMap ::
  forall reloc w proxy.
  ( w ~ EE.RelocationWidth reloc
  , MM.MemWidth w
  , EE.IsRelocationType reloc
  ) =>
  proxy reloc ->
  -- | Options configuring how to load the address of each dynamic relocation.
  MML.LoadOptions ->
  -- | The dynamically linked ELF binary.
  EE.ElfHeaderInfo w ->
  Either RelocationError (Map.Map (MM.MemWord w) (reloc, BS.ByteString))
elfRelocationMap _ loadOpts ehi = do
  relaDynMap <-
    case listToMaybe $ EE.findSectionByName ".rela.dyn" elf of
      Nothing -> Right Map.empty
      Just relaDyn -> do
        let relaDynBytes = EE.elfSectionData relaDyn
        case EE.decodeRelaEntries (EE.elfData elf) relaDynBytes of
          Left _ -> Right Map.empty
          Right relas -> do
            let results = map relaAddrType relas
            case sequence results of
              Left err -> Left err
              Right entries -> Right $ Map.fromList entries

  relDynMap <-
    case listToMaybe $ EE.findSectionByName ".rel.dyn" elf of
      Nothing -> Right Map.empty
      Just relDyn -> do
        let relDynBytes = EE.elfSectionData relDyn
        case EE.decodeRelEntries (EE.elfData elf) relDynBytes of
          Left _ -> Right Map.empty
          Right rels -> do
            let results = map relAddrType rels
            case sequence results of
              Left err -> Left err
              Right entries -> Right $ Map.fromList entries

  Right $ Map.union relaDynMap relDynMap
 where
  (_, elf) = EE.getElf ehi
  elfClass = EE.elfClass elf
  offset = fromMaybe 0 $ MML.loadOffset loadOpts

  relaAddrType :: EE.RelaEntry reloc -> Either RelocationError (MM.MemWord w, (reloc, BS.ByteString))
  relaAddrType rel =
    EE.elfClassInstances elfClass $
      let addr = MM.memWord (offset + fromIntegral (EE.relaAddr rel))
          relaType = EE.relaType rel
       in case resolveRelocSym relaType addr (EE.relaSym rel) of
            Left err -> Left err
            Right symb -> Right (addr, (relaType, symb))

  relAddrType :: EE.RelEntry reloc -> Either RelocationError (MM.MemWord w, (reloc, BS.ByteString))
  relAddrType rel =
    EE.elfClassInstances elfClass $
      let addr = MM.memWord (offset + fromIntegral (EE.relAddr rel))
          relType = EE.relType rel
       in case resolveRelocSym relType addr (EE.relSym rel) of
            Left err -> Left err
            Right symb -> Right (addr, (relType, symb))

  -- The dynamic symbol table, which contains the names of dynamic
  -- relocations. If the given binary does not have a symbol table, then this
  -- table will be empty.
  dynSymtab :: Vec.Vector (EE.SymtabEntry BS.ByteString (EE.ElfWordType w))
  dynSymtab = case EE.decodeHeaderDynsymLenient ehi of
    Right (Just v) -> EE.symtabEntries v
    _ -> Vec.empty

  -- Look up the name of a dynamic relocation from its dynamic symbol table
  -- entry. If you reach this point in the code, then we know that the binary
  -- is dynamically linked (and therefore has a dynamic symbol table), and
  -- moreover, each dynamic relocation should have a corresponding entry in
  -- the dynamic symbol table. If it doesn't, this function will raise an
  -- exception.
  resolveRelocSym ::
    Show reloc =>
    -- The relocation type (only used for displaying information in an
    -- exception).
    reloc ->
    -- The relocation's address.
    MM.MemWord w ->
    -- The relocation symbol's index in the dynamic symbol table.
    Word32 ->
    Either RelocationError BS.ByteString
  resolveRelocSym reloc addr idx =
    case dynSymtab Vec.!? fromIntegral idx of
      Just symb -> Right (EE.steName symb)
      Nothing ->
        Left $
          RelocationSymbolNotFound
            { relocType = show reloc
            , relocAddr = MM.memWordValue addr
            }
