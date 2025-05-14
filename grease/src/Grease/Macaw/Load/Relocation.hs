{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.Load.Relocation
  ( RelocType(..)
  , elfRelocationMap
  ) where

import Control.Exception (throw)
import Data.ByteString qualified as BS
import Data.Either (Either(..), either)
import Data.ElfEdit qualified as EE
import Data.Eq (Eq)
import Data.Function (($), const)
import Data.List (map)
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe(..), fromMaybe, listToMaybe)
import Data.Semigroup ((<>))
import Data.Type.Equality (type (~))
import Data.Vector qualified as Vec
import Data.Word (Word32)
import Grease.Utility (GreaseException(..), tshow)
import Prelude (Num(..), fromIntegral)
import Text.Show (Show(..))

-- | An architecture-independent description of the type of a relocation. When
-- appropriate, this groups together certain relocation types that have
-- identical treatment.
data RelocType
  = RelativeReloc
    -- ^ A @RELATIVE@ relocation (e.g., @R_ARM_RELATIVE@). These reference an
    -- address relative to the load address of a shared library.
  | SymbolReloc
    -- ^ A relocation that references the address of a symbol, plus an offset.
    -- These include absolute references to symbols in the same shared library
    -- (e.g., @R_ARM_ABS32@) and global variables defined in separate shared
    -- libraries (e.g., @R_ARM_GLOB_DAT@).
  deriving Eq

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
  Map.Map (MM.MemWord w) (reloc, BS.ByteString)
elfRelocationMap _ loadOpts ehi =
  let relaDynMap = fromMaybe Map.empty $ do
        relaDyn <- listToMaybe $ EE.findSectionByName ".rela.dyn" elf
        let relaDynBytes = EE.elfSectionData relaDyn
        relas <- rightToMaybe $ EE.decodeRelaEntries (EE.elfData elf) relaDynBytes
        Just $ Map.fromList $ map relaAddrType relas

      relDynMap = fromMaybe Map.empty $ do
        relDyn <- listToMaybe $ EE.findSectionByName ".rel.dyn" elf
        let relDynBytes = EE.elfSectionData relDyn
        rels <- rightToMaybe $ EE.decodeRelEntries (EE.elfData elf) relDynBytes
        Just $ Map.fromList $ map relAddrType rels in

  Map.union relaDynMap relDynMap
  where
    (_, elf) = EE.getElf ehi
    elfClass = EE.elfClass elf
    offset = fromMaybe 0 $ MML.loadOffset loadOpts

    relaAddrType :: EE.RelaEntry reloc -> (MM.MemWord w, (reloc, BS.ByteString))
    relaAddrType rel = EE.elfClassInstances elfClass $
      let addr = MM.memWord (offset + fromIntegral (EE.relaAddr rel))
          relaType = EE.relaType rel in
      (addr, (relaType, resolveRelocSym relaType addr (EE.relaSym rel)))

    relAddrType :: EE.RelEntry reloc -> (MM.MemWord w, (reloc, BS.ByteString))
    relAddrType rel = EE.elfClassInstances elfClass $
      let addr = MM.memWord (offset + fromIntegral (EE.relAddr rel))
          relType = EE.relType rel in
      (addr, (relType, resolveRelocSym relType addr (EE.relSym rel)))

    -- The dynamic symbol table, which contains the names of dynamic
    -- relocations. If the given binary does not have a symbol table, then this
    -- table will be empty.
    dynSymtab :: Vec.Vector (EE.SymtabEntry BS.ByteString (EE.ElfWordType w))
    dynSymtab = case EE.decodeHeaderDynsym ehi of
                  Just (Right v) -> EE.symtabEntries v
                  _ -> Vec.empty

    -- Look up the name of a dynamic relocation from its dynamic symbol table
    -- entry. If you reach this point in the code, then we know that the binary
    -- is dynamically linked (and therefore has a dynamic symbol table), and
    -- moreover, each dynamic relocation should have a corresponding entry in
    -- the dynamic symbol table. If it doesn't, this function will raise an
    -- exception.
    resolveRelocSym ::
      Show reloc =>
      -- | The relocation type (only used for displaying information in an
      -- exception).
      reloc ->
      -- | The relocation's address.
      MM.MemWord w ->
      -- | The relocation symbol's index in the dynamic symbol table.
      Word32 ->
      BS.ByteString
    resolveRelocSym reloc addr idx =
      case dynSymtab Vec.!? fromIntegral idx of
        Just symb -> EE.steName symb
        Nothing -> throw $ GreaseException $
          "Cannot find " <> tshow reloc <>
          " relocation (at address " <> tshow addr <>
          ") in the dynamic symbol table"

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just
