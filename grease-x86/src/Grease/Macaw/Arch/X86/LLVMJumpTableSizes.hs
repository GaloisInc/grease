{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.X86.LLVMJumpTableSizes (
  LLVMJumpTableSizesError (..),
  x86_64LLVMJumpTableSizesFromElf,
) where

import Data.ByteString qualified as BS
import Data.ElfEdit qualified as EE
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LLVMJumpTableSizes qualified as JT
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Word (Word64)
import Prettyprinter qualified as PP

-- | An x86_64 LLVM jump-table sizes section could not be decoded.
data LLVMJumpTableSizesError
  = InvalidJumpTableSectionSize JT.SectionSizeError
  | InvalidJumpTableRelocationSection
      BS.ByteString
      String
  deriving (Eq, Show)

instance PP.Pretty LLVMJumpTableSizesError where
  pretty = \case
    InvalidJumpTableSectionSize err -> PP.pretty err
    InvalidJumpTableRelocationSection name err ->
      "Could not decode ELF relocation section"
        PP.<+> PP.pretty (show name)
        PP.<> PP.colon
        PP.<+> PP.pretty err

-- | Read and resolve an LLVM jump-table sizes section from an x86_64 ELF.
--
-- This applies @R_X86_64_RELATIVE@ relocations and the configured load offset,
-- both of which are required to resolve jump-table addresses in PIE binaries.
x86_64LLVMJumpTableSizesFromElf ::
  LC.LoadOptions ->
  EE.ElfHeaderInfo 64 ->
  MM.Memory 64 ->
  Either
    LLVMJumpTableSizesError
    ([JT.UnresolvableAddress 64], Map.Map (MM.MemSegmentOff 64) (JT.JumpTableSize 64))
x86_64LLVMJumpTableSizesFromElf loadOpts ehi mem =
  case List.find ((== JT.sectionName) . EE.elfSectionName . snd) sections of
    Nothing -> Right ([], Map.empty)
    Just (_, jtSection) -> do
      relocs <- x86_64RelativeRelocations ehi
      entries <-
        mapErr InvalidJumpTableSectionSize $
          JT.parseLLVMJumpTableSizes MM.Addr64 endian (EE.elfSectionData jtSection)
      let sectionAddr = EE.elfSectionAddr jtSection
          relocateEntry idx (addr, count) =
            let slotAddr = sectionAddr + fromIntegral (idx * entryBytes)
                linkAddr =
                  Maybe.fromMaybe (MM.memWordValue addr) (Map.lookup slotAddr relocs)
             in (MM.memWord (linkAddr + loadOffset), count)
      pure $ resolveEntries mem (zipWith relocateEntry [0 :: Int ..] entries)
 where
  sections = Foldable.toList (EE.headerSections ehi)
  entryBytes = 2 * fromIntegral (MM.addrWidthReprByteCount MM.Addr64)
  loadOffset = Maybe.fromMaybe 0 (LC.loadOffset loadOpts)
  endian =
    case EE.headerData (EE.header ehi) of
      EE.ELFDATA2LSB -> MM.LittleEndian
      EE.ELFDATA2MSB -> MM.BigEndian
  mapErr :: (a -> b) -> Either a c -> Either b c
  mapErr f = \case
    Left err -> Left (f err)
    Right value -> Right value

x86_64RelativeRelocations ::
  EE.ElfHeaderInfo 64 ->
  Either LLVMJumpTableSizesError (Map.Map Word64 Word64)
x86_64RelativeRelocations ehi =
  case List.find ((== ".rela.dyn") . EE.elfSectionName . snd) sections of
    Nothing -> Right Map.empty
    Just (_, relaSection) ->
      case EE.decodeRelaEntries endian (EE.elfSectionData relaSection) of
        Left err ->
          Left $
            InvalidJumpTableRelocationSection
              (EE.elfSectionName relaSection)
              err
        Right relas -> Right $ Map.fromList (Maybe.mapMaybe relativeAddend relas)
 where
  sections = Foldable.toList (EE.headerSections ehi)
  endian = EE.headerData (EE.header ehi)
  relativeAddend rela
    | EE.relaType rela == EE.R_X86_64_RELATIVE =
        Just
          ( fromIntegral (EE.relaAddr rela)
          , fromIntegral (EE.relaAddend rela)
          )
    | otherwise = Nothing

resolveEntries ::
  MM.Memory 64 ->
  [(MM.MemWord 64, JT.JumpTableSize 64)] ->
  ([JT.UnresolvableAddress 64], Map.Map (MM.MemSegmentOff 64) (JT.JumpTableSize 64))
resolveEntries mem = Foldable.foldl' step ([], Map.empty)
 where
  step (errs, sizes) (addr, count) =
    case MM.resolveAbsoluteAddr mem addr of
      Nothing -> (JT.UnresolvableAddress addr : errs, sizes)
      Just segOff -> (errs, Map.insert segOff count sizes)
