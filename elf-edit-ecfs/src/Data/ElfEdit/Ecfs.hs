{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This is not directly part of Screach, it doesn't have a `panic` function
{- HLINT ignore "Use panic" -}

-- | Utilties for reading and analyzing [extended core file snapshot
-- (ECFS)](https://github.com/elfmaster/ecfs) files using
-- [`elf-edit`](https://github.com/GaloisInc/elf-edit).
module Data.ElfEdit.Ecfs (
  Ecfs (..),
  decodeEcfs,
  decodeExePath,
  decodeArgList,
  decodeAuxiliaryVector,
  decodeShlibMappingNames,
  PltGotInfo (..),
  decodePltGotInfo,
  decodeThreadCount,
  FdInfo (..),
  decodeFdInfo,
  decodePrStatuses,
  EcfsDecodeError (..),
  partitionEcfsParseErrors,
  pattern SHT_INJECTED,
  pattern SHT_PRELOADED,
  module Data.ElfEdit.Ecfs.Personality,
) where

import qualified Control.Exception as X
import Control.Monad (replicateM_, unless)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Coerce (coerce)
import qualified Data.ElfEdit as Elf
import qualified Data.ElfEdit.CoreDump as Elf.CoreDump
import Data.ElfEdit.Ecfs.Personality
import qualified Data.ElfEdit.Prim.Auxv as Elf.Prim
import Data.Int (Int32, Int64, Int8)
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vec
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Prettyprinter as PP
import Text.Printf (printf)

-- | Information about a parsed ECFS file.
data Ecfs w = Ecfs
  { ecfsElfHeaderInfo :: !(Elf.ElfHeaderInfo w)
  -- ^ The underlying ELF header information.
  , ecfsShstrtab :: !(Vec.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)))
  -- ^ The section header string table (usually located in the @.shstrtab@
  -- section).
  , ecfsDynsym :: !(Maybe (Elf.Symtab w))
  -- ^ The dynamic symbol table (usually located in the @.dynsym@ section),
  -- if one exists.
  , ecfsSymtab :: !(Maybe (Elf.Symtab w))
  -- ^ The static symbol table (usually located in the @.symtab@ section),
  -- if one exists.
  , ecfsPhdrs :: !(Vec.Vector (Elf.Phdr w))
  -- ^ The program headers.
  , ecfsPersonality :: !Personality
  -- ^ Custom information found in the @.personality@ section. If a
  -- @.personality@ section does not exist, then this will be filled in with
  -- 'defaultPersonality'.
  , ecfsPltRelaEntries :: !(Maybe (Vec.Vector (Elf.RelaEntry Elf.X86_64_RelocationType)))
  -- ^ The entries from the @.rela.plt@ section, if one exists.
  , ecfsGotPlt :: !(Maybe (Vec.Vector Word64))
  -- ^ The entries from the @.got.plt@ section, if one exists.
  , ecfsPltVirtAddr :: !(Maybe (Elf.ElfWordType w))
  -- ^ The virtual address of the @.plt@ section, if one exists.
  , ecfsPltSize :: !(Maybe (Elf.ElfWordType w))
  -- ^ The size of the @.plt@ section, if one exists.
  , ecfsPltSecVirtAddr :: !(Maybe (Elf.ElfWordType w))
  -- ^ The virtual address of the @.plt.sec@ section, if one exists.
  , ecfsPltSecSize :: !(Maybe (Elf.ElfWordType w))
  -- ^ The size of the @.plt.sec@ section, if one exists.
  }

-- | Errors that can arise when decoding an ECFS file (see 'decodeEcfs').
data EcfsDecodeError
  = InvalidEcfsMagic !BS.ByteString !BS.ByteString
  | DecodeElfHeaderInfoError !Get.ByteOffset !String
  | DecodeShsymtabError !Word16 !Elf.LookupStringError
  | DecodeDynsymError !Elf.SymtabError
  | DecodeSymtabError !Elf.SymtabError
  | DecodeRelaEntriesError !String
  | InvalidAuxvEntry !BS.ByteString !Get.ByteOffset !String
  | InvalidGotPltEntry !BS.ByteString !Get.ByteOffset !String
  | InvalidFdInfoEntry !BS.ByteString !Get.ByteOffset !String
  | InvalidPersonality !BS.ByteString !Get.ByteOffset !String
  | NoteDecodeError !Elf.CoreDump.NoteDecodeError

instance PP.Pretty EcfsDecodeError where
  pretty (InvalidEcfsMagic expected actual) =
    let ppBytes :: BS.ByteString -> PP.Doc a
        ppBytes =
          mconcat
            . map (PP.pretty . printf @(Word8 -> String) "0x%02x ")
            . BS.unpack
     in PP.vcat
          [ "Invalid magic number for ECFS:"
          , "Expected:" PP.<+> ppBytes expected
          , "Actual:  " PP.<+> ppBytes actual
          ]
  pretty (DecodeElfHeaderInfoError off err) =
    "Failed to decode ELF header information at offset"
      PP.<+> PP.pretty off
      PP.<> PP.colon
      PP.<+> PP.pretty err
  pretty (DecodeShsymtabError idx err) =
    "Failed to decode section header symbol table entry at index"
      PP.<+> PP.pretty idx
      PP.<> PP.colon
      PP.<+> PP.viaShow err
  pretty (DecodeDynsymError err) =
    PP.viaShow err
  pretty (DecodeSymtabError err) =
    PP.viaShow err
  pretty (DecodeRelaEntriesError err) =
    "Failed to decode RELA relocation entries:" PP.<+> PP.pretty err
  pretty (InvalidAuxvEntry rest off err) =
    ppGetParseError "ELF auxiliary vector entry" rest off err
  pretty (InvalidGotPltEntry rest off err) =
    ppGetParseError ".got.plt entry" rest off err
  pretty (InvalidFdInfoEntry rest off err) =
    ppGetParseError ".fdinfo entry" rest off err
  pretty (InvalidPersonality rest off err) =
    ppGetParseError "personality" rest off err
  pretty (NoteDecodeError err) =
    PP.viaShow err

instance Show EcfsDecodeError where
  show = show . PP.pretty

instance X.Exception EcfsDecodeError

-- | Decode an 'Ecfs' value from a 'BS.ByteString'.
decodeEcfs :: BS.ByteString -> Either EcfsDecodeError (Elf.SomeElf Ecfs)
decodeEcfs bs = do
  -- First, check if the ECFS magic bytes are in the expected place. This is a
  -- relatively cheap test to determine if this is actually an ECFS file or not,
  -- so do this before anything else.
  let magic = Elf.slice (eiPad, BS.length ecfsMagic) bs
  unless (ecfsMagic == magic) $
    Left $
      InvalidEcfsMagic ecfsMagic magic

  Elf.SomeElf ehi <-
    case Elf.decodeElfHeaderInfo bs of
      Left (off, err) -> Left $ DecodeElfHeaderInfoError off err
      Right sehi -> Right sehi
  let header = Elf.header ehi
  let d = Elf.headerData header
  shstrtab <-
    case Elf.headerNamedShdrs ehi of
      Left (idx, err) -> Left $ DecodeShsymtabError idx err
      Right shstrtab -> Right shstrtab
  dynsym <-
    case Elf.decodeHeaderDynsymLenient ehi of
      Left err -> Left $ DecodeDynsymError err
      Right dynsym -> Right dynsym
  symtab <-
    case Elf.decodeHeaderSymtabLenient ehi of
      Left err -> Left $ DecodeDynsymError err
      Right symtab -> Right symtab
  let phdrs = Vec.fromList $ Elf.headerPhdrs ehi
  personality <-
    case decodePersonality ehi shstrtab of
      Just res -> res
      Nothing -> Right defaultPersonality
  pltRelaEntries <-
    case fmap
      (Elf.decodeRelaEntries d . Elf.shdrData ehi)
      (Vec.find (\shdr -> Elf.shdrName shdr == ".rela.plt") shstrtab) of
      Just (Left err) -> Left $ DecodeRelaEntriesError err
      Just (Right pltRelaEntries) -> Right $ Just $ Vec.fromList pltRelaEntries
      Nothing -> Right Nothing
  gotPlt <-
    case decodeGotPlt ehi shstrtab of
      Just (Left err) -> Left err
      Just (Right gotPlt) -> Right $ Just gotPlt
      Nothing -> Right Nothing
  let (pltVirtAddr, pltSize) =
        case Vec.find (\shdr -> Elf.shdrName shdr == ".plt") shstrtab of
          Just shdr -> (Just (Elf.shdrAddr shdr), Just (Elf.shdrSize shdr))
          Nothing -> (Nothing, Nothing)
  let (pltSecVirtAddr, pltSecSize) =
        case Vec.find (\shdr -> Elf.shdrName shdr == ".plt.sec") shstrtab of
          Just shdr -> (Just (Elf.shdrAddr shdr), Just (Elf.shdrSize shdr))
          Nothing -> (Nothing, Nothing)
  Right $
    Elf.SomeElf $
      Ecfs
        { ecfsElfHeaderInfo = ehi
        , ecfsShstrtab = shstrtab
        , ecfsDynsym = dynsym
        , ecfsSymtab = symtab
        , ecfsPhdrs = phdrs
        , ecfsPersonality = personality
        , ecfsPltRelaEntries = pltRelaEntries
        , ecfsGotPlt = gotPlt
        , ecfsPltVirtAddr = pltVirtAddr
        , ecfsPltSize = pltSize
        , ecfsPltSecVirtAddr = pltSecVirtAddr
        , ecfsPltSecSize = pltSecSize
        }
 where
  -- The value of the EI_PAD constant, i.e., the offset where the padding
  -- bytes start in an ELF file.
  eiPad :: Elf.FileOffset Int
  eiPad = 9

  -- The 4-byte string expected at the start of the padding bytes in an ECFS
  -- file.
  ecfsMagic :: BS.ByteString
  ecfsMagic = "ECFS"

-- | Decode the path to the executable from an ECFS file's @.exepath@ section.
-- Return 'Nothing' if this section does not exist.
decodeExePath :: Ecfs w -> Maybe FilePath
decodeExePath ecfs =
  fmap
    (Text.unpack . Text.decodeUtf8 . Elf.shdrData ehi)
    maybeExepathShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  maybeExepathShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".exepath")
      (ecfsShstrtab ecfs)

-- | Decode the argument list from an ECFS file's @.arglist@ section. Return
-- 'Nothing' if this section does not exist.
decodeArgList :: Ecfs w -> Maybe (Vec.Vector String)
decodeArgList ecfs =
  fmap
    ( Vec.map (Text.unpack . Text.decodeUtf8)
        . Vec.fromList
        . BS.split 0
        . Elf.shdrData ehi
    )
    maybeArglistShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  maybeArglistShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".arglist")
      (ecfsShstrtab ecfs)

-- | Decode the ELF auxiliary vector from an ECFS file's @.auxvector@ section.
-- Return 'Nothing' if this section does not exist.
decodeAuxiliaryVector ::
  forall w.
  Ecfs w ->
  Maybe (Either EcfsDecodeError (Vec.Vector (Elf.Prim.AuxvEntry w)))
decodeAuxiliaryVector ecfs =
  fmap
    ( \shdr -> Elf.elfClassInstances cl $ do
        let shdrData = Elf.shdrData ehi shdr
        let shdrSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrSize shdr
        let shdrEntSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrEntSize shdr
        let auxvCount = shdrSize `quot` shdrEntSize
        Vec.generateM auxvCount $ decodeEntry shdrData $ Bytes shdrEntSize
    )
    maybeAuxvectorShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  header = Elf.header ehi
  cl = Elf.headerClass header
  d = Elf.headerData header

  maybeAuxvectorShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".auxvector")
      (ecfsShstrtab ecfs)

  decodeEntry ::
    -- \| The entry's bytes.
    BS.ByteString ->
    -- \| The size of an entry in the ELF auxiliary vector in bytes.
    Bytes ->
    -- \| The index of the entry in the ELF auxiliary vector.
    Int ->
    Either EcfsDecodeError (Elf.Prim.AuxvEntry w)
  decodeEntry buf entrySize entryIdx =
    let entryOff = unBytes entrySize * entryIdx
     in let entryBuf = BS.drop entryOff buf
         in decodeAuxvEntry cl d entryBuf

-- | Decode the shared library mappings in an ECFS file.
decodeShlibMappingNames :: Ecfs w -> Vec.Vector String
decodeShlibMappingNames ecfs =
  Vec.mapMaybe
    ( \shdr ->
        let shdrType = Elf.shdrType shdr
         in if shdrType `elem` [Elf.SHT_SHLIB, SHT_INJECTED, SHT_PRELOADED]
              then Just $ Text.unpack $ Text.decodeUtf8 $ Elf.shdrName shdr
              else Nothing
    )
    (ecfsShstrtab ecfs)

-- | Information about the procedure linkage table (PLT) and global offset table
-- (GOT).
data PltGotInfo = PltGotInfo
  { gotSite :: !Word64
  -- ^ Address of where the GOT entry exists
  , gotEntryVirtAddr :: !Word64
  -- ^ Address that is in the GOT entry (the pointer address)
  , pltEntryVirtAddr :: !Word64
  -- ^ The PLT address that the GOT entry should point to if not yet resolved
  , shlEntryVirtAddr :: !Word64
  -- ^ The shared library address the GOT should point to if it has been
  -- resolved
  }

-- | Decode the 'PltGotInfo' in an ECFS file.
-- Return 'Nothing' if the ECFS file does not have PLT/GOT information.
decodePltGotInfo ::
  forall w.
  Ecfs w ->
  Maybe (Vec.Vector PltGotInfo)
decodePltGotInfo ecfs =
  Elf.elfClassInstances cl $ do
    let pltSecEnabled = isJust (ecfsPltSecVirtAddr ecfs)
    pltVirtAddr <-
      if pltSecEnabled
        then ecfsPltSecVirtAddr ecfs
        else ecfsPltVirtAddr ecfs
    pltRelaEntries <- ecfsPltRelaEntries ecfs
    dynSymtab <- ecfsDynsym ecfs
    gotPlt <- ecfsGotPlt ecfs
    let gotPlt' = Vec.drop 3 gotPlt -- The first 3 entries are reserved
        firstPltAddr =
          fromIntegral @(Elf.ElfWordType w) @Word64 $
            if pltSecEnabled
              then pltVirtAddr
              else -- We want to start at the PLT entry after what is called PLT-0
                pltVirtAddr + 16
    Just $
      Vec.imap
        ( \idx relaEntry ->
            PltGotInfo
              { gotSite =
                  Elf.relaAddr relaEntry
              , gotEntryVirtAddr =
                  gotPlt' Vec.! idx
              , pltEntryVirtAddr =
                  firstPltAddr + (16 * fromIntegral @Int @Word64 idx)
              , shlEntryVirtAddr =
                  fromIntegral @(Elf.ElfWordType w) @Word64 $
                    Elf.steValue $
                      Elf.symtabEntries dynSymtab Vec.! fromIntegral @Word32 @Int (Elf.relaSym relaEntry)
              }
        )
        pltRelaEntries
 where
  ehi = ecfsElfHeaderInfo ecfs
  header = Elf.header ehi
  cl = Elf.headerClass header

-- | Decode the number of threads from the @.prstatus@ section in an ECFS file.
-- Return 'Nothing' if the ECFS file does not have a @.prstatus@ section.
decodeThreadCount ::
  forall w.
  Ecfs w ->
  Maybe Int
decodeThreadCount ecfs =
  Elf.elfClassInstances cl $
    fmap
      ( \shdr ->
          fromIntegral @(Elf.ElfWordType w) @Int $
            Elf.shdrSize shdr `quot` Elf.shdrEntSize shdr
      )
      maybePrstatusShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  header = Elf.header ehi
  cl = Elf.headerClass header

  maybePrstatusShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".prstatus")
      (ecfsShstrtab ecfs)

-- | Information about a file descriptor.
data FdInfo = FdInfo
  { fd :: Int32
  , fdPath :: FilePath
  , fdPos :: Int64
  , fdPerms :: Word32
  , fdSrcAddr :: Word32
  , fdDstAddr :: Word32
  , fdSrcPort :: Word16
  , fdDstPort :: Word16
  , fdNet :: Char
  }

-- | Decode the 'FdInfo' in an ECFS file.
-- Return 'Nothing' if the ECFS file does not have file descriptor information.
decodeFdInfo ::
  forall w.
  Ecfs w ->
  Maybe (Either EcfsDecodeError (Vec.Vector FdInfo))
decodeFdInfo ecfs =
  fmap
    ( \shdr -> Elf.elfClassInstances cl $ do
        let shdrData = Elf.shdrData ehi shdr
        let shdrSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrSize shdr
        let shdrEntSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrEntSize shdr
        let fdInfoCount = shdrSize `quot` shdrEntSize
        Vec.generateM fdInfoCount $ decodeEntry shdrData $ Bytes shdrEntSize
    )
    maybeFdInfoShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  header = Elf.header ehi
  cl = Elf.headerClass header
  d = Elf.headerData header

  maybeFdInfoShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".fdinfo")
      (ecfsShstrtab ecfs)

  decodeEntry ::
    -- \| The entry's bytes.
    BS.ByteString ->
    -- \| The size of an entry in the @.fdinfo@ section in bytes.
    Bytes ->
    -- \| The index of the entry in the @.fdinfo@ section.
    Int ->
    Either EcfsDecodeError FdInfo
  decodeEntry buf entrySize entryIdx =
    let entryOff = unBytes entrySize * entryIdx
     in let entryBuf = BS.drop entryOff buf
         in decodeFdInfoEntry d entryBuf

-- | Decode the 'PrStatus' values in an ECFS file.
-- Return 'Nothing' if the ECFS file does not have a @.prstatus@ section.
decodePrStatuses ::
  forall w.
  Ecfs w ->
  Maybe (Either EcfsDecodeError (Vec.Vector Elf.CoreDump.PrStatus))
decodePrStatuses ecfs =
  fmap
    ( \shdr -> Elf.elfClassInstances cl $ do
        let shdrData = Elf.shdrData ehi shdr
        let shdrSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrSize shdr
        let shdrEntSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrEntSize shdr
        let prStatusCount = shdrSize `quot` shdrEntSize
        Vec.generateM prStatusCount $ decodeEntry shdrData $ Bytes shdrEntSize
    )
    maybePrstatusShdr
 where
  ehi = ecfsElfHeaderInfo ecfs
  header = Elf.header ehi
  cl = Elf.headerClass header
  d = Elf.headerData header
  m = Elf.headerMachine header

  maybePrstatusShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".prstatus")
      (ecfsShstrtab ecfs)

  decodeEntry ::
    -- \| The entry's bytes.
    BS.ByteString ->
    -- \| The size of an entry in the @.prstatus@ section in bytes.
    Bytes ->
    -- \| The index of the entry in the @.prstatus@ section.
    Int ->
    Either EcfsDecodeError Elf.CoreDump.PrStatus
  decodeEntry buf entrySize entryIdx =
    let entryOff = unBytes entrySize * entryIdx
     in let entryBuf = BS.drop entryOff buf
         in case Elf.CoreDump.decodePrStatus cl d m entryBuf of
              Left err -> Left $ NoteDecodeError err
              Right prStatus -> Right prStatus

-- | Given a list of 'Elf.ElfParseError's that arise when parsing an ECFS file,
-- categorize them into @(warns, errs)@, where @warns@ are non-fatal
-- 'Elf.ElfParseError's that can be treated as warnings and @errs@ are fatal
-- errors.
partitionEcfsParseErrors ::
  [Elf.ElfParseError] ->
  ([Elf.ElfParseError], [Elf.ElfParseError])
partitionEcfsParseErrors = List.partition isWarn
 where
  isWarn :: Elf.ElfParseError -> Bool
  isWarn warnOrErr =
    case warnOrErr of
      -- The list of errors that we treat as warnings is somewhat ad hoc and
      -- determined primarily through experimenting with ECFS examples
      -- produced on examples on the wild. It is likely that the list below
      -- is incomplete.
      Elf.OverlapMovedLater{} -> True
      Elf.PhdrSizeIncreased{} -> True
      _ -> False

pattern SHT_INJECTED :: Elf.ElfSectionType
pattern SHT_INJECTED = Elf.ElfSectionType 0x200000

pattern SHT_PRELOADED :: Elf.ElfSectionType
pattern SHT_PRELOADED = Elf.ElfSectionType 0x300000

-----
-- Helpers, not exported
-----

-- | Decode an 'Elf.Prim.AuxvEntry' value from a 'BS.ByteString'.
decodeAuxvEntry ::
  Elf.ElfClass w ->
  Elf.ElfData ->
  BS.ByteString ->
  Either EcfsDecodeError (Elf.Prim.AuxvEntry w)
decodeAuxvEntry cl d bs =
  case strictRunGetOrFail (getAuxvEntry cl d) bs of
    Left (rest, off, msg) -> Left $ InvalidAuxvEntry rest off msg
    Right (_, _, auxvEntry) -> pure auxvEntry

-- | Decode a value in the @.got.plt@ section from a 'BS.ByteString'.
decodeGotPltEntry ::
  Elf.ElfData ->
  BS.ByteString ->
  Either EcfsDecodeError Word64
decodeGotPltEntry d bs =
  case strictRunGetOrFail (Elf.getWord64 d) bs of
    Left (rest, off, msg) -> Left $ InvalidGotPltEntry rest off msg
    Right (_, _, gotPltEntry) -> pure gotPltEntry

-- | Decode a value in the @.fdinfo@ section from a 'BS.ByteString'.
decodeFdInfoEntry ::
  Elf.ElfData ->
  BS.ByteString ->
  Either EcfsDecodeError FdInfo
decodeFdInfoEntry d bs =
  case strictRunGetOrFail (getFdInfo d) bs of
    Left (rest, off, msg) -> Left $ InvalidFdInfoEntry rest off msg
    Right (_, _, fdInfoEntry) -> pure fdInfoEntry

-- | Decode the 'Personality' from an ECFS file's @.personality@ section.
-- Return 'Nothing' if this section does not exist.
decodePersonality ::
  Elf.ElfHeaderInfo w ->
  Vec.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)) ->
  Maybe (Either EcfsDecodeError Personality)
decodePersonality ehi shstrtab =
  fmap
    (decode . Elf.shdrData ehi)
    maybePersonalityShdr
 where
  header = Elf.header ehi
  d = Elf.headerData header

  maybePersonalityShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".personality")
      shstrtab

  decode :: BS.ByteString -> Either EcfsDecodeError Personality
  decode bs =
    case strictRunGetOrFail (Elf.getWord32 d) bs of
      Left (rest, off, msg) -> Left $ InvalidPersonality rest off msg
      Right (_, _, p) -> pure $ Personality p

-- | Decode the entries from an ECFS file's @.got.plt@ section.
-- Return 'Nothing' if this section does not exist.
decodeGotPlt ::
  forall w.
  Elf.ElfHeaderInfo w ->
  Vec.Vector (Elf.Shdr BS.ByteString (Elf.ElfWordType w)) ->
  Maybe (Either EcfsDecodeError (Vec.Vector Word64))
decodeGotPlt ehi shstrtab =
  fmap
    ( \shdr -> Elf.elfClassInstances cl $ do
        let shdrData = Elf.shdrData ehi shdr
        let shdrSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrSize shdr
        let shdrEntSize =
              fromIntegral @(Elf.ElfWordType w) @Int $
                Elf.shdrEntSize shdr
        let gotPltCount = shdrSize `quot` shdrEntSize
        Vec.generateM gotPltCount $ decodeEntry shdrData $ Bytes shdrEntSize
    )
    maybeGotpltShdr
 where
  header = Elf.header ehi
  cl = Elf.headerClass header
  d = Elf.headerData header

  maybeGotpltShdr =
    Vec.find
      (\shdr -> Elf.shdrName shdr == ".got.plt")
      shstrtab

  decodeEntry ::
    -- \| The entry's bytes.
    BS.ByteString ->
    -- \| The size of an entry in the @.got.plt@ section in bytes.
    Bytes ->
    -- \| The index of the entry in the @.got.plt@ section.
    Int ->
    Either EcfsDecodeError Word64
  decodeEntry buf entrySize entryIdx =
    let entryOff = unBytes entrySize * entryIdx
     in let entryBuf = BS.drop entryOff buf
         in decodeGotPltEntry d entryBuf

-- | Parse an 'Elf.Prim.AuxvType' value.
getAuxvType :: Elf.ElfClass w -> Elf.ElfData -> Get.Get (Elf.Prim.AuxvType w)
getAuxvType cl d = coerce $ getElfWord cl d

-- | Parse an 'Elf.Prim.AuxvEntry' value.
getAuxvEntry :: Elf.ElfClass w -> Elf.ElfData -> Get.Get (Elf.Prim.AuxvEntry w)
getAuxvEntry cl d =
  Elf.Prim.AuxvEntry
    <$> getAuxvType cl d
    <*> getElfWord cl d

-- | Parse an 'FdInfo' value.
getFdInfo :: Elf.ElfData -> Get.Get FdInfo
getFdInfo d =
  FdInfo
    <$> getInt32 d
    <*> fmap (Text.unpack . Text.decodeUtf8) (Get.getByteString maxPath)
    <* paddingBytes 4
    <*> getInt64 d
    <*> Elf.getWord32 d
    <*> Elf.getWord32 d
    <*> Elf.getWord32 d
    <*> Elf.getWord16 d
    <*> Elf.getWord16 d
    <*> fmap (chr . fromIntegral @Int8 @Int) Get.getInt8
 where
  maxPath :: Int
  maxPath = 512

  paddingBytes :: Int -> Get.Get ()
  paddingBytes n = replicateM_ n Get.getWord8

-- | Parse a word-size integer, where the size and endianness of the word
-- depends on the particular ELF file.
getElfWord :: Elf.ElfClass w -> Elf.ElfData -> Get.Get (Elf.ElfWordType w)
getElfWord cl d =
  case cl of
    Elf.ELFCLASS32 -> Elf.getWord32 d
    Elf.ELFCLASS64 -> Elf.getWord64 d

-- | Parse an 'Int32' value, where the endianness of the value depends on the
-- particular ELF file.
getInt32 :: Elf.ElfData -> Get.Get Int32
getInt32 Elf.ELFDATA2LSB = Get.getInt32le
getInt32 Elf.ELFDATA2MSB = Get.getInt32be

-- | Parse an 'Int64' value, where the endianness of the value depends on the
-- particular ELF file.
getInt64 :: Elf.ElfData -> Get.Get Int64
getInt64 Elf.ELFDATA2LSB = Get.getInt64le
getInt64 Elf.ELFDATA2MSB = Get.getInt64be

-- | Pretty-print a parse error obtained from calling 'strictRunGetOrFail'.
ppGetParseError ::
  -- | What we attempted to parse.
  String ->
  -- | The rest of the input that was not parsed.
  BS.ByteString ->
  -- | The offset into the input at which we began parsing.
  Get.ByteOffset ->
  -- | The text of the parse failure.
  String ->
  PP.Doc a
ppGetParseError what rest off err =
  PP.vcat
    [ "Invalid" PP.<+> PP.pretty what PP.<> PP.colon PP.<+> PP.pretty err
    , "At offset" PP.<+> PP.pretty off
    , "Remaining bytes:" PP.<+> PP.pretty (BS.length rest)
    ]

-- Taken from elf-edit's unexported internals
strictRunGetOrFail ::
  Get.Get a ->
  BS.ByteString ->
  Either
    (BS.ByteString, Get.ByteOffset, String)
    (BS.ByteString, Get.ByteOffset, a)
strictRunGetOrFail m bs =
  case Get.pushEndOfInput (Get.pushChunk (Get.runGetIncremental m) bs) of
    Get.Fail rest off msg -> Left (rest, off, msg)
    Get.Partial _cont -> error "internal error: Get partial failed."
    Get.Done rest off r -> Right (rest, off, r)

-- | A newtype for expressing numbers of bytes. This newtype is explicitly
-- introduced to avoid confusion between widths expressed as numbers of bits vs.
-- numbers of bytes.
--
-- This is taken from @crucible-llvm@, which @elf-edit-ecfs@ does not depend on.
newtype Bytes = Bytes {unBytes :: Int}
  deriving (Eq, Ord, Num, Enum, Real, Integral)
