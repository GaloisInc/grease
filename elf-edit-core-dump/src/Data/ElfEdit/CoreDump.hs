{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Utilities for reading and analyzing ELF core dumps using @elf-edit@. The
-- following resources are helpful in understanding how core dumps work:
--
-- * A brief look into core dumps:
--   http://uhlo.blogspot.com/2012/05/brief-look-into-core-dumps.html
--
-- * Anatomy of an ELF core file:
--   https://www.gabriel.urdhr.fr/2015/05/29/core-file/
--
-- * System V ABI documentation on ELF note sections (which are heavily used in
--   core dump files):
--   https://refspecs.linuxbase.org/elf/gabi4+/ch5.pheader.html#note_section
module Data.ElfEdit.CoreDump
  ( -- * Types
    Note(..)
  , NoteDesc(..)
  , PrStatus(..)
  , FpRegSet(..)
  , PrpsInfo(..)
  , UserRegSet(..)

    -- * Decoding
  , decodeHeaderNotes
  , decodePrStatus
  , decodeUserRegSet
  , decodeArmUserRegs
  , decodePpcUserRegs
  , decodeX86_64UserRegs
  , decodeNhdr
  , NoteDecodeError(..)

    -- * Analysis
  , coreDumpPrStatusProgramCounter
  , coreDumpHeaderProgramCounter
  , CoreDumpAnalyzeError(..)
  ) where

import Control.Monad (unless)
import Data.Binary.Get qualified as Get
import Data.Bits (Bits(..))
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Maybe (mapMaybe, maybeToList)

import Data.ElfEdit qualified as Elf

import Data.ElfEdit.CoreDump.ARM qualified as ARM
import Data.ElfEdit.CoreDump.PPC qualified as PPC
import Data.ElfEdit.CoreDump.X86_64 qualified as X86_64
import Data.ElfEdit.Prim.Nhdr qualified as Nhdr

-----
-- Types
-----

-- | Special information that other programs will check for conformance,
-- compatibility, etc.
data Note = Note
  { noteName :: !BS.ByteString
    -- ^ The note's owner or originator.
  , noteDesc :: !NoteDesc
    -- ^ The note's descriptor.
  } deriving Show

-- | A note descriptor. The ABI places no constraints on a descriptor's
-- contents, but we recognize a subset of descriptor types that are commonly
-- used in core dump files.
data NoteDesc
  = NotePrStatus !PrStatus
  | NoteFpRegSet !FpRegSet
  | NotePrpsInfo !PrpsInfo
  deriving Show

-- | General-purpose user registers. These are different on every ELF machine
-- type, so we distinguish between the supported configurations here.
data UserRegSet
  = ArmUserRegSet ARM.ArmUserRegs
  | Ppc32UserRegSet (PPC.PpcUserRegs 32)
  | Ppc64UserRegSet (PPC.PpcUserRegs 64)
  | X86_64UserRegSet X86_64.X86_64UserRegs
  deriving Show

-- | This corresponds to the @elf_prstatus@ struct. Not all fields of the struct
-- have Haskell counterparts yet.
data PrStatus = PrStatus
  { prReg :: !UserRegSet
    -- ^ The user registers. Currently, this assumes x86-64 registers.
  }
  deriving Show

-- | Parse an 'Nhdr.NhdrType' value.
getNhdrType :: Elf.ElfData -> Get.Get Nhdr.NhdrType
getNhdrType d = Nhdr.NhdrType <$> Elf.getWord32 d

-- | Parse an 'Nhdr.Nhdr' value.
getNhdr :: Elf.ElfData -> Get.Get Nhdr.Nhdr
getNhdr d = do
  nhdrNameSize <- Elf.getWord32 d
  nhdrDescSize <- Elf.getWord32 d
  nhdrType <- getNhdrType d
  pure Nhdr.Nhdr{..}

-- | This corresponds to the @user_fpregs_struct@ struct. Not all fields of the
-- struct have Haskell counterparts yet.
data FpRegSet = FpRegSet
  deriving Show

-- | This corresponds to the @elf_prpsinfo@ struct. Not all fields of the struct
-- have Haskell counterparts yet.
data PrpsInfo = PrpsInfo
  deriving Show

-----
-- Decoding
-----

-- | Errors found when decoding 'Note's.
data NoteDecodeError
  = InvalidNhdr String
  | InvalidArmUserRegs String
  | InvalidPpcUserRegs String
  | InvalidX86_64UserRegs String
  | forall w. UnsupportedUserRegs (Elf.ElfClass w) Elf.ElfMachine

instance Show NoteDecodeError where
  show (InvalidNhdr msg) =
    "Error parsing note header: " ++ msg
  show (InvalidArmUserRegs msg) =
    "Error parsing ARM registers: " ++ msg
  show (InvalidPpcUserRegs msg) =
    "Error parsing PPC registers: " ++ msg
  show (InvalidX86_64UserRegs msg) =
    "Error parsing x86-64 registers: " ++ msg
  show (UnsupportedUserRegs cl m) =
    "Don't know how to support user registers on ELF class " ++ show cl ++
    " and machine " ++ show m

-- Taken from elf-edit's unexported internals
strictRunGetOrFail :: Get.Get a
                   -> BS.ByteString
                   -> Either (BS.ByteString, Get.ByteOffset, String) (BS.ByteString, Get.ByteOffset, a)
strictRunGetOrFail m bs =
  case Get.pushEndOfInput (Get.pushChunk (Get.runGetIncremental m) bs) of
    Get.Fail rest off msg -> Left (rest, off, msg)
    Get.Partial _cont -> error $ "internal error: Get partial failed."
    Get.Done rest off r -> Right (rest, off, r)

-- | Decode an 'Nhdr.Nhdr' value from a 'BS.ByteString'.
decodeNhdr ::
  Elf.ElfData ->
  BS.ByteString ->
  Either NoteDecodeError Nhdr.Nhdr
decodeNhdr d buf = do
  case strictRunGetOrFail (getNhdr d) buf of
    Left (_,_,msg) -> Left $ InvalidNhdr msg
    Right (_,_,nhdr) -> pure nhdr

-- | Decode an 'ARM.ArmUserRegs' value from a 'BS.ByteString'.
decodeArmUserRegs ::
  Elf.ElfData ->
  BS.ByteString ->
  Either NoteDecodeError ARM.ArmUserRegs
decodeArmUserRegs d buf = do
  case strictRunGetOrFail (ARM.getArmUserRegs d) buf of
    Left (_,_,msg) -> Left $ InvalidArmUserRegs msg
    Right (_,_,regs) -> pure regs

-- | Decode a 'PPC.PpcUserRegs' value from a 'BS.ByteString'.
decodePpcUserRegs ::
  Elf.ElfClass w ->
  Elf.ElfData ->
  BS.ByteString ->
  Either NoteDecodeError (PPC.PpcUserRegs w)
decodePpcUserRegs cl d buf = do
  case strictRunGetOrFail (PPC.getPpcUserRegs cl d) buf of
    Left (_,_,msg) -> Left $ InvalidPpcUserRegs msg
    Right (_,_,regs) -> pure regs

-- | Decode an 'X86_64.X86_64UserRegs' value from a 'BS.ByteString'.
decodeX86_64UserRegs ::
  Elf.ElfData ->
  BS.ByteString ->
  Either NoteDecodeError X86_64.X86_64UserRegs
decodeX86_64UserRegs d buf = do
  case strictRunGetOrFail (X86_64.getX86_64UserRegs d) buf of
    Left (_,_,msg) -> Left $ InvalidX86_64UserRegs msg
    Right (_,_,regs) -> pure regs

-- | Decode a 'UserRegSet' value from a 'BS.ByteString'.
decodeUserRegSet ::
  forall w.
  Elf.ElfClass w ->
  Elf.ElfData ->
  Elf.ElfMachine ->
  BS.ByteString ->
  Either NoteDecodeError UserRegSet
decodeUserRegSet cl d m buf =
  Elf.elfClassInstances cl $
  case (cl, m) of
    (Elf.ELFCLASS32, Elf.EM_ARM) ->
      let armUserRegsFileRange =
            ( Elf.FileOffset ARM.armPrRegOffset
            , ARM.armUserRegsSize
            ) in
      ArmUserRegSet <$>
        decodeArmUserRegs d (Elf.slice armUserRegsFileRange buf)
    (Elf.ELFCLASS32, Elf.EM_PPC) ->
      decodePpcUserRegSet Ppc32UserRegSet
    (Elf.ELFCLASS64, Elf.EM_PPC64) ->
      decodePpcUserRegSet Ppc64UserRegSet
    (Elf.ELFCLASS64, Elf.EM_X86_64) ->
      let x86_64UserRegsFileRange =
            ( Elf.FileOffset X86_64.x86_64PrRegOffset
            , X86_64.x86_64UserRegsSize
            ) in
      X86_64UserRegSet <$>
        decodeX86_64UserRegs d (Elf.slice x86_64UserRegsFileRange buf)
    _ ->
      Left $ UnsupportedUserRegs cl m
  where
    decodePpcUserRegSet ::
      Integral (Elf.ElfWordType w) =>
      (PPC.PpcUserRegs w -> UserRegSet) ->
      Either NoteDecodeError UserRegSet
    decodePpcUserRegSet mkPpcUserRegSet =
      let ppcUserRegsFileRange =
            ( Elf.FileOffset (PPC.ppcPrRegOffset cl)
            , PPC.ppcUserRegsSize cl
            ) in
      mkPpcUserRegSet <$>
        decodePpcUserRegs cl d (Elf.slice ppcUserRegsFileRange buf)

-- | Decode a 'PrStatus' value from a 'BS.ByteString'.
decodePrStatus ::
  Elf.ElfClass w ->
  Elf.ElfData ->
  Elf.ElfMachine ->
  BS.ByteString ->
  Either NoteDecodeError PrStatus
decodePrStatus cl d m buf = PrStatus <$> decodeUserRegSet cl d m buf

-- | Compute the padding necessary to make a word value 4-byte aligned (if using
-- 'Elf.ELFCLASS32') or 8-byte aligned (if using 'Elf.ELFCLASS64').
notePadding ::
  forall w.
  Elf.ElfClass w ->
  Elf.ElfWordType w ->
  Elf.ElfWordType w
notePadding cl sz =
    Elf.elfClassInstances cl $
    (align - (sz .&. (align - 1))) .&. (align - 1)
  where
    align :: Elf.ElfWordType w
    align =
      case cl of
        Elf.ELFCLASS32 -> 4
        Elf.ELFCLASS64 -> 8

-- | Decode a list of 'Note's from a program header's contents (represented as a
-- 'BS.ByteString').
decodeNotes ::
  forall w.
  Elf.ElfClass w ->
  Elf.ElfData ->
  Elf.ElfMachine ->
  BS.ByteString ->
  Either NoteDecodeError [Note]
decodeNotes cl d m phdrContents =
    Elf.elfClassInstances cl $ go phdrContents
  where
    -- Iterate through the ByteString and decode one Note at a time.
    go ::
      (Integral (Elf.ElfWordType w), Num (Elf.ElfWordType w)) =>
      BS.ByteString ->
      Either NoteDecodeError [Note]
    go buf
      | BS.null buf
      = pure []
      | otherwise
      = do -- First, decode the note header.
           nhdr <- decodeNhdr d buf
           -- Next, decode the note section (if we currently support it). This
           -- is the tricky part, as the amount of bytes that the note section
           -- uses is variable and depends on information found in the note
           -- header. See
           -- https://refspecs.linuxbase.org/elf/gabi4+/ch5.pheader.html#note_section
           -- for a higher-level description of how this decoding works.
           let nhdrSize :: Elf.ElfWordType w
               nhdrSize = fromIntegral Nhdr.nhdrSize

               nameFileOffset = Elf.FileOffset nhdrSize
               nameSize       = fromIntegral (Nhdr.nhdrNameSize nhdr)
               namePadding    = notePadding cl nameSize
               nameFileRange  = (nameFileOffset, nameSize)
               name           = BS.takeWhile (/= 0) (Elf.slice nameFileRange buf)

               descFileOffset = Elf.incOffset nameFileOffset (nameSize + namePadding)
               descSize       = fromIntegral (Nhdr.nhdrDescSize nhdr)
               descFileRange  = (descFileOffset, descSize)
               descPadding    = notePadding cl descSize
               descRaw        = Elf.slice descFileRange buf

               noteSize = nhdrSize + nameSize + namePadding + descSize + descPadding

           mbNoteDesc <-
             case Nhdr.nhdrType nhdr of
               Nhdr.NT_PRSTATUS -> do
                 prs <- decodePrStatus cl d m descRaw
                 pure $ Just $ NotePrStatus prs
               Nhdr.NT_FPREGSET -> pure $ Just $ NoteFpRegSet FpRegSet
               Nhdr.NT_PRPSINFO -> pure $ Just $ NotePrpsInfo PrpsInfo
               _                -> pure $ Nothing
           let mbNote =
                 fmap
                   (\desc ->
                     Note { noteName = name
                          , noteDesc = desc })
                   mbNoteDesc

           notes <- go (BS.drop (fromIntegral noteSize) buf)
           pure $ maybeToList mbNote ++ notes

-- | Decodes the 'Note's in an ELF file using header info.
decodeHeaderNotes ::
  Elf.ElfHeaderInfo w ->
  Either NoteDecodeError [Note]
decodeHeaderNotes ehi = do
  let eh        = Elf.header ehi
      cl        = Elf.headerClass eh
      d         = Elf.headerData eh
      m         = Elf.headerMachine eh
      contents  = Elf.headerFileContents ehi
      allPhdrs  = Elf.headerPhdrs ehi
      notePhdrs = filter (\p -> Elf.phdrSegmentType p == Elf.PT_NOTE) allPhdrs
  notess <-
    Elf.elfClassInstances cl $
    traverse
      (\phdr -> decodeNotes cl d m (Elf.slice (Elf.phdrFileRange phdr) contents))
      notePhdrs
  pure $ concat notess

-----
-- Analysis
-----

-- | Errors found when analyzing core dump files.
data CoreDumpAnalyzeError
  = forall w. UnexpectedUserRegSetClass (Elf.ElfClass w) UserRegSet
  | NonCoreDumpElfType Elf.ElfType
  | CoreDumpWithoutLoadPhdr
  | CoreDumpWithoutPrStatusNote

instance Show CoreDumpAnalyzeError where
  show (UnexpectedUserRegSetClass cl regs) =
    let showClBits =
          case cl of
            Elf.ELFCLASS32 -> "32"
            Elf.ELFCLASS64 -> "64"

        showRegs =
          case regs of
            ArmUserRegSet {} -> "32-bit ARM"
            Ppc32UserRegSet {} -> "32-bit PPC"
            Ppc64UserRegSet {} -> "64-bit PPC"
            X86_64UserRegSet {} -> "x86-64"
    in showClBits ++ "-bit ELF with " ++ showRegs
  show (NonCoreDumpElfType eType) =
    "Expected an ET_CORE ELF file, but received " ++ show eType
  show CoreDumpWithoutLoadPhdr =
    "Core dump file does not contain a LOAD program header"
  show CoreDumpWithoutPrStatusNote =
    "Core dump file does not contain a PRSTATUS note"

-- | Retrieve the instruction address in the program counter at the moment when
-- a core dump occurred. Note that this computes the load address, not the
-- virtual address.
--
-- See also 'coreDumpHeaderProgramCounter', a higher-level function that also
-- looks into the core dump's 'Elf.ElfHeaderInfo' to retrieve the first @LOAD@
-- program header and the 'PrStatus' note.
coreDumpPrStatusProgramCounter ::
  forall w.
  -- | The class of ELF binary.
  Elf.ElfClass w ->
  -- | The first @LOAD@ program header in the core dump file. This is used to
  -- translate the virtual addresses in the core dump file back into load
  -- addresses.
  Elf.Phdr w ->
  -- | Used to retrieve register values.
  PrStatus ->
  -- | If successful, return the load address of the instruction where the core
  -- was dumped in a 'Right' value. Otherwise, return a 'Left' value describing
  -- what went wrong.
  Either CoreDumpAnalyzeError (Elf.ElfWordType w)
coreDumpPrStatusProgramCounter cl firstLoadPhdr prStatus =
  Elf.elfClassInstances cl $ do

  let firstLoadVirtAddr :: Elf.ElfWordType w
      firstLoadVirtAddr = Elf.phdrSegmentVirtAddr firstLoadPhdr

      -- Each architecture has its own register for the program
      -- counter/instruction pointer, so we case on all supported architectures
      -- here.
      prStatusPcVirtAddrE :: Either CoreDumpAnalyzeError (Elf.ElfWordType w)
      prStatusPcVirtAddrE =
        let userRegs :: UserRegSet
            userRegs = prReg prStatus

            throwErr :: Either CoreDumpAnalyzeError (Elf.ElfWordType w)
            throwErr = Left $ UnexpectedUserRegSetClass cl userRegs in
        case (cl, userRegs) of
          (Elf.ELFCLASS32, ArmUserRegSet regs) ->
            Right $ ARM.r15 regs
          (Elf.ELFCLASS64, ArmUserRegSet {}) ->
            throwErr

          (Elf.ELFCLASS32, Ppc32UserRegSet regs) ->
            Right $ PPC.nip regs
          (Elf.ELFCLASS64, Ppc32UserRegSet {}) ->
            throwErr

          (Elf.ELFCLASS64, Ppc64UserRegSet regs) ->
            Right $ PPC.nip regs
          (Elf.ELFCLASS32, Ppc64UserRegSet {}) ->
            throwErr

          (Elf.ELFCLASS64, X86_64UserRegSet regs) ->
            Right $ X86_64.rip regs
          (Elf.ELFCLASS32, X86_64UserRegSet {}) ->
            throwErr

  prStatusPcVirtAddr <- prStatusPcVirtAddrE
  Right $ prStatusPcVirtAddr - firstLoadVirtAddr

-- | Retrieve the instruction address in the program counter at the moment when
-- a core dump occurred. Note that this computes the load address, not the
-- virtual address.
--
-- See also 'coreDumpHeaderPrStatusProgramCounter', a lower-level function that
-- takes the first @LOAD@ program header and the 'PrStatus' note directly as
-- arguments.
coreDumpHeaderProgramCounter ::
  -- | The header information for the ELF core dump.
  Elf.ElfHeaderInfo w ->
  -- | The ELF notes decoded from the core dump.
  [Note] ->
  -- | If successful, return the load address of the instruction where the core
  -- was dumped in a 'Right' value. Otherwise, return a 'Left' value describing
  -- what went wrong.
  Either CoreDumpAnalyzeError (Elf.ElfWordType w)
coreDumpHeaderProgramCounter ehi notes =
  Elf.elfClassInstances cl $ do
    unless (ty == Elf.ET_CORE) $
      Left $ NonCoreDumpElfType ty

    -- Find the first LOAD header in the core dump file.
    let allPhdrs = Elf.headerPhdrs ehi
        loadPhdrs =
          List.filter (\p -> Elf.phdrSegmentType p == Elf.PT_LOAD) allPhdrs
        -- All examples of ELF files I've seen in the wild sort their LOAD
        -- headers by virtual address, but let's sort them just to be on
        -- the safe side.
        loadPhdrsSorted = List.sortOn Elf.phdrSegmentVirtAddr loadPhdrs
    firstLoadPhdr <-
      case loadPhdrsSorted of
        loadPhdr:_ -> Right loadPhdr
        [] -> Left CoreDumpWithoutLoadPhdr

    -- Find a PRSTATUS note in the core dump file (if one exists).
    let prStatuses =
          mapMaybe
            (\note ->
              case noteDesc note of
                NotePrStatus prs -> Just prs
                _ -> Nothing)
            notes
    prStatus <-
      case prStatuses of
        prStatus:_ -> Right prStatus
        [] -> Left CoreDumpWithoutPrStatusNote

    coreDumpPrStatusProgramCounter cl firstLoadPhdr prStatus
  where
    eh = Elf.header ehi
    cl = Elf.headerClass eh
    ty = Elf.headerType eh
