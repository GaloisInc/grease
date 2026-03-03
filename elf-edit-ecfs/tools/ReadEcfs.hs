{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- For the `PrintfArg (FileOffset w)` instance
{-# OPTIONS_GHC -Wno-orphans #-}

-- This is dev-facing tool, less than ideal error handling is fine
{- HLINT ignore "Use panic" -}

-- | Parses and prints the details of an ECFS file to stdout.
--
-- This was inspired by the original @readecfs@ implementation upstream in
-- <https://github.com/elfmaster/ecfs/-/blob/main/tools/readecfs.c>.
module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (unless, when)
import Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ElfEdit as Elf
import qualified Data.ElfEdit.CoreDump as Elf.CoreDump
import qualified Data.ElfEdit.CoreDump.X86_64 as Elf.CoreDump.X86_64
import qualified Data.ElfEdit.Ecfs as Ecfs
import qualified Data.ElfEdit.Prim.Auxv as Elf.Prim
import Data.Foldable (for_, traverse_)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vec
import Data.Word (Word32, Word64, Word8, byteSwap32)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import Numeric (showInt)
import qualified Options.Applicative as Opt
import System.Environment (getArgs)
import Text.Printf (PrintfArg, printf)

-- | Decode an ECFS file and pass it to a continuation.
withEcfs :: BS.ByteString -> (forall w. Ecfs.Ecfs w -> r) -> r
withEcfs bs f =
  case Ecfs.decodeEcfs bs of
    Left err -> error $ "Failed to parse ECFS file: " ++ show err
    Right (Elf.SomeElf ecfs) -> f ecfs

-- | This brings a 'PrintfArg' instance into scope for the underlying word type
-- associated with an 'ElfClass'. (Note that 'Elf.elfClassInstances' does not
-- provide this instance.)
elfPrintfArgInstance ::
  Elf.ElfClass w ->
  (PrintfArg (Elf.ElfWordType w) => r) ->
  r
elfPrintfArgInstance Elf.ELFCLASS32 k = k
elfPrintfArgInstance Elf.ELFCLASS64 k = k

deriving instance PrintfArg w => PrintfArg (Elf.FileOffset w)

lookupDynsym :: forall w. Ecfs.Ecfs w -> Word64 -> BS.ByteString
lookupDynsym ecfs addr =
  Elf.elfClassInstances cl $
    fromMaybe (BSC.pack "<unknown>") $ do
      dynsym <- Ecfs.ecfsDynsym ecfs
      dynsymEntry <-
        Vec.find
          ( \dynsymEntry ->
              fromIntegral @(Elf.ElfWordType w) @Word64 (Elf.steValue dynsymEntry) == addr
          )
          (Elf.symtabEntries dynsym)
      Just $ Elf.steName dynsymEntry
 where
  ehi = Ecfs.ecfsElfHeaderInfo ecfs
  eh = Elf.header ehi
  cl = Elf.headerClass eh

printRegisters :: Elf.CoreDump.PrStatus -> IO ()
printRegisters prStatus =
  case Elf.CoreDump.prReg prStatus of
    Elf.CoreDump.X86_64UserRegSet regs ->
      printf
        ( unlines
            [ "r15:\t%llx"
            , "r14:\t%llx"
            , "r13:\t%llx"
            , "r12:\t%llx"
            , "rbp:\t%llx"
            , "rbx:\t%llx"
            , "r11:\t%llx"
            , "r10:\t%llx"
            , "r9: \t%llx"
            , "r8: \t%llx"
            , "rax:\t%llx"
            , "rcx:\t%llx"
            , "rdx:\t%llx"
            , "rsi:\t%llx"
            , "rdi:\t%llx"
            , "rip:\t%llx"
            , "rsp:\t%llx"
            , "cs: \t%llx"
            , "ss: \t%llx"
            , "ds: \t%llx"
            , "es: \t%llx"
            , "fs: \t%llx"
            , "gs: \t%llx"
            , "eflags: %llx"
            ]
        )
        (Elf.CoreDump.X86_64.r15 regs)
        (Elf.CoreDump.X86_64.r14 regs)
        (Elf.CoreDump.X86_64.r13 regs)
        (Elf.CoreDump.X86_64.r12 regs)
        (Elf.CoreDump.X86_64.rbp regs)
        (Elf.CoreDump.X86_64.rbx regs)
        (Elf.CoreDump.X86_64.r11 regs)
        (Elf.CoreDump.X86_64.r10 regs)
        (Elf.CoreDump.X86_64.r9 regs)
        (Elf.CoreDump.X86_64.r8 regs)
        (Elf.CoreDump.X86_64.rax regs)
        (Elf.CoreDump.X86_64.rcx regs)
        (Elf.CoreDump.X86_64.rdx regs)
        (Elf.CoreDump.X86_64.rsi regs)
        (Elf.CoreDump.X86_64.rdi regs)
        (Elf.CoreDump.X86_64.rip regs)
        (Elf.CoreDump.X86_64.rsp regs)
        (Elf.CoreDump.X86_64.cs regs)
        (Elf.CoreDump.X86_64.ss regs)
        (Elf.CoreDump.X86_64.ds regs)
        (Elf.CoreDump.X86_64.es regs)
        (Elf.CoreDump.X86_64.fs regs)
        (Elf.CoreDump.X86_64.gs regs)
        (Elf.CoreDump.X86_64.eflags regs)
    Elf.CoreDump.ArmUserRegSet{} ->
      fail "ARM not currently supported"
    Elf.CoreDump.Ppc32UserRegSet{} ->
      fail "PowerPC not currently supported"
    Elf.CoreDump.Ppc64UserRegSet{} ->
      fail "PowerPC not currently supported"

-- | Converts a host address to a representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
hostAddressToTuple :: Word32 -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
  let ha = htonl ha'
      byte i = fromIntegral (ha `shiftR` i) :: Word8
   in (byte 24, byte 16, byte 8, byte 0)

-- Taken from on the implementation of showIPv4 in Data.IP.Addr
showHostAddress :: Word32 -> ShowS
showHostAddress ip =
  let (u3, u2, u1, u0) = hostAddressToTuple ip
   in foldr1 (.) . List.intersperse (showChar '.') $ map showInt [u3, u2, u1, u0]

htonl :: Word32 -> Word32
htonl =
  case targetByteOrder of
    BigEndian -> id
    LittleEndian -> byteSwap32

-- | Parses and prints the details of an ECFS file to stdout.
readEcfs :: Opts -> IO ()
readEcfs opts = do
  let file = ecfsFile opts
  bs <- BS.readFile file
  withEcfs bs $ \ecfs -> do
    let ehi = Ecfs.ecfsElfHeaderInfo ecfs
    let eh = Elf.header ehi
    let cl = Elf.headerClass eh
    Elf.elfClassInstances cl $ elfPrintfArgInstance cl $ do
      printf "\n- readecfs output for file %s\n" file

      path <-
        case Ecfs.decodeExePath ecfs of
          Just path ->
            pure path
          Nothing ->
            fail "Unable to retreive executable path (is this an ECFS file?)"
      printf "- Executable path (.exepath): %s\n" path

      putStr "- Command line: "
      for_ (Ecfs.decodeArgList ecfs) $ \argVec ->
        traverse_ (printf "%s ") argVec
      putStrLn ""

      when (printAll opts || printEcfsStuff opts || printPersonality opts) $ do
        let personality = Ecfs.ecfsPersonality ecfs
        putStrLn "- Personality"
        printf "\tdynamically linked: %s\n" $ if Ecfs.isStatic personality then "no" else "yes"
        printf "\tcompiled as PIE: %s\n" $ if Ecfs.isPie personality then "yes" else "no"
        printf "\tlocal symtab reconstruction: %s\n" $
          if Ecfs.hasLocalSymtab personality then "yes" else "no"
        printf "\tmalware heuristics: %s\n" $ if Ecfs.hasMalwareHeuristics personality then "yes" else "no"
        printf "\toriginal bin had stripped section headers: %s\n" $
          if Ecfs.hasStrippedShdrs personality then "yes" else "no"

      when (printAll opts || printEcfsStuff opts) $ do
        -- printf "- Fault location: 0x%lx\n" fault
        for_ (Ecfs.decodeThreadCount ecfs) $ \threadCount ->
          printf "- Thread count (.prstatus): %d\n" threadCount
        -- putStrLn "- Thread info (.prstatus)"
        -- putStrLn ""
        putStrLn "- Register values"
        for_ (Ecfs.decodePrStatuses ecfs) $
          \case
            Left err -> fail $ show err
            Right prStatuses -> do
              traverse_ printRegisters prStatuses
              putStrLn ""
        -- printf "- Exited on signal (.siginfo): %d\n" signo
        putStrLn "- files/pipes/sockets (.fdinfo):"
        for_ (Ecfs.decodeFdInfo ecfs) $
          \case
            Left err -> fail $ show err
            Right fdInfoVec ->
              for_ fdInfoVec $ \fdInfo -> do
                printf
                  "\t[fd: %d:%d] perms: %#lx path: %s\n"
                  (Ecfs.fd fdInfo)
                  (Ecfs.fdPos fdInfo)
                  (Ecfs.fdPerms fdInfo)
                  (Ecfs.fdPath fdInfo)
                case Ecfs.fdNet fdInfo of
                  -- NET_TCP
                  '\1' -> do
                    putStrLn "\tPROTOCOL: TCP"
                    printf
                      "\tSRC: %s:%d\n"
                      (showHostAddress (Ecfs.fdSrcAddr fdInfo) "")
                      (Ecfs.fdSrcPort fdInfo)
                    printf
                      "\tDST: %s:%d\n"
                      (showHostAddress (Ecfs.fdDstAddr fdInfo) "")
                      (Ecfs.fdDstPort fdInfo)
                    putStrLn ""
                  -- NET_UDP
                  '\2' -> do
                    putStrLn "\tPROTOCOL: UDP"
                    printf
                      "\tSRC: %s:%d\n"
                      (showHostAddress (Ecfs.fdSrcAddr fdInfo) "")
                      (Ecfs.fdSrcPort fdInfo)
                    printf
                      "\tDST: %s:%d\n"
                      (showHostAddress (Ecfs.fdDstAddr fdInfo) "")
                      (Ecfs.fdDstPort fdInfo)
                    putStrLn ""
                  _ -> pure ()
        putStrLn ""

      when (printAll opts || printEcfsStuff opts || printLibNames opts) $ do
        let libNames = Ecfs.decodeShlibMappingNames ecfs
        putStrLn "- Printing shared library mappings:"
        traverse_ (printf "shlib:\t%s\n") libNames
        putStrLn ""

      when (printAll opts || printEcfsStuff opts || printSymbols opts) $ do
        putStrLn "- Dynamic Symbol section -"
        for_ (Ecfs.ecfsDynsym ecfs) $ \dynsym ->
          for_ (Elf.symtabEntries dynsym) $ \sym ->
            printf
              ".dynsym:\t%s -\t %lx\n"
              (Text.decodeUtf8 (Elf.steName sym))
              (Elf.steValue sym)
        putStrLn ""

        putStrLn "- Symbol Table section -"
        for_ (Ecfs.ecfsSymtab ecfs) $ \symtab ->
          for_ (Elf.symtabEntries symtab) $ \sym ->
            printf
              ".symtab:\t %s -\t %lx\n"
              (Text.decodeUtf8 (Elf.steName sym))
              (Elf.steValue sym)
        putStrLn ""

      when (printAll opts || printEcfsStuff opts || printGotInfo opts) $
        unless (Ecfs.isStatic (Ecfs.ecfsPersonality ecfs)) $ do
          putStrLn "- Printing out GOT/PLT characteristics (pltgot_info_t):"
          putStrLn "gotsite            gotvalue          gotshlib          pltval              symbol"
          for_ (Ecfs.decodePltGotInfo ecfs) $ \pltGotInfos ->
            for_ pltGotInfos $ \pltGotInfo ->
              let shlEntryVa = Ecfs.shlEntryVirtAddr pltGotInfo
               in printf
                    "0x%-16lx 0x%-16lx 0x%-16lx 0x%-16lx %s\n"
                    (Ecfs.gotSite pltGotInfo)
                    (Ecfs.gotEntryVirtAddr pltGotInfo)
                    shlEntryVa
                    (Ecfs.pltEntryVirtAddr pltGotInfo)
                    (Text.decodeUtf8 (lookupDynsym ecfs shlEntryVa))
          putStrLn ""

      when (printAll opts || printEcfsStuff opts || printAuxv opts) $ do
        putStrLn "- Printing auxiliary vector (.auxvector):"
        for_ (Ecfs.decodeAuxiliaryVector ecfs) $
          \case
            Left err -> fail $ show err
            Right auxv ->
              let auxv' =
                    Vec.takeWhile
                      ( \auxvEntry ->
                          Elf.Prim.auxvEntryType auxvEntry /= Elf.Prim.AT_NULL
                      )
                      auxv
               in for_ auxv' $ \auxvEntry ->
                    let auxvEntryType = Elf.Prim.auxvEntryType auxvEntry
                     in let auxvEntryVal = Elf.Prim.auxvEntryVal auxvEntry
                         in let printIt =
                                  printf "%s:\t 0x%lx\n" (show auxvEntryType) auxvEntryVal
                             in case Elf.Prim.auxvEntryType auxvEntry of
                                  Elf.Prim.AT_IGNORE -> printIt
                                  Elf.Prim.AT_EXECFD -> printIt
                                  Elf.Prim.AT_PHDR -> printIt
                                  Elf.Prim.AT_PHENT -> printIt
                                  Elf.Prim.AT_PHNUM -> printIt
                                  Elf.Prim.AT_PAGESZ -> printIt
                                  Elf.Prim.AT_BASE -> printIt
                                  Elf.Prim.AT_FLAGS -> printIt
                                  Elf.Prim.AT_ENTRY -> printIt
                                  Elf.Prim.AT_UID -> printIt
                                  Elf.Prim.AT_EUID -> printIt
                                  Elf.Prim.AT_GID -> printIt
                                  _ -> pure ()

      when (printAll opts || printEhdr opts) $ do
        let header = Elf.header $ Ecfs.ecfsElfHeaderInfo ecfs
        putStrLn "\n- Displaying ELF header:"
        printf
          ( "e_entry:\t0x%lx\n"
              ++ "e_phnum:\t%d\n"
              ++ "e_shnum:\t%d\n"
              ++ "e_shoff:\t0x%lx\n"
              ++ "e_phoff:\t0x%lx\n"
              ++ "e_shstrndx:\t%d\n"
          )
          (Elf.headerEntry header)
          (Elf.phdrCount ehi)
          (Elf.shdrCount ehi)
          (Elf.shdrTableFileOffset ehi)
          (Elf.phdrTableFileOffset ehi)
          (Elf.shstrtabIndex ehi)

      when (printAll opts || printShdrs opts) $ do
        printf "\n- Displaying ELF section headers:\n"
        printf "Address          Offset\t   Size\t   Entsize\t   Name\n"
        traverse_
          ( \shdr ->
              printf
                "0x%-16lx 0x%-8lx 0x%-8lx 0x%-4lx %s\n"
                (Elf.shdrAddr shdr)
                (Elf.shdrOff shdr)
                (Elf.shdrSize shdr)
                (Elf.shdrEntSize shdr)
                (Text.decodeUtf8 (Elf.shdrName shdr))
          )
          (Ecfs.ecfsShstrtab ecfs)

      when (printAll opts || printPhdrs opts) $ do
        putStrLn "\n- Displaying ELF program headers:"
        putStrLn "Address          Offset\t   FileSZ\t MemSZ\tType"
        for_ (Ecfs.ecfsPhdrs ecfs) $ \phdr ->
          printf
            "0x%-16lx 0x%-8lx 0x%-8lx 0x%-4lx   %s\n"
            (Elf.phdrSegmentVirtAddr phdr)
            (Elf.phdrFileStart phdr)
            (Elf.phdrFileSize phdr)
            (Elf.phdrMemSize phdr)
            (if Elf.phdrSegmentType phdr == Elf.PT_LOAD then "LOAD" else "NOTE")
        putStrLn ""

      putStrLn ""

-- | The command-line options.
data Opts = Opts
  { ecfsFile :: !FilePath
  , printShdrs :: !Bool
  , printSymbols :: !Bool
  , printLibNames :: !Bool
  , printPhdrs :: !Bool
  , printEhdr :: !Bool
  , printGotInfo :: !Bool
  , printPersonality :: !Bool
  , printAuxv :: !Bool
  , printAll :: !Bool
  , printEcfsStuff :: !Bool
  }

-- | The parser for the command-line options.
optsInfo :: Opt.ParserInfo Opts
optsInfo =
  Opt.info
    (opts <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "Parses and prints the details of an ECFS file to stdout."
    )
 where
  opts :: Opt.Parser Opts
  opts =
    Opts
      <$> Opt.strArgument
        ( Opt.help "path to ECFS file"
            <> Opt.metavar "PATH"
        )
      <*> Opt.switch
        ( Opt.short 'S'
            <> Opt.help "print ELF section headers"
        )
      <*> Opt.switch
        ( Opt.short 's'
            <> Opt.help "print symbol table info"
        )
      <*> Opt.switch
        ( Opt.short 'l'
            <> Opt.help "print shared library names"
        )
      <*> Opt.switch
        ( Opt.short 'p'
            <> Opt.help "print ELF program headers"
        )
      <*> Opt.switch
        ( Opt.short 'h'
            <> Opt.help "print ELF header"
        )
      <*> Opt.switch
        ( Opt.short 'g'
            <> Opt.help "print PLTGOT info"
        )
      <*> Opt.switch
        ( Opt.short 'P'
            <> Opt.help "print personality info"
        )
      <*> Opt.switch
        ( Opt.short 'A'
            <> Opt.help "print auxiliary vector"
        )
      <*> Opt.switch
        ( Opt.short 'a'
            <> Opt.help "print all"
        )
      <*> Opt.switch
        ( Opt.short 'e'
            <> Opt.help "print ECFS-specific details (auxiliary vector, process state, sockets, pipes, FDs, etc.)"
        )

main :: IO ()
main = do
  let pprefs = Opt.defaultPrefs
  parseRes <- Opt.execParserPure pprefs optsInfo <$> getArgs
  -- Ensure that at least one of the flags is set.
  let parseRes' :: Opt.ParserResult Opts
      parseRes' = do
        opts <- parseRes
        unless
          ( printShdrs opts
              || printSymbols opts
              || printLibNames opts
              || printPhdrs opts
              || printEhdr opts
              || printGotInfo opts
              || printAuxv opts
              || printPersonality opts
              || printAll opts
              || printEcfsStuff opts
          )
          $ Opt.Failure
          $ Opt.parserFailure pprefs optsInfo (Opt.ShowHelpText Nothing) mempty
        pure opts
  opts <- Opt.handleParseResult parseRes'
  readEcfs opts
