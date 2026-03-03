{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- This is dev-facing tool, less than ideal error handling is fine
{- HLINT ignore "Use panic" -}

-- | This program switches an ELF file back and forth from being type @ET_NONE@
-- or @ET_CORE@. In some cases you will want it to be @ET_CORE@ so you can use
-- it with GDB. In other cases you may want @objdump@ to be able to use the
-- section headers so it should be an @ET_NONE@.
--
-- This was inspired by the original @et_flip@ implementation upstream in
-- <https://github.com/elfmaster/ecfs/-/blob/main/tools/et_flip.c>.
module Main (main) where

import Control.Applicative ((<**>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ElfEdit as Elf
import qualified Data.ElfEdit.Ecfs as Ecfs
import qualified Foreign
import qualified Options.Applicative as Opt
import qualified System.IO.MMap as MMap

-- | Parse an ELF file and pass it to a continuation.
withElf :: BS.ByteString -> (forall w. Elf.Elf w -> r) -> r
withElf bs f =
  case Elf.parseElf bs of
    Elf.Elf32Res warnsAndErrs e32 ->
      let (_warns, errs) = Ecfs.partitionEcfsParseErrors warnsAndErrs
       in if null errs
            then f e32
            else error $ "Failed to parse 32-bit ELF file: " ++ show errs
    Elf.Elf64Res warnsAndErrs e64 ->
      let (_warns, errs) = Ecfs.partitionEcfsParseErrors warnsAndErrs
       in if null errs
            then f e64
            else error $ "Failed to parse 64-bit ELF file: " ++ show errs
    Elf.ElfHeaderError _ e -> error $ "Failed to parse ELF header: " ++ show e

-- | Flip an ELF file from @ET_NONE@ to @ET_CORE@ (or vice versa).
elfTypeFlip :: Opts -> IO ()
elfTypeFlip opts = do
  let file = elfFile opts
  -- First, load the ELF file from disk as a ForeignPtr using `mmap`. (See the
  -- comments near `withForeignPtr` below on why we use `mmap` in particular.)
  (fileForeignPtr, fileOffset, fileSize) <-
    MMap.mmapFileForeignPtr file MMap.ReadWrite Nothing
  -- Next, convert the ForeignPtr to a ByteString and parse it as an `Elf.Elf`
  -- value.
  let fileBs = BSI.fromForeignPtr fileForeignPtr fileOffset fileSize
  withElf fileBs $ \e ->
    -- Determine what the new ELF type should be.
    let oldType = Elf.elfType e
        newType =
          case elfType opts of
            Nothing
              | oldType == Elf.ET_NONE -> Elf.ET_CORE
              | otherwise -> Elf.ET_NONE
            Just et -> et
     in -- Finally, write the new ELF type back to the ELF file on disk.
        --
        -- Ideally, we would use elf-edit's high-level `renderElf` function to
        -- perform this transformation. Unfortunately, `renderElf` is somewhat buggy
        -- (see https://github.com/GaloisInc/elf-edit/issues/4), so we instead
        -- employ a more low-level approach where we `mmap` the ELF file's memory
        -- to a ForeignPtr and write directly to the pointer offset corresponding
        -- to the ELF type. This is probably fine for such a simple transformation,
        -- especially since the offset of ELF types in an ELF header is relatively
        -- portable and unlikely to change.
        Foreign.withForeignPtr fileForeignPtr $ \filePtr ->
          Foreign.pokeByteOff
            filePtr
            -- Per https://man7.org/linux/man-pages/man5/elf.5.html, the offset of
            -- ElfN_Ehdr's e_type field (the ELF type in an ELF header) is
            -- EI_NINDENT (16) bytes from the start of the ElfN_Ehdr struct.
            16
            (Elf.fromElfType newType)

-- | The command-line options.
data Opts = Opts
  { elfFile :: FilePath
  -- ^ The path to the ELF file to flip.
  , elfType :: Maybe Elf.ElfType
  -- ^ If @'Just' t@, then set the ELF file type to @t@. If @Nothing@, then
  -- toggle the ELF type from @ET_NONE@ to @ET_CORE@ (or vice versa).
  }

-- | The command-line option reader for 'elfType'.
elfTypeReader :: Opt.ReadM Elf.ElfType
elfTypeReader =
  Opt.eitherReader $ \str ->
    if
      | str == "ET_NONE" ->
          Right Elf.ET_NONE
      | str == "ET_REL" ->
          Right Elf.ET_REL
      | str == "ET_EXEC" ->
          Right Elf.ET_EXEC
      | str == "ET_DYN" ->
          Right Elf.ET_DYN
      | str == "ET_CORE" ->
          Right Elf.ET_CORE
      | otherwise ->
          Left $ "Unknown ELF file type: " ++ str

-- | The parser for the command-line options.
optsInfo :: Opt.ParserInfo Opts
optsInfo =
  Opt.info
    (opts <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "Flip an ELF file from ET_NONE to ET_CORE (or vice versa)."
    )
 where
  opts :: Opt.Parser Opts
  opts =
    Opts
      <$> Opt.strArgument
        ( Opt.help "path to ELF file"
            <> Opt.metavar "PATH"
        )
      <*> Opt.optional
        ( Opt.argument
            elfTypeReader
            ( Opt.help
                ( unlines
                    [ "The ELF file type to change to."
                    , "If not specified, this will flip ET_NONE files to ET_CORE,"
                    , "and it will flip all other ELF file types to ET_NONE."
                    ]
                )
                <> Opt.metavar "TYPE"
            )
        )

main :: IO ()
main = do
  opts <- Opt.execParser optsInfo
  elfTypeFlip opts
