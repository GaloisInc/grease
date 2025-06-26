{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Extract ELF notes from a core dump file and print various information
-- related to the core dump.
module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.ElfEdit.CoreDump qualified as CoreDump
import Numeric (showHex)
import Options.Applicative qualified as Opt
import Text.Show.Pretty (pPrint)

-- | Decode an ELF file and pass it to a continuation.
withElfHeader :: BS.ByteString -> (forall w. Elf.ElfHeaderInfo w -> r) -> r
withElfHeader bs f =
  case Elf.decodeElfHeaderInfo bs of
    Left (_, err) -> error ("Failed to parse ELF file: " ++ show err)
    Right (Elf.SomeElf e) -> f e

-- | Extract ELF notes from a core dump file and print various information
-- related to the core dump to @stdout@.
extractCoreDump :: Opts -> IO ()
extractCoreDump opts = do
  let file = coreDumpFile opts
  fileBs <- BS.readFile file
  withElfHeader fileBs $ \(ehi :: Elf.ElfHeaderInfo w) ->
    let eh = Elf.header ehi
        cl = Elf.headerClass eh
     in Elf.elfClassInstances cl $ do
          unless (Elf.headerType eh == Elf.ET_CORE) $
            fail $
              file ++ " is not an ELF core dump file."

          -- Pretty-print the ELF note sections.
          notes <-
            case CoreDump.decodeHeaderNotes ehi of
              Left err -> fail $ show err
              Right notes -> pure notes
          pPrint notes

          -- If enough information is present, print the instruction address where
          -- the core was dumped. Otherwise, print nothing.
          case CoreDump.coreDumpHeaderProgramCounter ehi notes of
            Left _err ->
              pure ()
            Right prStatusPc ->
              putStrLn $
                unlines
                  [ ""
                  , "Address where core was dumped: 0x" ++ showHex prStatusPc ""
                  ]

-- | The command-line options.
newtype Opts = Opts
  { coreDumpFile :: FilePath
  -- ^ The path to the core dump file to extract.
  }

-- | The parser for the command-line options.
optsInfo :: Opt.ParserInfo Opts
optsInfo =
  Opt.info
    (opts <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "Extract ELF notes from a core dump file."
    )
 where
  opts :: Opt.Parser Opts
  opts =
    Opts
      <$> Opt.strArgument
        ( Opt.help "path to core dump file"
            <> Opt.metavar "PATH"
        )

main :: IO ()
main = do
  opts <- Opt.execParser optsInfo
  extractCoreDump opts
