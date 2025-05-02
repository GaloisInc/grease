{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- See @doc/dev.md@ for a description of how tests are organized
module Main (main) where

import Prelude hiding (fail)

import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import qualified System.Directory as Dir

import Data.FileEmbed (embedFile)
import Data.Functor ((<&>))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import qualified Prettyprinter as PP

import qualified Control.Exception as X
import Control.Monad.IO.Class (MonadIO, liftIO)

import Oughta qualified
import qualified Lumberjack as LJ

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T.U

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Translation as Trans

import Grease.Cli (optsFromList)
import Grease.Diagnostic (Diagnostic, GreaseLogAction)
import Grease.Main (Results(..), simulateARM, simulateARMSyntax, simulatePPC32, simulatePPC32Syntax, simulateX86, simulateX86Syntax, simulateLlvm, simulateLlvmSyntax, SimOpts (..), optsToSimOpts, logResults)

import Shape (shapeTests)

prelude :: Text.Text
prelude = Text.decodeUtf8 $(embedFile "tests/test.lua")

-- Compute the command-line options for a test case.
--
-- Changes to this function should be reflected in @doc/dev.md@.
getTestOpts ::
  Maybe Arch ->
  -- | The content of the file potentially containing flags.
  Text.Text ->
  -- | The path to the program to be simulated.
  FilePath ->
  IO SimOpts
getTestOpts mArch content binName = do
  let ls = Text.lines content
  let isConfigComment =
        mconcat
        [ Text.stripPrefix "; flags: "
        , Text.stripPrefix "// flags: "
        , case mArch of
            Nothing -> const Nothing
            Just arch -> Text.stripPrefix ("// flags(" <> archComment arch <> "):")
        ]
  let configLines = Maybe.mapMaybe isConfigComment ls
  let args = List.concatMap Text.words configLines
  optsToSimOpts <$> optsFromList (binName : map Text.unpack args)

data Arch = Armv7 | PPC32 | X64
  deriving Eq

ppArch :: Arch -> String
ppArch Armv7 = "armv7l"
ppArch PPC32 = "ppc32"
ppArch X64 = "x64"

parseArch :: String -> Arch
parseArch s =
  if | s == ppArch Armv7 -> Armv7
     | s == ppArch PPC32 -> PPC32
     | s == ppArch X64 -> X64
     | otherwise -> error ("Unknown architecture: " ++ s)

-- Format for architecture-specific directives in comments in test files
archComment :: Arch -> Text.Text
archComment =
  \case
    Armv7 -> "arm"
    PPC32 -> "ppc32"
    X64 -> "x64"

capture ::
  MonadIO m =>
  IORef.IORef [Diagnostic] ->
  LJ.LogAction m Diagnostic
capture logRef =
  LJ.LogAction (\msg -> liftIO (IORef.modifyIORef logRef (msg :)))

withCapturedLogs ::
  (GreaseLogAction -> IO ()) ->
  IO Text.Text
withCapturedLogs withLogAction = do
  logRef <- IORef.newIORef []
  withLogAction (capture logRef)
  -- Reverse the list so that logs appear chronologically
  logs <- List.reverse <$> IORef.readIORef logRef
  let logTxt = Text.unlines (map (Text.pack . show . PP.pretty) logs)
  pure logTxt

-- | Make an "Oughta"-based test
oughta ::
  (GreaseLogAction -> IO Results) ->
  FilePath ->
  Oughta.LuaProgram ->
  IO ()
oughta go path prog0 = do
  let action =
        withCapturedLogs $ \la' -> do
          res <- go la'
          logResults la' res
  logTxt <-
    X.try @X.SomeException action <&>
      \case
        Left e -> Text.pack ("Exception: " ++ show e)
        Right t -> t
  Text.IO.writeFile (FilePath.replaceExtension path "out") logTxt
  let output = Oughta.Output (Text.encodeUtf8 logTxt)
  let prog = Oughta.addPrefix prelude prog0
  Oughta.Result r <- Oughta.check prog output
  case r of
    Left f -> X.throwIO f
    Right s ->
      let ms = Oughta.successMatches s in
      T.U.assertBool "Test has some assertions" (not (Seq.null ms))

-- | Make a "Oughta"-based tests for binaries
oughtaBin ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaBin dir fileName = do
  let drop1 = FilePath.dropExtension fileName
  let drop2 = FilePath.dropExtension drop1
  let archName = List.drop 1 (FilePath.takeExtension drop1)
  let arch = parseArch archName
  T.U.testCase (ppArch arch) $ do
    let c = dir </> FilePath.addExtension drop2 "c"
    content <- Text.IO.readFile c
    let isArchComment = Text.stripPrefix ("// " <> archComment arch <> ":")
    let isGenericComment = Text.stripPrefix "// all: "
    let isLuaComment = isArchComment <> isGenericComment
    let prog = Oughta.fromLines c isLuaComment content
    let go :: GreaseLogAction -> IO Results
        go la' = do
          opts <- getTestOpts (Just arch) content (dir </> fileName)
          case arch of
            Armv7 -> simulateARM opts la'
            PPC32 -> simulatePPC32 opts la'
            X64 -> simulateX86 opts la'
    oughta go (dir </> fileName) prog

-- | Make an "Oughta"-based test for an S-expression program
oughtaSexp ::
  (SimOpts -> GreaseLogAction -> IO Results) ->
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaSexp go dir fileName =
  T.U.testCase (FilePath.dropExtension (FilePath.dropExtension fileName)) $ do
    let path = dir </> fileName
    content <- Text.IO.readFile path
    let prog = Oughta.fromLineComments path ";; " content
    opts <- getTestOpts Nothing content path
    oughta (go opts) path prog

-- | Make an "Oughta"-based test for an LLVM bitcode program
oughtaBc ::
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaBc dir fileName =
  let dropped = FilePath.dropExtension fileName in
  T.U.testCase dropped $ do
    let c = dir </> FilePath.addExtension dropped  "c"
    content <- Text.IO.readFile c
    let prog = Oughta.fromLineComments c "/// " content
    opts <- getTestOpts Nothing content (dir </> fileName)
    let go = simulateLlvm Trans.defaultTranslationOptions
    oughta (go opts) (dir </> fileName) prog

-- | Create a test from a file, depending on the extension
fileTest :: FilePath -> FilePath -> [T.TestTree]
fileTest d f =
  let (f', ext) = FilePath.splitExtension f in
  case ext of
    ".bc" -> [oughtaBc d f]
    ".cbl" ->
      case FilePath.takeExtension f' of
        ".armv7l" -> [oughtaSexp simulateARMSyntax d f]
        ".llvm" -> [oughtaSexp simulateLlvmSyntax d f]
        ".ppc32" -> [oughtaSexp simulatePPC32Syntax d f]
        ".x64" -> [oughtaSexp simulateX86Syntax d f]
        _ -> []
    ".elf" -> [oughtaBin d f]
    _ -> []

-- | Recursively walk a directory tree, discovering tests
discoverTests :: FilePath -> IO T.TestTree
discoverTests d = do
  entries <- Dir.listDirectory d
  fmap (T.testGroup (FilePath.takeBaseName d) . List.concat) $
    for entries $ \ent -> do
      -- skip test support files
      if ent == "extra" || ".aux" `List.isInfixOf` ent
      then pure []
      else do
        let path = d </> ent
        isDir <- Dir.doesDirectoryExist path
        if isDir
        then do
          tests <- discoverTests path
          pure [tests]
        else pure (fileTest d ent)

main :: IO ()
main = do
  -- Each entry in this list should be documented in doc/dev.md
  let dirs =
        [ "llvm"
        , "llvm-bc"
        , "ppc32"
        , "prop"
        , "refine"
        , "sanity"
        , "x86"
        ]
  tests <- mapM discoverTests (map ("tests" </>) dirs)
  T.defaultMain $ T.testGroup "Tests" (shapeTests:tests)
