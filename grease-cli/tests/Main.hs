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

import System.FilePath ((</>), replaceExtension, replaceExtensions)
import System.FilePath qualified as FilePath
import qualified System.Directory as Dir

import Data.FileEmbed (embedFile)
import Data.Functor ((<&>))
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import qualified Prettyprinter as PP

import qualified Control.Exception as X
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Options.Applicative as Opt

import Oughta qualified
import qualified Lumberjack as LJ

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T.U

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Translation as Trans

import Grease.Diagnostic (Diagnostic, GreaseLogAction)
import Grease.Entrypoint (Entrypoint(..), EntrypointLocation(..), entrypointNoStartupOv)
import Grease.Main (Results(..), simulateARM, simulateARMSyntax, simulatePPC32, simulatePPC32Syntax, simulateX86, simulateX86Syntax, simulateLlvm, simulateLlvmSyntax, SimOpts (..), optsToSimOpts, logResults)
import Grease.Options (Opts, optsInfo)

import Shape (shapeTests)

prelude :: Text.Text
prelude = Text.decodeUtf8 $(embedFile "tests/test.lua")

testFunction :: Text.Text
testFunction = "test"

testEntry :: Entrypoint
testEntry = entrypointNoStartupOv $ EntrypointSymbolName testFunction

-- Compute the command-line options for a test case.
--
-- Changes to this function should be reflected in @doc/dev.md@.
getTestOpts ::
  -- | The path to the program being tested.
  FilePath ->
  IO SimOpts
getTestOpts binName = do
  -- Obtain the default command-line options by running the parser with no
  -- explicit arguments.
  defaultCliOpts <- parseOpts []

  -- Obtain the command-line options from the *.config files, if they exist.
  configArgs <- parseConfigFileArgs (replaceExtensions binName "config")
  let isExeTestCase = ".elf" == FilePath.takeExtension binName
  archConfigArgs <-
    -- `test.<arch>.config` files only make sense for executable test cases, so
    -- don't bother looking for them with other types of test cases.
    if isExeTestCase
      then parseConfigFileArgs (replaceExtension binName "config")
      else pure []
  let allConfigArgs = configArgs ++ archConfigArgs

  -- If there are no command-line options from the *.config files, then use the
  -- default command-line options (plus some test suite-specific tweaks).
  -- Otherwise, use the overridden command-line options from the *.config files.
  let defaultSimOpts = optsToSimOpts defaultCliOpts
      defaultEntryPoints = entryPoints defaultSimOpts
      defaultMaxIters = maxIters defaultSimOpts
  if List.null allConfigArgs
  then pure $ testSpecificOptions testEntrypoints testMaxIters defaultSimOpts
  else do configCliOpts <- parseOpts allConfigArgs
          let configSimOpts = optsToSimOpts configCliOpts
              configEntryPoints = entryPoints configSimOpts
              configMaxIters = maxIters configSimOpts
              -- While we want to inherit most of the default values obtained
              -- from parsing the options in the *.config files, we do _not_
              -- want to inherit the entrypoint or max-iterations values if the
              -- *.config files did not explicitly set them, as we give them
              -- test-specific defaults that are different from the `grease`
              -- executable's defaults. We check whether the options were
              -- explicitly set or not by comparing the parsed options to the
              -- *.config options and seeing if they differ. (Admittedly, this
              -- is rather clunky.)
              testEntrypoints' =
                if defaultEntryPoints == configEntryPoints
                  then testEntrypoints
                  else configEntryPoints
              testMaxIters' =
                if defaultMaxIters == configMaxIters
                  then testMaxIters
                  else configMaxIters
          pure $ testSpecificOptions testEntrypoints' testMaxIters' configSimOpts
  where
    -- Parse the command-line options in a *.config file as a list of Strings,
    -- which is the format expected by optparse-applicative.
    parseConfigFileArgs :: FilePath -> IO [String]
    parseConfigFileArgs fp = do
      exists <- Dir.doesFileExist fp
      if exists
        then do contents <- Text.IO.readFile fp
                pure $ List.words $ Text.unpack contents
        else pure []

    -- Invoke the `optsInfo` optparse-applicative parser using the path to the
    -- test executable (which is mandatory) plus the additional supplied
    -- arguments.
    parseOpts :: [String] -> IO Opts
    parseOpts args =
      Opt.handleParseResult $
      Opt.execParserPure Opt.defaultPrefs optsInfo (binName : args)

    -- If not otherwise specified, default to `--symbol test` as the entrypoint.
    testEntrypoints :: [Entrypoint]
    testEntrypoints = [testEntry]

    -- If not otherwise specified, default to `--iters 32` as the maximum number
    -- of iterations.
    testMaxIters :: Maybe Int
    testMaxIters = Just maxRefinementIters

    -- While the test suite inherits most of the default test options, there are
    -- a handful of places where we override the defaults:
    --
    -- * The `binPath` is always derived from the subdirectory where the test
    --   executable resides. (This cannot be overriden, even in *.config files.)
    --
    -- * Unless otherwise specified in *.config files, the values of
    --   `entryPoints` and `maxIters` are overridden (see `testEntrypoints` and
    --   `testMaxIters`).
    testSpecificOptions :: [Entrypoint] -> Maybe Int -> SimOpts -> SimOpts
    testSpecificOptions specificEntryPoints specificMaxIters opts =
      opts
        { binPath = binName
        , entryPoints = specificEntryPoints
        , maxIters = specificMaxIters
        }

-- Arbitrary, fairly low value. Making this higher makes the xfail-iter tests
-- take longer, making it lower may cause other tests to start exceeding this
-- bound.
maxRefinementIters :: Int
maxRefinementIters = 128

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
    let isArchComment =
          Text.stripPrefix $
            case arch of
              Armv7 -> "// arm: "
              PPC32 -> "// ppc32: "
              X64 -> "// x64: "
    let isGenericComment = Text.stripPrefix "// all: "
    let isLuaComment = isArchComment <> isGenericComment
    let prog = Oughta.fromLines c isLuaComment content
    let go :: GreaseLogAction -> IO Results
        go la' = do
          opts <- getTestOpts (dir </> fileName)
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
    opts <- getTestOpts path
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
    let path = dir </> FilePath.addExtension dropped  "c"
    content <- Text.IO.readFile path
    let prog = Oughta.fromLineComments path "/// " content
    opts <- getTestOpts (dir </> fileName)
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
