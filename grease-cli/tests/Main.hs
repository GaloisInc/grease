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

import System.FilePath ((</>), replaceExtension, replaceExtensions, takeDirectory)
import System.FilePath qualified as FilePath
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
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
import Control.Monad (filterM, forM)
import qualified Control.Monad as Monad
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

-- Compute the command-line options for an executable test case.
--
-- Changes to this function should be reflected in @doc/dev.md@.
getExeTestOpts :: FilePath -> IO SimOpts
getExeTestOpts = getTestOpts True

-- Compute the command-line options for a test case that involves a program that
-- is not an executable (e.g., LLVM bitcode or an S-expression program).
--
-- Changes to this function should be reflected in @doc/dev.md@.
getNonExeTestOpts :: FilePath -> IO SimOpts
getNonExeTestOpts = getTestOpts False

-- The workhorse for 'getExeTestOpts' and 'getNonExeTestOpts'.
--
-- Changes to this function should be reflected in @doc/dev.md@.
getTestOpts ::
  -- | 'True' if this is a test case for an executable, 'False' otherwise. If
  -- 'True', look for optional @test.<arch>.config@ files.
  Bool ->
  -- | The path to the program being tested.
  FilePath ->
  IO SimOpts
getTestOpts isExeTestCase binName = do
  -- Obtain the default command-line options by running the parser with no
  -- explicit arguments.
  defaultCliOpts <- parseOpts []

  -- Obtain the command-line options from the *.config files, if they exist.
  configArgs <- parseConfigFileArgs (replaceExtensions binName "config")
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
      exists <- doesFileExist fp
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

subdirs :: FilePath -> IO [FilePath]
subdirs dir = do
  entries <- listDirectory dir
  let absEntries = map (dir </>) entries
  filterM doesDirectoryExist absEntries

-- Arbitrary, fairly low value. Making this higher makes the xfail-iter tests
-- take longer, making it lower may cause other tests to start exceeding this
-- bound.
maxRefinementIters :: Int
maxRefinementIters = 128

data Arch = Armv7 | PPC32 | X64
  deriving Eq

arches :: [Arch]
arches = [Armv7, PPC32, X64]

ppArch :: Arch -> String
ppArch Armv7 = "armv7l"
ppArch PPC32 = "ppc32"
ppArch X64 = "x86_64"

testCaseBinPath :: FilePath -> Arch -> FilePath
testCaseBinPath dir arch =
  case arch of
    Armv7 -> dir </> "test.armv7l.elf"
    PPC32 -> dir </> "test.ppc32.elf"
    X64   -> dir </> "test.x64.elf"

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
  (GreaseLogAction -> IO Results) ->
  Arch ->
  -- | Directory
  FilePath ->
  -- | File
  FilePath ->
  T.TestTree
oughtaBin go arch dir fileName =
  T.U.testCase (FilePath.takeBaseName dir) $ do
    let drop1 = FilePath.dropExtension fileName
    let drop2 = FilePath.dropExtension drop1
    let path = dir </> FilePath.addExtension drop2 "c"
    content <- Text.IO.readFile path
    let isArchComment =
          Text.stripPrefix $
            case arch of
              Armv7 -> "// arm: "
              PPC32 -> "// ppc32: "
              X64 -> "// x64: "
    let isGenericComment = Text.stripPrefix "// all: "
    let isLuaComment = isArchComment <> isGenericComment
    let prog = Oughta.fromLines path isLuaComment content
    oughta go (dir </> fileName) prog

-- | Make a "Oughta"-based tests for binaries from a directory
oughtaDir :: Arch -> FilePath -> IO T.TestTree
oughtaDir arch d = do
  subds <- subdirs d
  tests <-
    for subds $ \subd -> do
      let binPath = testCaseBinPath subd arch
      binPathExists <- doesFileExist binPath
      if not binPathExists
      then pure Nothing
      else do
        let go :: GreaseLogAction -> IO Results
            go la' = do
              opts <- getExeTestOpts binPath
              case arch of
                Armv7 -> simulateARM opts la'
                PPC32 -> simulatePPC32 opts la'
                X64 -> simulateX86 opts la'
        let dir = takeDirectory binPath
        let file = FilePath.takeFileName binPath
        pure (Just (oughtaBin go arch dir file))
  pure (T.testGroup (FilePath.takeBaseName d) (Maybe.catMaybes tests))

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
    opts <- getNonExeTestOpts path
    oughta (go opts) path prog

findWithExt :: FilePath -> String -> IO [FilePath]
findWithExt dir ext = do
  entries <- Dir.listDirectory dir
  files <- Monad.filterM (Dir.doesFileExist . (dir </>)) entries
  pure (List.filter ((== ext) . FilePath.takeExtension) files)

llvmTests :: IO T.TestTree
llvmTests = do
  let dir = "tests/llvm"
  cbls <- findWithExt dir ".cbl"
  let mkTest = oughtaSexp simulateLlvmSyntax dir
  pure (T.testGroup "LLVM CFG" (List.map mkTest cbls))

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
    opts <- getNonExeTestOpts (dir </> fileName)
    let go = simulateLlvm Trans.defaultTranslationOptions
    oughta (go opts) (dir </> fileName) prog

llvmBcTests :: IO T.TestTree
llvmBcTests = do
  let dir = "tests/llvm-bc"
  bcs <- findWithExt dir ".bc"
  let mkTest = oughtaBc dir
  pure (T.testGroup "LLVM bitcode" (List.map mkTest bcs))

armCfgTests :: IO T.TestTree
armCfgTests = do
  let dir = "tests/arm"
  cbls <- findWithExt dir ".cbl"
  let mkTest = oughtaSexp simulateARMSyntax dir
  pure (T.testGroup "ARM CFG" (List.map mkTest cbls))

ppc32CfgTests :: IO T.TestTree
ppc32CfgTests = do
  let dir = "tests/ppc32"
  cbls <- findWithExt dir ".cbl"
  let mkTest = oughtaSexp simulatePPC32Syntax dir
  pure (T.testGroup "PPC32 CFG" (List.map mkTest cbls))

x86CfgTests :: IO T.TestTree
x86CfgTests = do
  let dir = "tests/x86"
  cbls <- findWithExt dir ".cbl"
  let mkTest = oughtaSexp simulateX86Syntax dir
  pure (T.testGroup "x86_64 CFG" (List.map mkTest cbls))

main :: IO ()
main = do
  archTests <-
    for arches $ \arch -> do
      props <- subdirs "tests/prop"
      propTestGroups <-
        forM props $ \prop -> do
          dirs <- subdirs prop
          tests <- mapM (oughtaDir arch) dirs
          return (T.testGroup prop tests)
      let propTests = T.testGroup "prop" propTestGroups

      refineTests <- do
        dirs <- subdirs "tests/refine"
        tests <- mapM (oughtaDir arch) dirs
        return (T.testGroup "refine" tests)

      sanityTests <- do
        dirs <- subdirs "tests/sanity"
        tests <- mapM (oughtaDir arch) dirs
        return (T.testGroup "sanity" tests)

      return (T.testGroup (ppArch arch) [propTests, refineTests, sanityTests])

  llTests <- llvmTests
  bcTests <- llvmBcTests
  armTests <- armCfgTests
  ppc32Tests <- ppc32CfgTests
  x86Tests <- x86CfgTests
  T.defaultMain $ T.testGroup "Tests" (shapeTests:llTests:bcTests:armTests:ppc32Tests:x86Tests:archTests)
