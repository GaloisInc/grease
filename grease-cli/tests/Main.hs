{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- See @doc/dev.md@ for a description of how tests are organized
module Main (main) where

import Prelude (Bounded(..), (==), (||), otherwise, error)

import System.Exit (exitFailure)
import System.IO (IO, putStrLn)
import System.FilePath ((</>), FilePath, replaceExtension, replaceExtensions, takeBaseName, takeDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

import Data.Bool (Bool(..), not)
import Data.Eq (Eq)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List ((++), map)
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for)
import Text.Show (show)
import qualified Prettyprinter as PP

import Control.Applicative (pure)
import Control.Exception (SomeException, try)
import Control.Monad (Monad((>>), (>>=), return), filterM, forM, mapM, forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Options.Applicative as Opt

import FileCheck qualified as FileCheck
import qualified Lumberjack as LJ

import qualified Test.Tasty as T
import qualified Test.Tasty.ExpectedFailure as T
import qualified Test.Tasty.HUnit as T.U

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Translation as Trans

import qualified Grease.Bug as Bug
import qualified Grease.Bug.UndefinedBehavior as UB
import Grease.Diagnostic (Diagnostic, GreaseLogAction)
import Grease.Entrypoint (Entrypoint(..), EntrypointLocation(..), entrypointNoStartupOv)
import Grease.Output (CheckStatus(..), BatchStatus (..))
import qualified Grease.Output as Output
import Grease.Main (Results(..), simulateARM, simulateARMSyntax, simulatePPC32, simulatePPC32Syntax, simulateX86, simulateX86Syntax, simulateLlvm, simulateLlvmSyntax, SimOpts (..), optsToSimOpts, logResults)
import Grease.Options (Opts, optsInfo)
import Grease.Requirement (Requirement (..), parseReq)
import Grease.Utility (pshow)

import Shape (shapeTests)

-- We silence all diagnostic messages while running the test suite to keep
-- the output relatively concise.
la :: GreaseLogAction
la = LJ.LogAction $ \_ -> pure ()

testFunction :: Text.Text
testFunction = "test"

testEntry :: Entrypoint
testEntry = entrypointNoStartupOv $ EntrypointSymbolName testFunction

-- Compute the command-line options for an executable test case.
--
-- Changes to this function should be reflected in @doc/dev.md@.
getExeTestOpts :: FilePath -> [Requirement] -> IO SimOpts
getExeTestOpts = getTestOpts True

-- Compute the command-line options for a test case that involves a program that
-- is not an executable (e.g., LLVM bitcode or an S-expression program).
--
-- Changes to this function should be reflected in @doc/dev.md@.
getNonExeTestOpts :: FilePath -> [Requirement] -> IO SimOpts
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
  -- | 'Requirement's to test.
  [Requirement] ->
  IO SimOpts
getTestOpts isExeTestCase binName rs = do
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
    -- * The `binPath` and `reqs` are always derived from the subdirectory where
    --   the test executable resides. (These cannot be overriden, even in
    --   *.config files.)
    --
    -- * Unless otherwise specified in *.config files, the values of
    --   `entryPoints` and `maxIters` are overridden (see `testEntrypoints` and
    --   `testMaxIters`).
    testSpecificOptions :: [Entrypoint] -> Maybe Int -> SimOpts -> SimOpts
    testSpecificOptions specificEntryPoints specificMaxIters opts =
      opts
        { binPath = binName
        , reqs = rs
        , entryPoints = specificEntryPoints
        , maxIters = specificMaxIters
        }

getEntrypointResult :: SimOpts -> Results -> BatchStatus
getEntrypointResult opts (Results resultMap) =
  case Map.lookup (getEntrypoint (entryPoints opts)) resultMap of
    Nothing -> error "Couldn't find result for expected entrypoint"
    Just bs -> Output.batchStatus bs
  where
    -- Currently, all test cases are expected to have exactly one entrypoint.
    getEntrypoint :: [Entrypoint] -> Entrypoint
    getEntrypoint [] = error "No entrypoints given"
    getEntrypoint [x] = x
    getEntrypoint entries = error ("Too many entry points given: " ++ show entries)

allChecks :: [Requirement]
allChecks = [minBound .. maxBound]

subdirs :: FilePath -> IO [FilePath]
subdirs dir = do
  entries <- listDirectory dir
  let absEntries = map (dir </>) entries
  filterM doesDirectoryExist absEntries

-- | Taken from the @extra@ library, which is BSD-3–licensed.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (pure [])
    where f x xs = do mbY <- op x; case mbY of Nothing -> xs; Just y -> do ys <- xs; pure $ y:ys

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

sim :: Arch -> FilePath -> [Requirement] -> IO BatchStatus
sim arch dir rs = do
  opts <- getExeTestOpts bin rs
  case arch of
    Armv7 -> getEntrypointResult opts <$> simulateARM opts la
    PPC32 -> getEntrypointResult opts <$> simulatePPC32 opts la
    X64 -> getEntrypointResult opts <$> simulateX86 opts la
  where
    bin = testCaseBinPath dir arch

xfail :: [(Arch, String)]
xfail =
  [ -- Code discovery failuure (bug in Macaw): "TopV where PSTATE_T expected"
    (Armv7, "tests/prop/in-text/pos/func_ptr"),
    -- gitlab#240
    (PPC32, "tests/refine/bug/assert_false"),
    -- https://github.com/GaloisInc/macaw/issues/418
    (PPC32, "tests/refine/neg/write_const_global"),
    -- https://github.com/GaloisInc/grease/issues/47
    (PPC32, "tests/refine/bug/uninit_stack_conditional"),
    -- Division by zero using `divw` does not trap on PPC:
    --
    -- > If you try to divide by zero or (for divw) if you try to divide
    -- > 0x80000000 by −1, then the results are garbage, and if you used the o
    -- > version of the instruction, then the overflow flag is set. No trap is
    -- > generated. (If you didn’t use the o version, then you get no indication
    -- > that anything went wrong. You just get garbage.)
    --
    -- https://devblogs.microsoft.com/oldnewthing/20180808-00/?p=99445
    (PPC32, "tests/refine/neg/libpng-cve-2018-13785"),
    (X64,   "tests/refine/bug/uninit_stack_conditional"),
    -- https://github.com/GaloisInc/grease/issues/11
    (PPC32, "tests/refine/xfail-neg/deref_arg_arg_index"),
    (X64,   "tests/refine/xfail-neg/deref_arg_arg_index")
  ]

-- | If a test binary exists, construct a test case for it (i.e., return a
-- 'Just'). Otherwise, skip the test (i.e., return a 'Nothing').
exeTestCase :: Arch -> String -> IO () -> IO (Maybe T.TestTree)
exeTestCase arch name test = do
  binPathExists <- doesFileExist (testCaseBinPath name arch)
  pure $ if binPathExists
         then Just testTree
         else Nothing
  where
    testTree :: T.TestTree
    testTree =
      if (arch, name) `List.elem` xfail
      then T.expectFail (T.U.testCase name test)
      else T.U.testCase name test

makePropTests :: Arch -> FilePath -> IO T.TestTree
makePropTests arch d =
  let kind = takeBaseName d
  in if | kind == "pos" || kind == "xfail-pos" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (oneFail subd)) subds
            return (T.testGroup d tests)
        | kind == "neg" || kind == "xfail-neg" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (allPass subd)) subds
            return (T.testGroup d tests)
        | otherwise -> putStrLn ("Unexpected directory " ++ d) >> exitFailure

  where
    oneFail :: FilePath -> IO ()
    oneFail dir = do
      let reqName = Text.pack (takeBaseName (takeDirectory (takeDirectory dir)))
      check <- parseReq reqName
      sim arch dir [check] >>= assertAssertionFailure

    allPass :: FilePath -> IO ()
    allPass dir = sim arch dir allChecks >>= assertSuccess

assertSuccess :: BatchStatus -> IO ()
assertSuccess =
  \case
    BatchBug {} -> T.U.assertFailure "Unexpected possible bug"
    BatchCantRefine {} -> T.U.assertFailure "Unexpected refinement failure"
    BatchTimeout -> T.U.assertFailure "Unexpected timeout"
    BatchItersExceeded -> T.U.assertFailure "Refinement loop iterations exceeded"
    BatchCouldNotInfer _ -> T.U.assertFailure "Failed to infer preconditions"
    -- NB: This treats not checking any assertions at all as success.
    b@(BatchChecks cs) -> forM_ cs $ \case
      CheckSuccess -> pure ()
      CheckAssertionFailure _ ->
        T.U.assertFailure (show (PP.pretty b))

assertAssertionFailure :: BatchStatus -> IO ()
assertAssertionFailure =
  \case
    BatchBug {} -> T.U.assertFailure "Unexpected possible bug"
    BatchCantRefine {} -> T.U.assertFailure "Unexpected refinement failure"
    BatchTimeout -> T.U.assertFailure "Unexpected timeout"
    BatchItersExceeded -> T.U.assertFailure "Refinement loop iterations exceeded"
    BatchCouldNotInfer _ -> T.U.assertFailure "Failed to infer preconditions"
    BatchChecks cs
      | Map.null cs ->
          T.U.assertFailure "No assertions checked"
      | otherwise ->
          forM_ cs $ \case
            CheckSuccess -> T.U.assertFailure "Assertion passed"
            CheckAssertionFailure _ -> pure ()

assertCouldNotInfer :: BatchStatus -> IO ()
assertCouldNotInfer =
  \case
    BatchBug {} -> T.U.assertFailure "Unexpected possible bug"
    BatchCantRefine {} -> T.U.assertFailure "Unexpected refinement failure"
    BatchTimeout -> T.U.assertFailure "Unexpected timeout"
    BatchItersExceeded -> T.U.assertFailure "Refinement loop iterations exceeded"
    BatchCouldNotInfer _ -> pure ()
    b@(BatchChecks cs)
      | Map.null cs ->
          T.U.assertFailure "No assertions checked"
      | otherwise ->
          forM_ cs $ \case
            CheckSuccess -> T.U.assertFailure "Assertion passed"
            CheckAssertionFailure _ ->
              T.U.assertFailure (show (PP.pretty b))

assertPossibleBug :: BatchStatus -> IO ()
assertPossibleBug =
  \case
    BatchBug {} -> pure ()
    BatchCantRefine reason -> T.U.assertFailure ("Unexpected:" <> Text.unpack (pshow reason))
    BatchTimeout -> T.U.assertFailure "Unexpected timeout"
    BatchItersExceeded -> T.U.assertFailure "Refinement loop iterations exceeded"
    BatchCouldNotInfer {} -> T.U.assertFailure "Unexpected inference failure"
    BatchChecks {} -> T.U.assertFailure "Unexpected inference success"

assertSpecificBug :: Bug.BugType -> Maybe UB.UBType -> BatchStatus -> IO ()
assertSpecificBug t ub =
  \case
    BatchBug b -> do
      T.U.assertEqual "bug types agree" t (Bug.bugType (Output.bugDesc b))
      T.U.assertEqual "UB types agree" ub (UB.ubType <$> Bug.bugUb (Output.bugDesc b))
    BatchCantRefine reason -> T.U.assertFailure ("Unexpected:" <> Text.unpack (pshow reason))
    BatchTimeout -> T.U.assertFailure "Unexpected timeout"
    BatchItersExceeded -> T.U.assertFailure "Refinement loop iterations exceeded"
    BatchCouldNotInfer {} -> T.U.assertFailure "Unexpected inference failure"
    BatchChecks {} -> T.U.assertFailure "Unexpected inference success"

makeRefineTests :: Arch -> FilePath -> IO T.TestTree
makeRefineTests arch d =
  let kind = takeBaseName d
  in if | kind == "pos" || kind == "xfail-pos" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (pass subd)) subds
            return (T.testGroup d tests)
        | kind == "neg" || kind == "xfail-neg" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (fail subd)) subds
            return (T.testGroup d tests)
        | kind == "bug" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (bug subd)) subds
            return (T.testGroup d tests)
        | otherwise -> putStrLn ("Unexpected directory " ++ d) >> exitFailure

  where
    bug :: FilePath -> IO ()
    bug dir = sim arch dir allChecks >>= assertPossibleBug

    fail :: FilePath -> IO ()
    fail dir = sim arch dir allChecks >>= assertCouldNotInfer

    pass :: FilePath -> IO ()
    pass dir = sim arch dir allChecks >>= assertSuccess

makeSanityTests :: Arch -> FilePath -> IO T.TestTree
makeSanityTests arch d =
  let kind = takeBaseName d
  in if | kind == "pass" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (pass subd)) subds
            return (T.testGroup d tests)
        | kind == "xfail-iters" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (iters subd)) subds
            return (T.testGroup d tests)
        | kind == "xfail-timeout" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (timeout subd)) subds
            return (T.testGroup d tests)
        | kind == "xfail-panic" -> do
            subds <- subdirs d
            tests <- mapMaybeM (\subd -> exeTestCase arch subd (panic subd)) subds
            return (T.testGroup d tests)
        | otherwise -> putStrLn ("Unexpected directory " ++ d) >> exitFailure

  where
    pass :: FilePath -> IO ()
    pass dir = void (sim arch dir allChecks)

    panic :: FilePath -> IO ()
    panic dir =
      try (void (sim arch dir allChecks)) >>= \(res :: Either SomeException ()) ->
        case res of
          Right _ -> T.U.assertFailure "Test did not panic"
          Left _ -> pure ()

    iters :: FilePath -> IO ()
    iters dir =
      sim arch dir allChecks >>= \case
        BatchItersExceeded -> pure ()
        _ -> T.U.assertFailure "Iterations not exceeded"

    timeout :: FilePath -> IO ()
    timeout dir =
      sim arch dir allChecks >>= \case
        BatchTimeout -> pure ()
        _ -> T.U.assertFailure "Timeout not exceeded"

llvmTests :: T.TestTree
llvmTests =
  T.testGroup "LLVM"
    [ testCase "abort" "abort.llvm.cbl"
    , testCase "both branches abort" "both-branches-abort.llvm.cbl"
    , testCase "double free" "double-free.llvm.cbl"
    , testCase "free non pointer" "free-non-ptr.llvm.cbl"
    , testCase "free stack pointer" "free-stack.llvm.cbl"
    , testCase "uninit stack" "uninit-stack.llvm.cbl"
    , testCase "empty" "empty.llvm.cbl"
    , testCase "exit" "exit.llvm.cbl"
    , testCase "function call" "id-bool.llvm.cbl"
    , testCase "free" "free.llvm.cbl"
    , testCase "func-ptr" "func-ptr.llvm.cbl"
    , testCase "func-ptr-error" "func-ptr-error.llvm.cbl"
    , testCase "load" "load.llvm.cbl"
    , testCase "malloc" "malloc.llvm.cbl"
    , testCase "memcpy" "memcpy.llvm.cbl"
    , testCase "memset" "memset.llvm.cbl"
    , testCase "memset large const len" "memset-large-const-len.llvm.cbl"
    , testCase "null read" "null-read.llvm.cbl"
    , testCase "null write" "null-write.llvm.cbl"
    , testCase "pointer add offset" "ptr-add-offset.llvm.cbl"
    , testCase "pointer add large offset" "ptr-add-large-offset.llvm.cbl"
    , testCase "skip non-void call" "skip-ptr.llvm.cbl"
    , testCase "skip void call" "skip-void.llvm.cbl"
    , testCase "store" "store.llvm.cbl"
    , testCase "trap" "trap.llvm.cbl"
    , testCase "user override" "user-override.llvm.cbl"
    , testCase "user override defun" "user-override-defun.llvm.cbl"
    , testCase "struct override" "struct-override-caller.llvm.cbl"
    , testCase "declare in override" "declare-in-override/declare-in-override.llvm.cbl"
    , testCase "startup override" "startup-override/test.llvm.cbl"
    , testCase "Rust enum" "rust-enum.llvm.cbl"
    ]
  where

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
      logs <- List.reverse <$> IORef.readIORef logRef
      let logTxt = Text.unlines (map (Text.pack . show . PP.pretty) logs)
      pure logTxt

    testCase ::
      T.TestName ->
      FilePath ->
      T.TestTree
    testCase testName fileName =
      T.U.testCase testName $ do
        let path = "tests/llvm" </> fileName
        content <- Text.IO.readFile path
        opts <- getNonExeTestOpts path []
        logTxt <-
          withCapturedLogs $ \la' -> do
            res <- simulateLlvmSyntax opts la'
            logResults la' res
        let output = FileCheck.Output logTxt
        let prefix = Nothing
        let comment = "; "
        (cmds, _) <- FileCheck.parseCommentsAndCheck' prefix comment (Just path) content output
        T.U.assertBool "Test has some assertions" (not (List.null cmds))
        pure ()

llvmBcTests :: T.TestTree
llvmBcTests =
  T.testGroup "LLVM bitcode"
    [ testCase "malloc/free redefined" "malloc_free_redefined" $ assertSpecificBug Bug.MustFail (Just UB.DoubleFree)
    , testCase "memset" "memset" assertSuccess
    , testCase "skip" "skip" assertSuccess
    , testCase "load-handle bc" "load_handle_bc" assertSuccess
    , testCase "declare in override" "declare-in-override" assertSuccess
    , testCase "startup override" "startup-override" assertSuccess
    , testCase "multiple defines" "multiple_defines" $ assertSpecificBug Bug.MustFail Nothing
    ]
  where
    testCase ::
      T.TestName ->
      FilePath ->
      (BatchStatus -> IO ()) ->
      T.TestTree
    testCase testName dirName assertCont =
      T.U.testCase testName $ do
        opts <- getNonExeTestOpts ("tests/llvm-bc" </> dirName </> "test.bc") []
        res <- simulateLlvm Trans.defaultTranslationOptions opts la
        assertCont $ getEntrypointResult opts res

armCfgTests :: T.TestTree
armCfgTests =
  T.testGroup "ARM CFG"
    [ testCase "id" "id.armv7l.cbl" $ \_ -> pure ()
    , testCase "user override" "user-override.armv7l.cbl" assertSuccess
    , testCase "declare in override" "declare-in-override/declare-in-override.armv7l.cbl" assertSuccess
    , testCase "startup override" "startup-override/test.armv7l.cbl" assertSuccess
    ]
  where
    testCase ::
      T.TestName ->
      FilePath ->
      (BatchStatus -> IO ()) ->
      T.TestTree
    testCase testName fileName assertCont =
      T.U.testCase testName $ do
        opts <- getNonExeTestOpts ("tests/arm" </> fileName) []
        res <- simulateARMSyntax opts la
        assertCont $ getEntrypointResult opts res

ppc32CfgTests :: T.TestTree
ppc32CfgTests =
  T.testGroup "PPC32 CFG"
    [ testCase "id" "id.ppc32.cbl" $ \_ -> pure ()
    , testCase "pos-stack-offset-read" "pos-stack-offset-read.ppc32.cbl" $ assertSpecificBug Bug.UninitStackRead Nothing
    , testCase "pos-stack-offset-write" "pos-stack-offset-write.ppc32.cbl" $ assertSpecificBug Bug.MustFail Nothing
    , testCase "user override" "user-override.ppc32.cbl" assertSuccess
    , testCase "declare in override" "declare-in-override/declare-in-override.ppc32.cbl" assertSuccess
    , testCase "startup override" "startup-override/test.ppc32.cbl" assertSuccess
    ]
  where
    testCase ::
      T.TestName ->
      FilePath ->
      (BatchStatus -> IO ()) ->
      T.TestTree
    testCase testName fileName assertCont =
      T.U.testCase testName $ do
        opts <- getNonExeTestOpts ("tests/ppc32" </> fileName) []
        res <- simulatePPC32Syntax opts la
        assertCont $ getEntrypointResult opts res

x86CfgTests :: T.TestTree
x86CfgTests =
  T.testGroup "x86_64 CFG"
    [ testCase "id" "id.x64.cbl" $ \_ -> pure ()
    , testCase "null-read" "null-read.x64.cbl" $ assertSpecificBug Bug.MustFail Nothing
    , testCase "null-write" "null-write.x64.cbl" $ assertSpecificBug Bug.MustFail Nothing
    , testCase "pos-stack-offset-read" "pos-stack-offset-read.x64.cbl" $ assertSpecificBug Bug.UninitStackRead Nothing
    , testCase "pos-stack-offset-write" "pos-stack-offset-write.x64.cbl" $ assertSpecificBug Bug.MustFail Nothing
    , testCase "user override" "user-override.x64.cbl" assertSuccess
    , testCase "declare in override" "declare-in-override/declare-in-override.x64.cbl" assertSuccess
    , testCase "startup override" "startup-override/test.x64.cbl" assertSuccess
    , testCase "write to rax pointer" "write-to-rax-pointer.x64.cbl" assertSuccess
    ]
  where
    testCase ::
      T.TestName ->
      FilePath ->
      (BatchStatus -> IO ()) ->
      T.TestTree
    testCase testName fileName assertCont =
      T.U.testCase testName $ do
        opts <- getNonExeTestOpts ("tests/x86" </> fileName) []
        res <- simulateX86Syntax opts la
        assertCont $ getEntrypointResult opts res

main :: IO ()
main = do
  archTests <-
    for arches $ \arch -> do
      props <- subdirs "tests/prop"
      propTestGroups <-
        forM props $ \prop -> do
          dirs <- subdirs prop
          tests <- mapM (makePropTests arch) dirs
          return (T.testGroup prop tests)
      let propTests = T.testGroup "prop" propTestGroups

      refineTests <- do
        dirs <- subdirs "tests/refine"
        tests <- mapM (makeRefineTests arch) dirs
        return (T.testGroup "refine" tests)

      sanityTests <- do
        dirs <- subdirs "tests/sanity"
        tests <- mapM (makeSanityTests arch) dirs
        return (T.testGroup "sanity" tests)

      return (T.testGroup (ppArch arch) [propTests, refineTests, sanityTests])

  T.defaultMain $ T.testGroup "Tests" (shapeTests:llvmTests:llvmBcTests:armCfgTests:ppc32CfgTests:x86CfgTests:archTests)
