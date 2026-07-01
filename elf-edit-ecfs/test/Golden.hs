-- | Tests for the @readecfs@ example program, and through it the decoders in
-- "Data.ElfEdit.Ecfs".
--
-- There are two groups:
--
-- * Golden tests run @readecfs@ with each flag against a checked-in ECFS
--   snapshot and compare against a golden file in @test\/golden@. To regenerate
--   the golden files after an intentional change, run the suite with @--accept@
--   (e.g. @cabal test elf-edit-ecfs --test-options=--accept@) and review the
--   diff.
--
-- * A differential test compares our @readecfs@ against the upstream C
--   @readecfs@ from <https://github.com/elfmaster/ee-ecfs>, which acts as an
--   oracle. It is skipped unless the @ECFS_C_READECFS@ environment variable
--   points at the upstream executable on the machine running the tests (build
--   it from @ee-ecfs@ or use an installed copy, e.g.
--   @ECFS_C_READECFS=/opt/ecfs/bin/readecfs cabal test elf-edit-ecfs@). We
--   cannot just look for @readecfs@ on @PATH@: @build-tool-depends@ puts /our/
--   @readecfs@ there, so the name is ambiguous.
--
-- The snapshot is the same one used by @ghidra-ecfs@ and @screach@, referenced
-- by a relative path so there is a single source of truth.
module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import qualified System.Process as Proc
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | The ECFS snapshot under test, relative to this package's directory (the
-- working directory when @cabal test@ runs the suite).
snapshotPath :: FilePath
snapshotPath = ".." </> "ghidra-ecfs" </> "src" </> "test" </> "resources" </> "ecfs" </> "test.x64.elf"

-- | Directory holding the golden files.
goldenDir :: FilePath
goldenDir = "test" </> "golden"

-- | A single golden case: a descriptive name and the @readecfs@ flag that
-- selects which decoders to exercise.
data Case = Case
  { caseName :: String
  , caseFlag :: String
  }

cases :: [Case]
cases =
  [ Case "ehdr" "-h"
  , Case "shdrs" "-S"
  , Case "symbols" "-s"
  , Case "libnames" "-l"
  , Case "phdrs" "-p"
  , Case "gotinfo" "-g"
  , Case "personality" "-P"
  , Case "auxv" "-A"
  , Case "ecfs" "-e"
  ]

-- | Run a @readecfs@-like program with the given flag against 'snapshotPath',
-- capturing stdout and stderr together. Fails loudly on a non-zero exit rather
-- than baking an error into a golden file.
runReadEcfs :: FilePath -> String -> IO Text
runReadEcfs exe flag = do
  let cp = Proc.proc exe [flag, snapshotPath]
  (ec, out, err) <- Proc.readCreateProcessWithExitCode cp ""
  let combined = out ++ err
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c ->
      error $ exe ++ " " ++ flag ++ " exited with code " ++ show c ++ ":\n" ++ combined
  pure (Text.filter (/= '\NUL') (Text.pack combined))

-- | Normalize output for differential comparison: strip trailing whitespace
-- from each line. NUL bytes are already stripped by 'runReadEcfs'.
normalize :: Text -> Text
normalize = Text.unlines . map Text.stripEnd . Text.lines

-- | Golden test for one flag: diff our @readecfs@ output against the golden
-- file. 'goldenVsString' handles @--accept@ and prints a diff on mismatch.
goldenTest :: Case -> TestTree
goldenTest c =
  goldenVsString
    (caseName c)
    (goldenDir </> "readecfs-" ++ caseName c <.> "out")
    (LBS.fromStrict . Text.encodeUtf8 . normalize <$> runReadEcfs "readecfs" (caseFlag c))

-- | One differential test case: compare our output for one flag against the
-- upstream C oracle, after normalizing NUL bytes (done in 'runReadEcfs') and
-- trailing whitespace on each line.
differentialTest :: FilePath -> Case -> TestTree
differentialTest cReadecfs c =
  testCase (caseName c) $ do
    ours <- normalize <$> runReadEcfs "readecfs" (caseFlag c)
    oracle <- normalize <$> runReadEcfs cReadecfs (caseFlag c)
    assertEqual ("flag " ++ caseFlag c) oracle ours

main :: IO ()
main = do
  mbCReadecfs <- lookupEnv "ECFS_C_READECFS"
  let goldenGroup = testGroup "golden" (map goldenTest cases)
  let diffGroup =
        case mbCReadecfs of
          Just exe | not (null exe) -> map (differentialTest exe) cases
          _ ->
            -- No oracle configured: record a passing placeholder so the skip is
            -- visible in test output rather than silently absent.
            [ testCase "differential (skipped)" $
                putStrLn "Set ECFS_C_READECFS to the upstream readecfs executable to enable this test."
            ]
  defaultMain $
    testGroup
      "readecfs"
      [ goldenGroup
      , testGroup "differential" diffGroup
      ]
