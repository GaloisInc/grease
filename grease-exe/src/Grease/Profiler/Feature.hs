-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Profiler.Feature (
  greaseProfilerFeature,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Data.ByteString qualified as BS
import Data.Foldable qualified as Foldable
import Grease.Profiler.EmbeddedData (profilerDataFiles)
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.Profiling qualified as C
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

-- | Return an execution feature that enables Crucible profiling within
-- @grease@, as well as an 'Async' action that updates a profiling report at
-- periodic intervals.
greaseProfilerFeature ::
  FilePath ->
  IO (C.GenericExecutionFeature sym, Async ())
greaseProfilerFeature dir = do
  Foldable.for_ profilerDataFiles $ \(fp, contents) ->
    createDirectoriesAndWriteFile (dir </> fp) contents
  tbl <- C.newProfilingTable
  let flt = C.profilingEventFilter
  let reportPath = dir </> "report_data.js"
  let doLog = do
        C.writeProfileReport reportPath "grease-profile" "grease" tbl
        threadDelay 1000000
        doLog
  logger <- async doLog
  opt <- C.profilingFeature tbl flt Nothing
  return (opt, logger)

-- | Like 'BS.writeFile', but also create parent directories if they are
-- missing.
createDirectoriesAndWriteFile :: FilePath -> BS.ByteString -> IO ()
createDirectoriesAndWriteFile path bs = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  BS.writeFile path bs
