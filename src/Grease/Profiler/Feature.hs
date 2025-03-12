{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Profiler.Feature
  ( greaseProfilerFeature
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Extension as C
import qualified Lang.Crucible.Simulator as C
import qualified Lang.Crucible.Simulator.Profiling as C

-- grease
import Grease.Profiler.EmbeddedData (profilerDataFiles)

-- | Return an execution feature that enables Crucible profiling within
-- @grease@, as well as an 'Async' action that updates a profiling report at
-- periodic intervals.
greaseProfilerFeature ::
  forall p sym ext rtp.
  (C.IsSymInterface sym, C.IsSyntaxExtension ext) =>
  FilePath ->
  IO (C.ExecutionFeature p sym ext rtp, Async ())
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
  return (C.genericToExecutionFeature opt, logger)

-- | Like 'BS.writeFile', but also create parent directories if they are
-- missing.
createDirectoriesAndWriteFile :: FilePath -> BS.ByteString -> IO ()
createDirectoriesAndWriteFile path bs = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  BS.writeFile path bs
