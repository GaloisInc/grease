{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

-- | Various file paths to @sympro-ui@ files and directories, which are
-- consulted in "Grease.Profiler.EmbeddedData". Ideally, we'd just put these
-- directly in "Grease.Profiler.EmbeddedData", but that would trip up GHC
-- staging restrictions.
module Grease.Profiler.Paths
  ( profilerDir
  , profileHtmlPath
  ) where

import System.FilePath ((</>))

profilerDir :: FilePath
profilerDir = "deps" </> "sympro-ui"

profileHtmlPath :: FilePath
profileHtmlPath = "profile.html"
