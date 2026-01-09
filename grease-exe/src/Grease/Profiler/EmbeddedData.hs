{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Profiler.EmbeddedData (
  profilerDataFiles,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFileRelative, makeRelativeToProject)
import Grease.Profiler.Paths (profileHtmlPath, profilerDir)
import System.FilePath ((</>))

-- | The contents of the @sympro-ui@ files needed to display the profiler UI.
-- This is kept in its own module to minimize the costs of recompilation due to
-- Template Haskell file dependency tracking.
profilerDataFiles :: [(FilePath, ByteString)]
profilerDataFiles =
  let cssFiles = map (first ("css" </>)) ($(makeRelativeToProject (profilerDir </> "css") >>= embedDir))
      jsFiles = map (first ("js" </>)) ($(makeRelativeToProject (profilerDir </> "js") >>= embedDir))
      tsFiles = map (first ("ts" </>)) ($(makeRelativeToProject (profilerDir </> "ts") >>= embedDir))
      profileHtmlContents = $(embedFileRelative (profilerDir </> profileHtmlPath))
   in (profileHtmlPath, profileHtmlContents)
        : concat @[] [cssFiles, jsFiles, tsFiles]
