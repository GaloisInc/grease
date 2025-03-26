{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE TemplateHaskell #-}

module Grease.Profiler.EmbeddedData
  ( profilerDataFiles
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFile)
import System.FilePath ((</>))

import Grease.Profiler.Paths (profilerDir, profileHtmlPath)

-- | The contents of the @sympro-ui@ files needed to display the profiler UI.
-- This is kept in its own module to minimize the costs of recompilation due to
-- Template Haskell file dependency tracking.
profilerDataFiles :: [(FilePath, ByteString)]
profilerDataFiles =
  let cssFiles = map (first ("css" </>)) ($(embedDir $ profilerDir </> "css"))
      jsFiles  = map (first ("js"  </>)) ($(embedDir $ profilerDir </> "js"))
      tsFiles  = map (first ("ts"  </>)) ($(embedDir $ profilerDir </> "ts"))
      profileHtmlContents = $(embedFile $ profilerDir </> profileHtmlPath)
  in (profileHtmlPath, profileHtmlContents)
   : concat @[] [cssFiles, jsFiles, tsFiles]
