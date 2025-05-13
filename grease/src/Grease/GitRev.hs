{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE TemplateHaskell #-}

-- | These are placed in their own module to minimize the cost of recompilation
-- due to Template Haskell.
module Grease.GitRev (hash, branch, dirty) where

import GitHash (GitInfo, tGitInfoCwd, giHash, giBranch, giDirty)

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

hash :: String
hash = giHash gitInfo

branch :: String
branch = giBranch gitInfo

dirty :: Bool
dirty = giDirty gitInfo
