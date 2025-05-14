{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE TemplateHaskell #-}

-- | These are placed in their own module to minimize the cost of recompilation
-- due to Template Haskell.
module Grease.GitRev (hash, branch, dirty) where

import GitHash (GitInfo, giBranch, giDirty, giHash, tGitInfoCwdTry)

gitInfo :: Either String GitInfo
gitInfo = $$tGitInfoCwdTry

hash :: String
hash = case gitInfo of
    Left _ -> "UNKNOWN"
    Right gi -> giHash gi

branch :: String
branch = case gitInfo of
    Left _ -> "UNKNOWN"
    Right gi -> giBranch gi

dirty :: Bool
dirty = case gitInfo of
  Left _ -> False
  Right gi -> giDirty gi
