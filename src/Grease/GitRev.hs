{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE TemplateHaskell #-}

-- | These are placed in their own module to minimize the cost of recompilation
-- due to Template Haskell.
module Grease.GitRev (hash, branch, dirty) where

import Development.GitRev (gitBranch, gitDirty, gitHash)

hash :: String
hash = $(gitHash)

branch :: String
branch = $(gitBranch)

dirty :: Bool
dirty = $(gitDirty)
