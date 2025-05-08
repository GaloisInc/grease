{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Version (verStr) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFileRelative)
import Data.List (intersperse)
import Data.Text qualified as Text
import Data.Version (showVersion)

import Grease.GitRev qualified as GitRev
import Paths_grease (version)

-- | The GREASE version and @git@ commit, formatted for displaying with the
-- @--version@ flag.
verStr :: String
verStr =
  mconcat $
  intersperse "\n"
    [ "grease " ++ ver
    , "Git commit " ++ commitHash
    , "    branch " ++ commitBranch ++ dirtyLab
    ]
  where
    ver = showVersion version

    dirtyLab | commitDirty = " (non-committed files present during build)"
             | otherwise   = ""

-- Helper, not exported
commitHash :: String
commitHash
  | hash /= unknown
  = hash
  -- See Note [grease.buildinfo.json]
  | Just buildinfoVal <- Aeson.decodeStrict buildinfo
  , Just (Aeson.String buildinfoHash) <- KeyMap.lookup "hash" buildinfoVal
  = Text.unpack buildinfoHash
  | otherwise
  = unknown
  where
    hash = GitRev.hash

-- Helper, not exported
commitBranch :: String
commitBranch
  | branch /= unknown
  = branch
  -- See Note [grease.buildinfo.json]
  | Just buildinfoVal <- Aeson.decodeStrict buildinfo
  , Just (Aeson.String buildinfoCommit) <- KeyMap.lookup "branch" buildinfoVal
  = Text.unpack buildinfoCommit
  | otherwise
  = unknown
  where
    branch = GitRev.branch

-- Helper, not exported
commitDirty :: Bool
commitDirty
  | dirty
  = dirty
  -- See Note [grease.buildinfo.json]
  | Just buildinfoVal <- Aeson.decodeStrict buildinfo
  , Just (Aeson.Bool buildinfoDirty) <- KeyMap.lookup "dirty" buildinfoVal
  = buildinfoDirty
  | otherwise
  = False
  where
    dirty = GitRev.dirty

-- Helper, not exported
--
-- What to report if we are unable to determine git-related information. This
-- intentionally matches what the @gitrev@ library prints in such a scenario.
unknown :: String
unknown = "UNKNOWN"

-- Helper, not exported
--
-- See Note [grease.buildinfo.json]
buildinfo :: BS.ByteString
buildinfo = $(embedFileRelative "grease.buildinfo.json")

{-
Note [grease.buildinfo.json]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By default, we determine the git commit hash, branch, and dirty information
using the gitrev library, which invokes git at compile time to query the
relevant information in the .git subdirectory. This works well for local
developments where the git binary and the .git subdirectory are both readily
available. It does not work so well for building in a Docker image, as we
intentionally do not copy over the .git subdirectory into the image to prevent
spurious cache invalidations caused by the contents of .git changing (which they
do, quite often).

As an alternative to gitrev, we also employ a convention where a build system
can create a grease.buildinfo.json file locally which contains the necessary
git-related information. The schema for this file is:

  {
    "hash": <string>,
    "branch": <string>,
    "dirty": <bool>
  }

This way, a build system (which has access to git/.git) can write this
information to a file, proceed to build the Docker image (which does not have
access to git/.git), and then have all of the expected information embedded into
the output of --version.
-}
