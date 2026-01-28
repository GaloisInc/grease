{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Panic (Grease, panic) where

import GHC.Stack (HasCallStack)
import Panic (
  PanicComponent (
    panicComponentIssues,
    panicComponentName,
    panicComponentRevision
  ),
  useGitRevision,
 )
import Panic qualified

data Grease = Grease

-- | `panic` represents an error condition that should only
--   arise due to a programming error. It will exit the program
--   and print a message asking users to open a ticket.
panic ::
  HasCallStack =>
  -- | Short name of where the error occurred
  String ->
  -- | More detailed description of the error
  [String] ->
  a
panic = Panic.panic Grease

instance PanicComponent Grease where
  panicComponentName _ = "GREASE"
  panicComponentIssues _ = "https://github.com/GaloisInc/grease/issues"

  {-# NOINLINE panicComponentRevision #-}
  panicComponentRevision = $useGitRevision
