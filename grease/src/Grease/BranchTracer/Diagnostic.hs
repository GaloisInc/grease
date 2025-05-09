{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.BranchTracer.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Prettyprinter qualified as PP

-- what4
import What4.ProgramLoc qualified as W4

import Grease.Diagnostic.Severity (Severity(Debug))
import Grease.Utility (ppProgramLoc)

data Diagnostic where
  ReachedBranch ::
    Maybe W4.ProgramLoc
      {- ^ The function name -} ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      ReachedBranch mloc ->
        "Branching at" PP.<+>
          case mloc of
            Just loc -> PP.pretty (ppProgramLoc loc)
            Nothing -> "unknown location"

severity :: Diagnostic -> Severity
severity =
  \case
    ReachedBranch{} -> Debug
