{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.SimulatorHooks.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Text qualified as Text
import Data.Void (Void, absurd)
import Grease.Diagnostic.Severity (Severity (Debug))
import Prettyprinter qualified as PP

data Diagnostic where
  ExecutingInstruction ::
    -- | Address
    PP.Doc Void ->
    -- | Disassembly
    Text.Text ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      ExecutingInstruction addr dis ->
        "Executing instruction at" PP.<+> fmap absurd addr PP.<> ":" PP.<+> PP.pretty dis

severity :: Diagnostic -> Severity
severity =
  \case
    ExecutingInstruction{} -> Debug
