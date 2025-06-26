{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Skip.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Debug))
import Prettyprinter qualified as PP
import What4.FunctionName qualified as W4

data Diagnostic where
  FunctionCall ::
    -- | The function name
    W4.FunctionName ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      FunctionCall fnName ->
        "Invoking the" PP.<+> PP.squotes (PP.pretty fnName) PP.<+> "function"

severity :: Diagnostic -> Severity
severity =
  \case
    FunctionCall{} -> Debug
