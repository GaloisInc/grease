{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.SetupHook.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Warn))
import Lang.Crucible.LLVM.Translation qualified as CLT
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L

data Diagnostic where
  LLVMTranslationWarning ::
    CLT.LLVMTranslationWarning -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      LLVMTranslationWarning (CLT.LLVMTranslationWarning (L.Symbol symbol) pos warn) ->
        PP.pretty symbol <> ":" <> PP.pretty pos <> ":" PP.<+> PP.pretty warn

severity :: Diagnostic -> Severity
severity =
  \case
    LLVMTranslationWarning{} -> Warn
