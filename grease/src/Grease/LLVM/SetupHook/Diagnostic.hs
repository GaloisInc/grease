{-|
Copyright        : (c) Galois, Inc. 2025
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.LLVM.SetupHook.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Grease.Diagnostic.Severity (Severity(Warn))
import Lang.Crucible.LLVM.Translation qualified as Trans
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L

data Diagnostic where
  LLVMTranslationWarning ::
    Trans.LLVMTranslationWarning -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      LLVMTranslationWarning (Trans.LLVMTranslationWarning (L.Symbol symbol) pos warn) ->
        PP.pretty symbol <> ":" <> PP.pretty pos <> ":" PP.<+> PP.pretty warn

severity :: Diagnostic -> Severity
severity =
  \case
    LLVMTranslationWarning{} -> Warn
