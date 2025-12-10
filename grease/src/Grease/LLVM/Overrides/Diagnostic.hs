{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Debug, Warn))
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L
import Text.LLVM.PP qualified as L

data Diagnostic where
  CantSkip ::
    L.Declare ->
    Diagnostic
  FoundDeclare ::
    -- | The declared function
    L.Declare ->
    Diagnostic
  RegisteredOverride ::
    forall p sym.
    -- | The override
    CLLVM.SomeLLVMOverride p sym CLLVM.LLVM ->
    Diagnostic

ppDeclare :: L.Declare -> PP.Doc a
ppDeclare d = PP.viaShow (L.ppLLVM38 (L.ppDeclare d))

instance PP.Pretty Diagnostic where
  pretty =
    \case
      CantSkip d ->
        "Can't skip function" PP.<+> ppDeclare d
      FoundDeclare d ->
        "Found `declare`:" PP.<+> ppDeclare d
      RegisteredOverride (CLLVM.SomeLLVMOverride ov) ->
        "Registered override for:" PP.<+> ppDeclare (CLLVM.llvmOverride_declare ov)

severity :: Diagnostic -> Severity
severity =
  \case
    CantSkip{} -> Warn
    FoundDeclare{} -> Debug
    RegisteredOverride{} -> Debug
