{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.LLVM.Overrides.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import qualified Prettyprinter as PP

import qualified Text.LLVM.AST as L
import qualified Text.LLVM.PP as L

import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM

import Grease.Diagnostic.Severity (Severity(Debug))

data Diagnostic where
  FoundDeclare ::
    L.Declare
      {- ^ The declared function -} ->
    Diagnostic
  RegisteredOverride ::
    forall p sym.
    CLLVM.SomeLLVMOverride p sym CLLVM.LLVM
      {- ^ The override -} ->
    Diagnostic

ppDeclare :: L.Declare -> PP.Doc a
ppDeclare d = PP.viaShow (L.ppLLVM38 (L.ppDeclare d))

instance PP.Pretty Diagnostic where
  pretty =
    \case
      FoundDeclare d ->
        "Found `declare`:" PP.<+> ppDeclare d
      RegisteredOverride (CLLVM.SomeLLVMOverride ov) ->
        "Registered override for:" PP.<+> ppDeclare (CLLVM.llvmOverride_declare ov)

severity :: Diagnostic -> Severity
severity =
  \case
    FoundDeclare{} -> Debug
    RegisteredOverride{} -> Debug
