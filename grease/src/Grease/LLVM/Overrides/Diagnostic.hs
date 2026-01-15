{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides.Diagnostic (
  Diagnostic (..),
  ppType,
  severity,
) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Parameterized.TraversableFC qualified as TFC
import Grease.Diagnostic.Severity (Severity (Debug, Warn))
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.Intrinsics.Declare qualified as Decl
import Lang.Crucible.LLVM.MemModel (ppLLVMIntrinsicTypes)
import Lang.Crucible.Types (TypeRepr, ppIntrinsicDefault, ppTypeRepr)
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L

data Diagnostic where
  CantSkip ::
    Decl.Declare args ret ->
    Diagnostic
  FoundDeclare ::
    Decl.Declare args ret ->
    Diagnostic
  RegisteredOverride ::
    forall p sym.
    CLI.SomeLLVMOverride p sym CLI.LLVM ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      CantSkip decl -> "Can't skip function" PP.<+> ppDecl decl
      FoundDeclare decl -> "Found `declare`:" PP.<+> ppDecl decl
      RegisteredOverride sov@(CLI.SomeLLVMOverride _ov) ->
        case CLI.someLlvmOverrideDeclare sov of
          Decl.SomeDeclare decl ->
            "Registered override for:" PP.<+> ppDecl decl

ppType :: TypeRepr tp -> PP.Doc ann
ppType = runIdentity . ppTypeRepr (ppLLVMIntrinsicTypes (\s ctx -> Identity (ppIntrinsicDefault s ctx)))

ppFuncType :: [PP.Doc x] -> PP.Doc x
ppFuncType = PP.hsep . zipWith (PP.<+>) ("::" : repeat "->")

ppDecl :: Decl.Declare args ret -> PP.Doc ann
ppDecl decl =
  let L.Symbol name = Decl.decName decl
   in PP.hsep
        [ PP.pretty name
        , ppFuncType $
            TFC.toListFC ppType (Decl.decArgs decl) ++ [ppType (Decl.decRet decl)]
        ]

severity :: Diagnostic -> Severity
severity =
  \case
    CantSkip{} -> Warn
    FoundDeclare{} -> Debug
    RegisteredOverride{} -> Debug
