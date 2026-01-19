{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.SymbolRepr qualified as Symb
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Diagnostic.Severity (Severity (Debug, Warn))
import Grease.Panic (panic)
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.Intrinsics.Declare qualified as Decl
import Lang.Crucible.Types (TypeRepr (BVRepr), ppTypeRepr)
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
    CLLVM.SomeLLVMOverride p sym CLLVM.LLVM ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      CantSkip decl -> "Can't skip function" PP.<+> ppDecl decl
      FoundDeclare decl -> "Found `declare`:" PP.<+> ppDecl decl
      RegisteredOverride (CLLVM.SomeLLVMOverride ov) ->
        let decl = CLLVM.llvmOverrideDeclare ov
         in "Registered override for:" PP.<+> ppDecl decl

-- TODO: Upstream ppIntrinsic to Crucible-LLVM
ppType :: TypeRepr tp -> PP.Doc ann
ppType = runIdentity . ppTypeRepr ppIntrinsic
 where
  ppIntrinsic :: Symb.SymbolRepr symb -> Ctx.Assignment TypeRepr ctx -> Identity (PP.Doc ann)
  ppIntrinsic symbRepr tyCtx =
    Identity $
      case testEquality symbRepr (Symb.knownSymbol @"LLVM_pointer") of
        Nothing ->
          PP.pretty ("<unknown type: " <> Symb.symbolRepr symbRepr <> ">")
        Just Refl ->
          case Ctx.viewAssign tyCtx of
            Ctx.AssignExtend (Ctx.viewAssign -> Ctx.AssignEmpty) (BVRepr w) ->
              "(Ptr" PP.<+> PP.viaShow w <> ")"
            -- These are impossible by the definition of LLVMPointerImpl
            Ctx.AssignEmpty ->
              panic "ppType" ["Impossible: LLVMPointerType empty context"]
            Ctx.AssignExtend _ _ ->
              panic "ppType" ["Impossible: LLVMPointerType ill-formed context"]

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
