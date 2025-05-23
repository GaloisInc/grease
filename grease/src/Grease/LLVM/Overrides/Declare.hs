{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Grease.LLVM.Overrides.Declare
  ( mkDeclare
  , typedOverrideToSomeLLVMOverride
  ) where

import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some qualified as Some
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text (Text)
import Data.Text qualified as Text
import Grease.Panic (panic)
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Types qualified as C
import Text.LLVM.AST qualified as L

-- | Lift a Crucible type to an LLVM type.
--
-- This function has several missing cases that can be filled in as necessary.
llvmType :: Mem.HasPtrWidth w => C.TypeRepr t -> Maybe L.Type
llvmType =
  \case
    C.AnyRepr {} -> Nothing
    C.BoolRepr -> Just (L.PrimType (L.Integer 1))
    C.CharRepr {} -> Nothing
    C.BVRepr w -> Just (intType w)
    C.ComplexRealRepr {} -> Nothing
    C.FloatRepr {} -> Nothing  -- TODO?
    C.FunctionHandleRepr {} -> Nothing
    C.IEEEFloatRepr {} -> Nothing  -- TODO?
    C.IntegerRepr {} -> Nothing
    C.MaybeRepr {} -> Nothing
    C.NatRepr {} -> Nothing
    C.RealValRepr {} -> Nothing
    C.RecursiveRepr {} -> Nothing
    C.ReferenceRepr {} -> Nothing
    C.SequenceRepr {} -> Nothing
    C.StringRepr {} -> Nothing
    C.StringMapRepr {} -> Nothing
    C.StructRepr fieldAssn -> do
      fieldTys <-
        traverse (Some.viewSome llvmType) $
        TFC.toListFC Some.Some fieldAssn
      Just $ L.Struct fieldTys
    C.SymbolicArrayRepr {} -> Nothing
    C.SymbolicStructRepr {} -> Nothing
    C.UnitRepr -> Just (L.PrimType L.Void)
    C.VariantRepr {} -> Nothing
    C.VectorRepr {} -> Nothing
    C.WordMapRepr {} -> Nothing

    Mem.LLVMPointerRepr w ->
      case C.testEquality w ?ptrWidth of
        Just C.Refl -> Just L.PtrOpaque
        Nothing -> Just (intType w)
    C.IntrinsicRepr {} -> Nothing
  where
    -- TODO(lb): Avoid 'fromIntegral', handle overflow gracefully
    intType :: NatRepr.NatRepr n -> L.Type
    intType w = L.PrimType (L.Integer (fromIntegral (NatRepr.natValue w)))

-- | Create an LLVM declaration from Crucible types.
--
-- See https://github.com/GaloisInc/crucible/issues/1138 for progress on
-- obviating this code.
mkDeclare ::
  Mem.HasPtrWidth w =>
  String ->
  Ctx.Assignment C.TypeRepr args ->
  C.TypeRepr ret ->
  Either Text L.Declare
mkDeclare name args ret = do
  let getType :: forall t. C.TypeRepr t -> Either Text L.Type
      getType t =
        case llvmType t of
          Nothing -> Left ("Can't make LLVM type from Crucible type " <> tshow t)
          Just llTy -> Right llTy
  llvmArgs <- sequence (TFC.toListFC getType args)
  llvmRet <- getType ret
  pure $
    L.Declare
    { L.decArgs = llvmArgs
    , L.decAttrs = []
    , L.decComdat = Nothing
    , L.decLinkage = Nothing
    , L.decName = L.Symbol name
    , L.decRetType = llvmRet
    , L.decVarArgs = False
    , L.decVisibility = Nothing
    }

-- | Convert a 'C.TypedOverride' into a 'CLLVM.SomeLLVMOverride'.
--
-- Panics if 'mkDeclare' fails to make an LLVM declaration.
typedOverrideToSomeLLVMOverride ::
  Mem.HasPtrWidth w =>
  -- | Override name, only used in error messages
  String ->
  C.TypedOverride p sym ext args ret ->
  CLLVM.SomeLLVMOverride p sym ext
typedOverrideToSomeLLVMOverride nm ov =
  let argTys = C.typedOverrideArgs ov in
  let retTy = C.typedOverrideRet ov in
  case mkDeclare nm argTys retTy of
    Right decl ->
      CLLVM.SomeLLVMOverride $
        CLLVM.LLVMOverride
          { CLLVM.llvmOverride_declare = decl
          , CLLVM.llvmOverride_args = argTys
          , CLLVM.llvmOverride_ret = retTy
          , CLLVM.llvmOverride_def =
              \_mvar args -> C.typedOverrideHandler ov (TFC.fmapFC (C.RV . C.regValue) args)
          }
    Left err ->
      panic ("Bad override for `" <> nm <> "`: " <> Text.unpack err) []
