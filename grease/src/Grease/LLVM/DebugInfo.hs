{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Deduce initial argument shapes from types contained in LLVM debug info.
--
-- c.f. "Grease.Macaw.Dwarf".
module Grease.LLVM.DebugInfo (
  diArgShapes,
) where

import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Semigroup (Min (Min), Semigroup (sconcat))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.Panic (panic)
import Grease.Shape (ExtShape, Shape)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrShape (ShapePtrBV))
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.LLVM.Bytes (Bytes)
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Types as C
import Text.LLVM.AST as L
import Text.LLVM.DebugUtils qualified as LDU
import What4.FunctionName qualified as W4

-- | A view of a 'LDU.Info'
data DITypeView
  = Bv
  | Ptr !(PtrShape.PtrTarget 64 NoTag)

-- This function assumes x86_64
decodeBasicTypeName :: String -> Maybe DITypeView
decodeBasicTypeName =
  \case
    "unsigned char" -> Just Bv
    "unsigned short" -> Just Bv
    "unsigned int" -> Just Bv
    "unsigned long" -> Just Bv
    "unsigned long long" -> Just Bv
    "char" -> Just Bv
    "short" -> Just Bv
    "int" -> Just Bv
    "long" -> Just Bv
    "long long" -> Just Bv
    _ -> Nothing

-- It is acceptable for this function to under-estimate the size of a type:
-- GREASE's heuristics will expand the allocation as necessary.
minTypeSize :: Mem.HasPtrWidth w => LDU.Info -> Bytes
minTypeSize =
  \case
    LDU.ArrInfo _elemTy -> 0 -- arrays can be empty
    LDU.BaseType _nm dibt -> Bytes.bitsToBytes (L.dibtSize dibt)
    LDU.Pointer _ty -> Bytes.bitsToBytes (natValue ?ptrWidth)
    LDU.Structure _nm fields -> structSize fields
    LDU.Typedef _nm ty -> minTypeSize ty
    LDU.Union _nm fields -> unionSize fields
    LDU.Unknown -> 0
 where
  structSize fields =
    case List.reverse fields of
      [] -> 0
      (lastField : _) ->
        let lastOffset = Bytes.bitsToBytes (LDU.sfiOffset lastField)
         in lastOffset + minTypeSize (LDU.sfiInfo lastField)

  unionSize fields =
    let fieldSizes = List.map (minTypeSize . LDU.ufiInfo) fields
        getMin :: NE.NonEmpty Bytes -> Bytes
        getMin = coerce (sconcat @(Min Bytes))
     in case NE.nonEmpty fieldSizes of
          Nothing -> 0
          Just ne -> getMin ne

-- | Compute a suitable 'PtrShape.PtrTarget' corresponding to a type from DWARF
-- ('LDU.Info') that appears behind a pointer.
--
-- It is acceptable for this function to under-estimate the size of a type:
-- GREASE's heuristics will expand the allocation as necessary.
asPtrTarget :: Mem.HasPtrWidth 64 => LDU.Info -> PtrShape.PtrTarget 64 NoTag
asPtrTarget =
  \case
    LDU.ArrInfo _elemTy -> PtrShape.PtrTarget Nothing Seq.empty
    t@(LDU.BaseType _nm _dibt) -> uninit (minTypeSize t)
    LDU.Pointer{} -> PtrShape.PtrTarget Nothing (Seq.singleton (PtrShape.Pointer NoTag (PtrShape.Offset 0) (PtrShape.PtrTarget Nothing Seq.empty)))
    LDU.Structure _nm fields -> structPtrTarget fields
    LDU.Typedef _nm ty -> asPtrTarget ty
    t@(LDU.Union _nm _fields) -> uninit (minTypeSize t)
    LDU.Unknown -> uninit 0
 where
  uninit sz = PtrShape.PtrTarget Nothing (Seq.singleton (PtrShape.Uninitialized sz))

  -- For structs, we need to add uninitialized padding bytes between their
  -- fields.
  structPtrTarget :: [LDU.StructFieldInfo] -> PtrShape.PtrTarget 64 NoTag
  structPtrTarget fields =
    let ?ptrWidth = knownNat @64
     in let tgt = PtrShape.ptrTarget Nothing (fieldsPtrTarget fields)
            !tgtSize = PtrShape.ptrTargetSize (knownNat @64) tgt
            !minSize = minTypeSize (LDU.Structure Nothing fields)
         in if tgtSize >= minSize
              then tgt
              else
                panic
                  "Size mismatch in struct shape calculation"
                  [ "Expected size at least " ++ show minSize
                  , "Found size " ++ show tgtSize
                  ]

  fieldsPtrTarget :: [LDU.StructFieldInfo] -> Seq (PtrShape.MemShape 64 NoTag)
  fieldsPtrTarget =
    let ?ptrWidth = knownNat @64
     in snd . Foldable.foldl' go (0, Seq.empty)
   where
    go (bytesWritten, memShapes) sfi =
      let
        -- Compute the number of padding bytes needed and the shape of the
        -- field, add them both to the sequence of shapes for the struct.
        !bitsWritten = fromIntegral (Bytes.bytesToBits bytesWritten)
        !missingBits = LDU.sfiOffset sfi - bitsWritten
        !missingBytes = Bytes.bitsToBytes missingBits
        !fieldPtrTarget = asPtrTarget (LDU.sfiInfo sfi)
        !fieldSize = PtrShape.ptrTargetSize (knownNat @64) fieldPtrTarget
        !bytesWritten' = bytesWritten + missingBytes + fieldSize
        memShapes' =
          mconcat
            [ memShapes
            , if missingBits > 0
                then Seq.singleton (PtrShape.Uninitialized missingBytes)
                else Seq.empty
            , PtrShape.ptrTargetShapes fieldPtrTarget
            ]
        tgtSz = PtrShape.ptrTargetSize (knownNat @64) (PtrShape.PtrTarget Nothing memShapes')
       in
        if bytesWritten' == tgtSz
          then (bytesWritten', memShapes')
          else
            panic
              "Size mismatch in struct shape calculation"
              [ "Bytes written before this field: " ++ show bytesWritten
              , "Padding bytes: " ++ show missingBytes
              , "Field size: " ++ show fieldSize
              , "Total bytes written including this field: " ++ show bytesWritten'
              , "Size: " ++ show tgtSz
              ]

-- Only supports simple pointer- and integer-like types at the moment.
decodeType :: Mem.HasPtrWidth 64 => LDU.Info -> Maybe DITypeView
decodeType =
  \case
    LDU.ArrInfo _elemTy -> Nothing
    LDU.BaseType nm _dibt -> decodeBasicTypeName nm
    LDU.Pointer ty -> Just (Ptr (asPtrTarget ty))
    LDU.Structure{} -> Nothing
    LDU.Typedef _nm ty -> decodeType ty
    LDU.Union{} -> Nothing
    LDU.Unknown -> Nothing

-- | Infer the 'Shape' of a single argument from debug information
diArgShape ::
  ( ExtShape ext ~ PtrShape ext 64
  , Mem.HasPtrWidth 64
  ) =>
  Seq (Maybe DITypeView) ->
  Int ->
  C.TypeRepr t ->
  IO (Shape ext NoTag t)
diArgShape tyViews i t = do
  let fallback = Shape.minimalShapeWithPtrs (pure . const NoTag) t
  case tyViews Seq.!? i of
    Just (Just tyView) ->
      case (tyView, t) of
        (Bv, Mem.LLVMPointerRepr w) -> pure (Shape.ShapeExt (ShapePtrBV NoTag w))
        (Ptr tgt, Mem.LLVMPointerRepr w)
          | Just Refl <- testEquality w ?ptrWidth ->
              pure (Shape.ShapeExt (PtrShape.ShapePtr NoTag (PtrShape.Offset 0) tgt))
        _ -> fallback
    _ -> fallback

-- | Deduce initial argument shapes from types contained in LLVM debug info.
diArgShapes ::
  ( ExtShape ext ~ PtrShape ext 64
  , Mem.HasPtrWidth 64
  ) =>
  W4.FunctionName ->
  Ctx.Assignment C.TypeRepr args ->
  L.Module ->
  IO (Ctx.Assignment (Shape ext NoTag) args)
diArgShapes fnName argTys llvmMod = do
  let defaults = TFC.traverseFC (Shape.minimalShapeWithPtrs (pure . const NoTag)) argTys
  let fnSymb = L.Symbol (Text.unpack (W4.functionName fnName))
  case LDU.computeFunctionTypes llvmMod fnSymb of
    Just (_retTy : argTyInfos) -> do
      let argTyViews = Seq.fromList (List.map (decodeType =<<) argTyInfos)
      Ctx.traverseWithIndex (\idx -> diArgShape argTyViews (Ctx.indexVal idx)) argTys
    _ -> defaults
