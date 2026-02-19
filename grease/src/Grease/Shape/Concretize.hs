{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Shape.Concretize (
  concShape,
  concPtrTarget,
) where

import Data.BitVector.Sized qualified as BV
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Parameterized.TraversableFC (fmapFC)
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Grease.Panic (panic)
import Grease.Shape (ExtShape, Shape)
import Grease.Shape qualified as Shape
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.MemModel.Pointer qualified as CLMP
import Numeric.Natural (Natural)

-- | Turns 'PtrShape.Initialized' into 'PtrShape.Exactly'.
-- Transforms 'NoData pointers into 'Precond pointers using allocMap.
concMemShape ::
  CLMP.HasPtrWidth wptr =>
  Map Natural (PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.NoData) ->
  PtrShape.MemShape wptr (Conc.ConcRV' sym) 'PtrShape.NoData ->
  PtrShape.MemShape wptr (Conc.ConcRV' sym) 'PtrShape.Precond
concMemShape allocMap =
  \case
    PtrShape.Uninitialized bytes -> PtrShape.Uninitialized bytes
    PtrShape.Initialized (Conc.ConcRV' tag) _bs ->
      let toWord8 :: BV.BV 8 -> Word8
          toWord8 = fromIntegral . BV.asUnsigned
       in let concByte b = PtrShape.TaggedByte b (toWord8 (CLMP.concOffset (Conc.unConcRV' b)))
           in PtrShape.Exactly (List.map concByte (Vec.toList tag))
    PtrShape.Pointer tag PtrShape.NoPtrData ->
      let ptr = Conc.unConcRV' tag
          blockNum = CLMP.concBlock ptr
       in case Map.lookup (fromInteger blockNum) allocMap of
            Just target ->
              let concTarget = concPtrTarget allocMap target
                  offsetBV = CLMP.concOffset ptr
                  offsetBytes = BV.asUnsigned offsetBV
                  offset = PtrShape.Offset (CLB.toBytes offsetBytes)
               in PtrShape.Pointer tag (PtrShape.PrecondPtrData offset concTarget)
            Nothing -> panic "concMemShape" ["Block " ++ show blockNum ++ " not found in allocMap during concretization (concMemShape)"]
    PtrShape.Exactly bs -> PtrShape.Exactly bs

concPtrTarget ::
  CLMP.HasPtrWidth wptr =>
  Map Natural (PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.NoData) ->
  PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.NoData ->
  PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.Precond
concPtrTarget allocMap (PtrShape.PtrTarget bid s) =
  PtrShape.PtrTarget bid (fmap (concMemShape allocMap) s)

-- | Turns 'PtrShape.ShapePtrBV' into 'PtrShape.ShapePtrBVLit'.
-- Transforms 'NoData pointers into 'Precond pointers using allocMap.
concPtrShape ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext (Conc.ConcRV' sym) 'PtrShape.NoData ~ PtrShape ext wptr (Conc.ConcRV' sym) 'PtrShape.NoData) =>
  Map Natural (PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.NoData) ->
  PtrShape.PtrShape ext wptr (Conc.ConcRV' sym) 'PtrShape.NoData t ->
  PtrShape.PtrShape ext wptr (Conc.ConcRV' sym) 'PtrShape.Precond t
concPtrShape allocMap =
  \case
    PtrShape.ShapePtrBV tag w ->
      PtrShape.ShapePtrBVLit tag w (CLMP.concOffset (Conc.unConcRV' tag))
    PtrShape.ShapePtrBVLit tag w bv -> PtrShape.ShapePtrBVLit tag w bv
    PtrShape.ShapePtr tag PtrShape.NoPtrData ->
      let ptr = Conc.unConcRV' tag
          blockNum = CLMP.concBlock ptr
       in if blockNum == 0
            then PtrShape.ShapePtrBVLit tag (CLMP.concWidth ptr) (CLMP.concOffset ptr)
            else case Map.lookup (fromInteger blockNum) allocMap of
              Just target ->
                let concTarget = concPtrTarget allocMap target
                    offsetBV = CLMP.concOffset ptr
                    offsetBytes = BV.asUnsigned offsetBV
                    offset = PtrShape.Offset (CLB.toBytes offsetBytes)
                 in PtrShape.ShapePtr tag (PtrShape.PrecondPtrData offset concTarget)
              Nothing -> panic "concPtrTarget" $ ["Block " ++ show blockNum ++ " not found in allocMap during concretization (concPtrShape)"]

concShape ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext (Conc.ConcRV' sym) 'PtrShape.NoData ~ PtrShape ext wptr (Conc.ConcRV' sym) 'PtrShape.NoData) =>
  (ExtShape ext (Conc.ConcRV' sym) 'PtrShape.Precond ~ PtrShape ext wptr (Conc.ConcRV' sym) 'PtrShape.Precond) =>
  Map Natural (PtrShape.PtrTarget wptr (Conc.ConcRV' sym) 'PtrShape.NoData) ->
  Shape ext (Conc.ConcRV' sym) 'PtrShape.NoData t ->
  Shape ext (Conc.ConcRV' sym) 'PtrShape.Precond t
concShape allocMap =
  \case
    Shape.ShapeBool tag -> Shape.ShapeBool tag
    Shape.ShapeFloat tag fi -> Shape.ShapeFloat tag fi
    Shape.ShapeStruct tag fields -> Shape.ShapeStruct tag (fmapFC (concShape allocMap) fields)
    Shape.ShapeUnit tag -> Shape.ShapeUnit tag
    Shape.ShapeExt ext -> Shape.ShapeExt (concPtrShape allocMap ext)
