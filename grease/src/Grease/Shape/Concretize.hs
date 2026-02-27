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

-- | Look up a block in the allocation map and create PrecondPtrData from a concrete pointer
--
-- This helper eliminates duplication between concMemShape and concPtrShape by
-- encapsulating the logic of: looking up the block, concretizing the target,
-- extracting the offset, and constructing PrecondPtrData.
lookupBlockAndCreatePtrData ::
  CLMP.HasPtrWidth wptr =>
  String ->
  Map Natural (PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym)) ->
  Conc.ConcRegValue sym (CLMP.LLVMPointerType wptr) ->
  PtrShape.PtrData 'PtrShape.Precond wptr (Conc.ConcRV' sym)
lookupBlockAndCreatePtrData callerName allocMap ptr =
  let blockNum = CLMP.concBlock ptr
      target = case Map.lookup (fromInteger blockNum) allocMap of
        Just t -> t
        Nothing -> panic callerName ["Block " ++ show blockNum ++ " not found in allocMap"]
      concTarget = concPtrTarget allocMap target
      offsetBV = CLMP.concOffset ptr
      offsetBytes = BV.asUnsigned offsetBV
      offset = PtrShape.Offset (CLB.toBytes offsetBytes)
   in PtrShape.PrecondPtrData offset concTarget

-- | Turns 'PtrShape.Initialized' into 'PtrShape.Exactly'.
-- Transforms 'NoData pointers into 'Precond pointers using allocMap.
concMemShape ::
  CLMP.HasPtrWidth wptr =>
  Map Natural (PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym)) ->
  PtrShape.MemShape wptr 'PtrShape.NoData (Conc.ConcRV' sym) ->
  PtrShape.MemShape wptr 'PtrShape.Precond (Conc.ConcRV' sym)
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
          ptrData = lookupBlockAndCreatePtrData "concMemShape" allocMap ptr
       in PtrShape.Pointer tag ptrData
    PtrShape.Exactly bs -> PtrShape.Exactly bs

concPtrTarget ::
  CLMP.HasPtrWidth wptr =>
  Map Natural (PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym)) ->
  PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym) ->
  PtrShape.PtrTarget wptr 'PtrShape.Precond (Conc.ConcRV' sym)
concPtrTarget allocMap (PtrShape.PtrTarget bid s) =
  PtrShape.PtrTarget bid (fmap (concMemShape allocMap) s)

-- | Turns 'PtrShape.ShapePtrBV' into 'PtrShape.ShapePtrBVLit'.
-- Transforms 'NoData pointers into 'Precond pointers using allocMap.
concPtrShape ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext 'PtrShape.NoData (Conc.ConcRV' sym) ~ PtrShape ext wptr 'PtrShape.NoData (Conc.ConcRV' sym)) =>
  Map Natural (PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym)) ->
  PtrShape.PtrShape ext wptr 'PtrShape.NoData (Conc.ConcRV' sym) t ->
  PtrShape.PtrShape ext wptr 'PtrShape.Precond (Conc.ConcRV' sym) t
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
            else
              let ptrData = lookupBlockAndCreatePtrData "concPtrShape" allocMap ptr
               in PtrShape.ShapePtr tag ptrData

concShape ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext 'PtrShape.NoData (Conc.ConcRV' sym) ~ PtrShape ext wptr 'PtrShape.NoData (Conc.ConcRV' sym)) =>
  (ExtShape ext 'PtrShape.Precond (Conc.ConcRV' sym) ~ PtrShape ext wptr 'PtrShape.Precond (Conc.ConcRV' sym)) =>
  Map Natural (PtrShape.PtrTarget wptr 'PtrShape.NoData (Conc.ConcRV' sym)) ->
  Shape ext 'PtrShape.NoData (Conc.ConcRV' sym) t ->
  Shape ext 'PtrShape.Precond (Conc.ConcRV' sym) t
concShape allocMap =
  \case
    Shape.ShapeBool tag -> Shape.ShapeBool tag
    Shape.ShapeFloat tag fi -> Shape.ShapeFloat tag fi
    Shape.ShapeStruct tag fields -> Shape.ShapeStruct tag (fmapFC (concShape allocMap) fields)
    Shape.ShapeUnit tag -> Shape.ShapeUnit tag
    Shape.ShapeExt ext -> Shape.ShapeExt (concPtrShape allocMap ext)
