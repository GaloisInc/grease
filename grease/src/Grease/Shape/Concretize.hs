{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Grease.Shape.Concretize
  ( concShape
  ) where

import Data.BitVector.Sized as BV
import Data.List qualified as List
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Grease.Shape (Shape, ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem

-- | Turns 'PtrShape.Initialized' into 'PtrShape.Exactly', calls 'concPtrTarget'.
concMemShape ::
  PtrShape.MemShape wptr (Conc.ConcRV' sym) ->
  PtrShape.MemShape wptr (Conc.ConcRV' sym)
concMemShape =
  \case
    s@PtrShape.Uninitialized {} -> s
    PtrShape.Initialized (Conc.ConcRV' tag) _bs ->
      let toWord8 :: BV.BV 8 -> Word8
          toWord8 = fromIntegral . BV.asUnsigned in
      let concByte b = PtrShape.TaggedByte b (toWord8 (Mem.concOffset (Conc.unConcRV' b))) in
      PtrShape.Exactly (List.map concByte (Vec.toList tag))
    PtrShape.Pointer tag off tgt -> PtrShape.Pointer tag off (concPtrTarget tgt)
    PtrShape.Exactly bs -> PtrShape.Exactly bs

concPtrTarget ::
  PtrShape.PtrTarget wptr (Conc.ConcRV' sym) ->
  PtrShape.PtrTarget wptr (Conc.ConcRV' sym)
concPtrTarget (PtrShape.PtrTarget s) = PtrShape.PtrTarget (fmap concMemShape s)

-- | Turns 'PtrShape.ShapePtrBV' into 'PtrShape.ShapePtrBVLit', calls 'concPtrTarget'.
concPtrShape ::
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  PtrShape.PtrShape ext wptr (Conc.ConcRV' sym) t ->
  PtrShape.PtrShape ext wptr (Conc.ConcRV' sym) t
concPtrShape =
  \case
    PtrShape.ShapePtrBV tag w ->
      PtrShape.ShapePtrBVLit tag w (Mem.concOffset (Conc.unConcRV' tag))
    s@PtrShape.ShapePtrBVLit {} -> s
    PtrShape.ShapePtr tag off tgt ->
      let ptr = Conc.unConcRV' tag in
      if Mem.concBlock ptr == 0
      then PtrShape.ShapePtrBVLit tag (Mem.concWidth ptr) (Mem.concOffset ptr)
      else PtrShape.ShapePtr tag off (concPtrTarget tgt)

concShape ::
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  Shape ext (Conc.ConcRV' sym) t ->
  Shape ext (Conc.ConcRV' sym) t
concShape =
  \case
    s@Shape.ShapeBool {} -> s
    s@Shape.ShapeStruct {} -> s
    s@Shape.ShapeUnit {} -> s
    Shape.ShapeExt ext -> Shape.ShapeExt (concPtrShape ext)
