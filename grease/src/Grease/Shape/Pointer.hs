{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Shape.Pointer (
  TaggedByte (..),
  traverseTaggedByte,
  MemShape (..),
  BlockId (..),
  traverseMemShapeWithType,
  memShapeSize,
  PtrTarget (..),
  ptrTarget,
  traversePtrTargetWithType,
  ptrTargetSize,
  Offset (..),
  PtrShape (..),
  traversePtrShapeWithType,
  ptrShapeType,
  getPtrTag,
  setPtrTag,
  ptrShapeTag,
  minimalPtrShape,
  x64StackPtrShape,
  ppcStackPtrShape,
  armStackPtrShape,
  bytesToPointers,
  growPtrTargetBy,
  growPtrTargetUpTo,
  growPtrTarget,
  initializePtrTarget,
  initializeOrGrowPtrTarget,
  modifyPtrTarget,
) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.BitVector.Sized (BV)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.Classes (ShowF (..))
import Data.Parameterized.NatRepr (NatRepr, natValue)
import Data.Parameterized.TraversableF qualified as TF
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import Data.Word (Word8)
import GHC.TypeLits (type Natural)
import Grease.Cursor
import Grease.Cursor.Pointer (Dereference (..))
import Grease.Options (ExtraStackSlots (..))
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Utility
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Bytes (Bytes (..))
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Prettyprinter qualified as PP

newtype BlockId = BlockId {getBlockId :: Int}
  deriving (Eq, Ord, Show)

-- | A byte ('Word8') along with a @tag@ (see 'Grease.Shape.Shape').
data TaggedByte tag
  = TaggedByte
  { taggedByteTag :: tag (Mem.LLVMPointerType 8)
  , taggedByteValue :: Word8
  }

deriving instance Eq (tag (Mem.LLVMPointerType 8)) => Eq (TaggedByte tag)

-- | Helper, not exported
ppTag :: MC.PrettyF tag => tag x -> PP.Doc ann
ppTag tag = PP.hcat ["[", MC.prettyF tag, "]"]

instance MC.PrettyF tag => PP.Pretty (TaggedByte tag) where
  pretty tb = PP.hcat [PP.pretty (taggedByteValue tb), ppTag (taggedByteTag tb)]

instance TF.FunctorF TaggedByte where
  fmapF = TF.fmapFDefault

instance TF.FoldableF TaggedByte where
  foldMapF = TF.foldMapFDefault

-- | Specialization of 'traverseTaggedByte'
instance TF.TraversableF TaggedByte where
  traverseF f = traverseTaggedByte f

-- | Traverse the @tag@ of a 'TaggedByte'
traverseTaggedByte ::
  Applicative m =>
  (tag (Mem.LLVMPointerType 8) -> m (tag' (Mem.LLVMPointerType 8))) ->
  TaggedByte tag ->
  m (TaggedByte tag')
traverseTaggedByte f (TaggedByte tag val) = TaggedByte <$> f tag <*> pure val

-- | Part of the content of an allocation (see 'PtrTarget')
--
-- Type parameters:
--
-- * @wptr@: Width of a pointer, in bits
-- * @tag@: See 'Grease.Shape.Shape'
data MemShape wptr tag
  = -- | Some number of uninitialized bytes
    Uninitialized !Bytes
  | -- | Some number of symbolically-initialized bytes
    Initialized (tag (C.VectorType (Mem.LLVMPointerType 8))) !Bytes
  | -- | Several (generally 4 or 8) initialized bytes that form a pointer, plus
    -- an offset into that pointer
    Pointer (tag (Mem.LLVMPointerType wptr)) !Offset (PtrTarget wptr tag)
  | -- | Some concrete bytes
    Exactly [TaggedByte tag]

deriving instance
  ( Eq (tag (Mem.LLVMPointerType 8))
  , Eq (tag (C.VectorType (Mem.LLVMPointerType 8)))
  , Eq (tag (Mem.LLVMPointerType wptr))
  ) =>
  Eq (MemShape wptr tag)

instance MC.PrettyF tag => PP.Pretty (MemShape wptr tag) where
  pretty =
    \case
      Uninitialized bs -> "uninitialized x" PP.<+> PP.viaShow (Bytes.bytesToInteger bs)
      Initialized tag bs ->
        PP.hcat
          [ "initialized"
          , ppTag tag
          , "x "
          , PP.viaShow (Bytes.bytesToInteger bs)
          ]
      Pointer tag (Offset off) tgt ->
        PP.hcat
          [ "ptr"
          , ppTag tag
          , ":"
          , PP.pretty tgt
          , "+"
          , PP.viaShow off
          ]
      Exactly bs -> "exactly:" PP.<+> PP.pretty bs

instance TF.FunctorF (MemShape wptr) where
  fmapF = TF.fmapFDefault

instance TF.FoldableF (MemShape wptr) where
  foldMapF = TF.foldMapFDefault

instance TF.TraversableF (MemShape wptr) where
  traverseF f =
    \case
      Uninitialized bs -> pure (Uninitialized bs)
      Initialized tag bs -> Initialized <$> f tag <*> pure bs
      Pointer tag off tgt -> Pointer <$> f tag <*> pure off <*> TF.traverseF f tgt
      Exactly bs -> Exactly <$> traverse (TF.traverseF f) bs

-- | Like 'TF.traverseF', but with access to the appropriate 'C.TypeRepr'.
traverseMemShapeWithType ::
  Mem.HasPtrWidth wptr =>
  Applicative m =>
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  MemShape wptr tag ->
  m (MemShape wptr tag')
traverseMemShapeWithType f =
  \case
    Uninitialized bs -> pure (Uninitialized bs)
    Initialized tag bs ->
      Initialized
        <$> f (C.VectorRepr (Mem.LLVMPointerRepr C.knownNat)) tag
        <*> pure bs
    Pointer tag off tgt ->
      Pointer
        <$> f (Mem.LLVMPointerRepr ?ptrWidth) tag
        <*> pure off
        <*> traversePtrTargetWithType f tgt
    Exactly bs ->
      Exactly
        <$> traverse (traverseTaggedByte (f (Mem.LLVMPointerRepr (C.knownNat @8)))) bs

merge ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  MemShape wptr tag ->
  MemShape wptr tag ->
  Maybe (MemShape wptr tag)
merge s1 s2 =
  case (s1, s2) of
    (Uninitialized i, Uninitialized j) -> Just (Uninitialized (i + j))
    (Initialized tagi i, Initialized tagj j) -> Just (Initialized (tagi <> tagj) (i + j))
    (Exactly i, Exactly j) -> Just (Exactly (i List.++ j))
    _ -> Nothing

-- | Number of bytes required to store this 'MemShape'
memShapeSize ::
  Mem.HasPtrWidth w =>
  proxy w ->
  MemShape wptr tag ->
  Bytes
memShapeSize _proxy =
  \case
    Uninitialized bs -> bs
    Initialized _tag bs -> bs
    Pointer _ _ _ -> Bytes.bitsToBytes (C.widthVal ?ptrWidth)
    Exactly bs -> Bytes.Bytes (fromIntegral (List.length bs))

initializeMemShape ::
  tag (C.VectorType (Mem.LLVMPointerType 8)) ->
  MemShape wptr tag ->
  MemShape wptr tag
initializeMemShape tag =
  \case
    Uninitialized bs -> Initialized tag bs
    ms -> ms

{-
Note [Deduplicating Pointer Targets Based on BlockIDs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During parsing of shapes, a given block is parsed as many times as it is needed into a 'PtrTarget'.
This duplciation of blocks causes 'setup' to lose track of which 'PtrTarget's are identical and should be aliased.
Indeed, the shapes datastructure does not directly allow for 'PtrTarget's to be aliased. We create aliasing by annotating 'PtrTarget's
with an optional 'BlockId'.

'PtrTarget's with a 'BlockId' are deemed equivalent and during 'setup' each use of a given 'BlockId' will be deduplicated into a single runtime value.
'PtrTarget's without a 'BlockId' can be thought of as fresh on-the-fly 'BlockId's which is how the printer handles them. Inside of setup, pointer initialization is memoized
such that the same result is returned for matching 'BlockId's. This behavior means that for a given list of shapes passed to 'setup' any 'PtrTarget' with a matching 'BlockId' is expected
to have the same shape, otherwise the first observed 'PtrTarget's shape will win.
-}

-- | The target of a pointer.
--
-- An empty sequence indicates that the pointer is not yet known to point to
-- allocated space. Otherwise, the pointer points to an allocation large enough
-- to hold all the 'MemShape's in the 'Seq' (see 'ptrTargetSize').
--
-- The `BlockId` is used to deduplicate target blocks during setup. If a `BlockId` is present
-- a single block will be allocated for that identifier. If the identifier is `Nothing` then
-- a fresh identifier will be generated. Invariant: it is expected that any two 'PtrTarget's with the same 'BlockId'
-- used within the same context will have the same shape. Violating this invariant will mean that 'setup' will produce
-- incorrect blocks for the second time a 'BlockId' is used.
--
-- There are \"non-canonical\" instances of this type, e.g., those that involve
-- @'Uninitialized' 0@ or two 'Initialized' that are adjacent and could be
-- merged. This isn\'t really a problem for any of the code that consumes
-- this type, so it\'s not an invariant that all instances are canonical. The
-- 'ptrTarget' smart constructor makes canonical instances.
--
-- Type parameters:
--
-- * @wptr@: Width of a pointer, in bits
-- * @tag@: See 'Grease.Shape.Shape'
data PtrTarget wptr tag
  = PtrTarget
  { ptrTargetBlock :: Maybe BlockId
  , ptrTargetShapes :: Seq (MemShape wptr tag)
  }

instance TF.FunctorF (PtrTarget wptr) where
  fmapF = TF.fmapFDefault

instance TF.FoldableF (PtrTarget wptr) where
  foldMapF = TF.foldMapFDefault

instance TF.TraversableF (PtrTarget wptr) where
  traverseF f (PtrTarget bid tgt) = PtrTarget bid <$> traverse (TF.traverseF f) tgt

deriving instance
  ( Eq (tag (Mem.LLVMPointerType 8))
  , Eq (tag (C.VectorType (Mem.LLVMPointerType 8)))
  , Eq (tag (Mem.LLVMPointerType wptr))
  ) =>
  Eq (PtrTarget wptr tag)

-- | Like 'TF.traverseF', but with access to the appropriate 'C.TypeRepr'.
traversePtrTargetWithType ::
  Mem.HasPtrWidth wptr =>
  Applicative m =>
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  PtrTarget wptr tag ->
  m (PtrTarget wptr tag')
traversePtrTargetWithType f (PtrTarget bid tgt) =
  PtrTarget bid <$> traverse (traverseMemShapeWithType f) tgt

-- | Smart constructor that merges compatible adjacent shapes.
ptrTarget ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  Maybe BlockId ->
  Seq (MemShape wptr tag) ->
  PtrTarget wptr tag
ptrTarget bid = PtrTarget bid . Foldable.foldl' go Seq.empty
 where
  go s (Uninitialized 0) = s
  go s (Initialized _tag 0) = s
  go Seq.Empty memShape = Seq.singleton memShape
  go accum@(pfx Seq.:|> lastElem) memShape =
    case merge lastElem memShape of
      Just last' -> pfx Seq.:|> last'
      Nothing -> accum Seq.:|> memShape

-- | Compute the minimal size of an allocation that could contain this
-- 'PtrTarget'
ptrTargetSize ::
  Mem.HasPtrWidth w =>
  proxy w ->
  PtrTarget wptr tag ->
  Bytes
ptrTargetSize proxy (PtrTarget _ s) = Foldable.sum (fmap (memShapeSize proxy) s)

-- | Grow an allocation by adding some uninitialized bytes to the end
growPtrTargetBy ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  Bytes ->
  PtrTarget wptr tag ->
  PtrTarget wptr tag
growPtrTargetBy amount (PtrTarget bid s) = ptrTarget bid (s Seq.|> Uninitialized amount)

-- | Grow an allocation by adding uninitialized bytes to the end, up to the
-- given size (or at least by 1).
growPtrTargetUpTo ::
  Mem.HasPtrWidth w =>
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  -- | Add uninitialized bytes to the allocation until it becomes this size
  Bytes ->
  PtrTarget wptr tag ->
  PtrTarget wptr tag
growPtrTargetUpTo amount t =
  growPtrTargetBy (max 1 (amount - ptrTargetSize ?ptrWidth t)) t

-- | Grow an allocation by adding an uninitialized byte to the end
growPtrTarget ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  PtrTarget wptr tag ->
  PtrTarget wptr tag
growPtrTarget = growPtrTargetBy (Bytes.toBytes (1 :: Integer))

-- | Initialize all uninitialized parts of an allocation
initializePtrTarget ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  -- | Tag for newly-initialized bytes
  tag (C.VectorType (Mem.LLVMPointerType 8)) ->
  PtrTarget wptr tag ->
  PtrTarget wptr tag
initializePtrTarget tag (PtrTarget bid ms) =
  ptrTarget bid (fmap (initializeMemShape tag) ms)

-- | Initialize all uninitialized parts of an allocation, or if it is already
-- fully initialized, grow it by one uninitialized byte.
initializeOrGrowPtrTarget ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  -- | Tag for newly-initialized bytes
  tag (C.VectorType (Mem.LLVMPointerType 8)) ->
  PtrTarget wptr tag ->
  PtrTarget wptr tag
initializeOrGrowPtrTarget tag t@(PtrTarget _ ms) =
  if Foldable.all isInit ms
    then growPtrTarget t
    else initializePtrTarget tag t
 where
  isInit =
    \case
      Uninitialized{} -> False
      Initialized{} -> True
      Pointer{} -> True
      Exactly{} -> True

-- | Replace the targets with pointers
ptrTargetToPtrs ::
  Mem.HasPtrWidth wptr =>
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  proxy wptr ->
  tag (Mem.LLVMPointerType wptr) ->
  PtrTarget wptr tag ->
  PtrTarget wptr tag
ptrTargetToPtrs proxy tag tgt =
  let sz = ptrTargetSize proxy tgt
      ptrBytes = Bytes.bitsToBytes (C.widthVal ?ptrWidth)
      (nPtrs, remBytes) = sz `divMod` ptrBytes
      nPtrs' = Bytes.bytesToInteger $ if remBytes == 0 then nPtrs else nPtrs + 1
      genSeq n x = Seq.iterateN n id x
   in ( PtrTarget Nothing $
          genSeq (fromIntegral nPtrs') $
            Pointer tag (Offset 0) $
              ptrTarget Nothing Seq.Empty
      )

instance PP.Pretty BlockId where
  pretty bid = "blockid:" PP.<+> (PP.pretty $ getBlockId bid)

instance MC.PrettyF tag => PP.Pretty (PtrTarget wptr tag) where
  pretty :: MC.PrettyF tag => PtrTarget wptr tag -> PP.Doc ann
  pretty =
    \case
      PtrTarget bid Seq.Empty -> PP.pretty bid PP.<+> "<unallocated>"
      PtrTarget bid ms -> PP.pretty bid PP.<+> PP.list (Foldable.toList (fmap PP.pretty ms))

newtype Offset = Offset {getOffset :: Bytes}
  deriving (Eq, Show)

-- Type parameters:
--

-- * @ext@: Crucible syntax extension

-- * @w@: Width of a pointer, in bits

-- * @tag@: See 'Grease.Shape.Shape'

-- * @t@: Crucible type corresponding to this shape
type PtrShape :: Type -> Natural -> (C.CrucibleType -> Type) -> C.CrucibleType -> Type
data PtrShape (ext :: Type) w tag (t :: C.CrucibleType) where
  ShapePtrBV ::
    1 C.<= w' =>
    tag (Mem.LLVMPointerType w') ->
    NatRepr w' ->
    PtrShape ext w tag (Mem.LLVMPointerType w')
  ShapePtrBVLit ::
    1 C.<= w' =>
    tag (Mem.LLVMPointerType w') ->
    NatRepr w' ->
    BV w' ->
    PtrShape ext w tag (Mem.LLVMPointerType w')
  ShapePtr ::
    tag (Mem.LLVMPointerType w) ->
    Offset ->
    PtrTarget w tag ->
    PtrShape ext w tag (Mem.LLVMPointerType w)

-- | Returns @'Just' 'Refl'@ iff the shapes are identical.
--
-- Disobeys the guidance
--
-- > Typically, only singleton types should inhabit this class.
--
-- on 'TestEquality', because 'PtrShape' is not a singleton.
instance
  ( Eq (tag (Mem.LLVMPointerType 8))
  , Eq (tag (Mem.LLVMPointerType w))
  , Eq (tag (C.VectorType (Mem.LLVMPointerType 8)))
  , TestEquality tag
  ) =>
  TestEquality (PtrShape ext w tag)
  where
  testEquality s s' =
    case (s, s') of
      (ShapePtrBV tag w, ShapePtrBV tag' w') ->
        case (testEquality tag tag', testEquality w w') of
          (Just Refl, Just Refl) -> Just Refl
          _ -> Nothing
      (ShapePtrBVLit tag w bv, ShapePtrBVLit tag' w' bv') ->
        case (testEquality tag tag', testEquality w w') of
          (Just Refl, Just Refl) | bv == bv' -> Just Refl
          _ -> Nothing
      (ShapePtr tag offset target, ShapePtr tag' offset' target') ->
        case (testEquality tag tag', offset == offset', target == target') of
          (Just Refl, True, True) -> Just Refl
          _ -> Nothing
      _ -> Nothing

instance MC.PrettyF tag => Show (PtrShape ext w tag t) where
  show = show . MC.prettyF
instance MC.PrettyF tag => ShowF (PtrShape ext w tag)
instance MC.PrettyF tag => MC.PrettyF (PtrShape ext w tag) where
  prettyF =
    \case
      ShapePtrBV tag _w -> "bv" PP.<> ppTag tag
      ShapePtrBVLit tag w bv ->
        PP.hcat
          [ PP.viaShow bv
          , ":[bv"
          , PP.viaShow w
          , "]"
          , ppTag tag
          ]
      ShapePtr tag (Offset off) tgt ->
        PP.pretty tgt PP.<> "+" PP.<> PP.viaShow off PP.<> ppTag tag

instance TFC.FunctorFC (PtrShape ext w) where
  fmapFC = TFC.fmapFCDefault

instance TFC.FoldableFC (PtrShape ext w) where
  foldMapFC = TFC.foldMapFCDefault

instance TFC.TraversableFC (PtrShape ext w) where
  traverseFC f =
    \case
      ShapePtrBV tag w -> ShapePtrBV <$> f tag <*> pure w
      ShapePtrBVLit tag w bv -> ShapePtrBVLit <$> f tag <*> pure w <*> pure bv
      ShapePtr tag off tgt ->
        ShapePtr <$> f tag <*> pure off <*> TF.traverseF f tgt

ptrShapeType :: Mem.HasPtrWidth w => PtrShape ext w tag t -> C.TypeRepr t
ptrShapeType =
  \case
    ShapePtrBV _tag w -> Mem.LLVMPointerRepr w
    ShapePtrBVLit _tag w _ -> Mem.LLVMPointerRepr w
    ShapePtr _tag _ _ -> Mem.LLVMPointerRepr ?ptrWidth

-- | Like 'TF.traverseFC', but with access to the appropriate 'C.TypeRepr'.
traversePtrShapeWithType ::
  Mem.HasPtrWidth wptr =>
  Applicative m =>
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  PtrShape ext wptr tag t ->
  m (PtrShape ext wptr tag' t)
traversePtrShapeWithType f =
  \case
    ShapePtrBV tag w ->
      ShapePtrBV <$> f (Mem.LLVMPointerRepr w) tag <*> pure w
    ShapePtrBVLit tag w bv ->
      ShapePtrBVLit <$> f (Mem.LLVMPointerRepr w) tag <*> pure w <*> pure bv
    ShapePtr tag off tgt ->
      ShapePtr
        <$> f (Mem.LLVMPointerRepr ?ptrWidth) tag
        <*> pure off
        <*> traversePtrTargetWithType f tgt

-- | Get the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
getPtrTag :: PtrShape ext w tag t -> tag t
getPtrTag =
  \case
    ShapePtrBV tag _w -> tag
    ShapePtrBVLit tag _w _ -> tag
    ShapePtr tag _ _ -> tag

-- | Set the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
setPtrTag :: PtrShape ext w tag t -> tag t -> PtrShape ext w tag t
setPtrTag shape tag =
  case shape of
    ShapePtrBV _tag w -> ShapePtrBV tag w
    ShapePtrBVLit _tag w bv -> ShapePtrBVLit tag w bv
    ShapePtr _tag off tgt -> ShapePtr tag off tgt

-- | A 'Lens.Lens' for the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
ptrShapeTag :: Lens.Lens' (PtrShape ext w tag t) (tag t)
ptrShapeTag = Lens.lens getPtrTag setPtrTag

minimalPtrShape ::
  ( 1 C.<= w
  , Mem.HasPtrWidth wptr
  , Applicative m
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  (forall t'. C.TypeRepr t' -> m (tag t')) ->
  NatRepr w ->
  m (PtrShape ext wptr tag (Mem.LLVMPointerType w))
minimalPtrShape mkTag w =
  let tag = mkTag (Mem.LLVMPointerRepr w)
   in case C.testEquality w ?ptrWidth of
        Just C.Refl ->
          ShapePtr <$> tag <*> pure (Offset 0) <*> pure (ptrTarget Nothing Seq.empty)
        Nothing -> ShapePtrBV <$> tag <*> pure w

-- | The x86_64 stack pointer points to the end of a large, fresh, mostly-
-- uninitialized allocation, which is typical for a stack pointer. The very end
-- is initialized with (in order):
--
-- * A pointer-sized chunk of bytes representing the return address.
--
-- * A number of pointer-sized stack slocks, which are reserved for
--   stack-spilled arguments.
x64StackPtrShape ::
  -- | Initialize the end of the stack to a concrete sequence of bytes if the
  -- value is @Just@, otherwise initialize the end of the stack to 8 fresh,
  -- symbolic bytes.
  Maybe [Word8] ->
  ExtraStackSlots ->
  PtrShape ext wptr NoTag (Mem.LLVMPointerType wptr)
x64StackPtrShape returnAddrBytes stackArgSlots =
  let ptrWidth = Bytes 8
      (returnAddr, returnAddrSize) = returnAddrMemShape returnAddrBytes ptrWidth
      (stackArgs, stackArgsSize) = stackArgMemShapes stackArgSlots ptrWidth
      uninit = stackSizeInMiB - returnAddrSize - stackArgsSize
      tgt =
        ptrTarget Nothing $
          Seq.fromList $
            [Uninitialized uninit, returnAddr] List.++ stackArgs
   in ShapePtr NoTag (Offset uninit) tgt

-- | The PowerPC stack pointer points to the end of a large, fresh, mostly-
-- uninitialized allocation, which is typical for a stack pointer. The very end
-- is initialized with (in order):
--
-- * A pointer-sized chunk of bytes representing the return address.
--
-- * A pointer-sized zero value, representing the PowerPC back chain for the
--   entrypoint function.
--
-- * A number of pointer-sized stack slocks, which are reserved for
--   stack-spilled arguments.
ppcStackPtrShape ::
  Mem.HasPtrWidth wptr =>
  -- | Initialize the end of the stack to a concrete sequence of bytes if the
  -- value is @Just@, otherwise initialize the end of the stack to 8 fresh,
  -- symbolic bytes.
  Maybe [Word8] ->
  ExtraStackSlots ->
  PtrShape ext wptr NoTag (Mem.LLVMPointerType wptr)
ppcStackPtrShape returnAddrBytes stackArgSlots =
  let ptrWidth = Bytes.bitsToBytes (natValue ?ptrWidth)
      (returnAddr, returnAddrSize) = returnAddrMemShape returnAddrBytes ptrWidth
      byte = TaggedByte NoTag 0
      backChain = Exactly (List.replicate (fromIntegral ptrWidth) byte)
      backChainSize = ptrWidth
      (stackArgs, stackArgsSize) = stackArgMemShapes stackArgSlots ptrWidth
      uninit = stackSizeInMiB - returnAddrSize - backChainSize - stackArgsSize
      tgt =
        ptrTarget Nothing $
          Seq.fromList $
            [Uninitialized uninit, returnAddr, backChain] List.++ stackArgs
   in ShapePtr NoTag (Offset uninit) tgt

-- | The AArch32 stack pointer points to the end of a large, fresh, mostly-
-- uninitialized allocation, which is typical for a stack pointer. The very end
-- is initialized with a number of pointer-sized stack slocks, which are
-- reserved for stack-spilled arguments.
--
-- Unlike the stack pointer in x86_64 and PowerPC, the AArch32 stack pointer
-- does /not/ store the return address on the stack.
armStackPtrShape ::
  ExtraStackSlots ->
  PtrShape ext wptr NoTag (Mem.LLVMPointerType wptr)
armStackPtrShape stackArgSlots =
  let ptrWidth = Bytes 4
      (stackArgs, stackArgsSize) = stackArgMemShapes stackArgSlots ptrWidth
      uninit = stackSizeInMiB - stackArgsSize
      tgt =
        ptrTarget Nothing $
          Seq.fromList $
            Uninitialized uninit : stackArgs
   in ShapePtr NoTag (Offset uninit) tgt

-- Helper, not exported
stackSizeInMiB :: Bytes
stackSizeInMiB = 1024 * kib
 where
  kib = 1024

-- Helper, not exported
returnAddrMemShape :: Maybe [Word8] -> Bytes -> (MemShape wptr NoTag, Bytes)
returnAddrMemShape returnAddrBytes ptrWidth =
  case returnAddrBytes of
    Just bs ->
      let bs' = List.map (TaggedByte NoTag) bs
       in (Exactly bs', Bytes (fromIntegral (List.length bs)))
    Nothing -> (Initialized NoTag ptrWidth, ptrWidth)

-- Helper, not exported
stackArgMemShapes :: ExtraStackSlots -> Bytes -> ([MemShape wptr NoTag], Bytes)
stackArgMemShapes stackArgSlots ptrWidth =
  let stackArgs =
        List.replicate
          (getExtraStackSlots stackArgSlots)
          (Initialized NoTag ptrWidth)
      stackArgsSize = Bytes (toInteger stackArgSlots) * ptrWidth
   in (stackArgs, stackArgsSize)

-- This is far too broad, it makes the entire allocation consist of pointers
-- when it's likely that only one part of it needs to be.
bytesToPointers ::
  forall proxy m ext tag w ts.
  ( MonadIO m
  , MonadThrow m
  , Mem.HasPtrWidth w
  , CursorExt ext ~ Dereference ext w
  , Last ts ~ Mem.LLVMPointerType 8
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  proxy w ->
  tag (Mem.LLVMPointerType w) ->
  Cursor ext (Mem.LLVMPointerType w ': ts) ->
  PtrTarget w tag ->
  m (PtrTarget w tag)
bytesToPointers proxy tag path tgt =
  case (path, tgt) of
    -- At the end of the path, grow/initialize the pointer here
    (Here _, _) -> pure (ptrTargetToPtrs proxy tag tgt)
    -- Need to keep dereferencing as specified by the path
    (CursorExt (DereferencePtr @_ @_ @ts' idx rest), PtrTarget bid ms) ->
      case ms Seq.!? idx of
        Just (Pointer tag' off subTgt) -> do
          C.Refl <- pure $ lastCons (Proxy @(Mem.LLVMPointerType w)) (Proxy @ts')
          subTgt' <- bytesToPointers proxy tag rest subTgt
          pure (ptrTarget Nothing (Seq.update idx (Pointer tag' off subTgt') ms))
        Just Exactly{} -> pure (ptrTargetToPtrs proxy tag tgt)
        Just Initialized{} -> pure (ptrTargetToPtrs proxy tag tgt)
        Just Uninitialized{} -> pure (ptrTargetToPtrs proxy tag tgt)
        Nothing -> throw $ GreaseException $ "Internal error: path index out of bounds!"
    (CursorExt (DereferenceByte _ _), _) ->
      throw $ GreaseException $ "Internal error: can't have pointer precondition on byte!"

modifyPtrTarget ::
  forall proxy m ext tag w ts.
  ( MonadIO m
  , MonadThrow m
  , Mem.HasPtrWidth w
  , CursorExt ext ~ Dereference ext w
  , Last ts ~ Mem.LLVMPointerType w
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  proxy w ->
  -- | See 'growPtrTarget', 'initializePtrTarget', and
  -- 'initializeOrGrowPtrTarget'
  (PtrTarget w tag -> m (PtrTarget w tag)) ->
  Cursor ext (Mem.LLVMPointerType w ': ts) ->
  PtrTarget w tag ->
  m (PtrTarget w tag)
modifyPtrTarget proxy modify path tgt =
  case (path, tgt) of
    -- At the end of the path, grow/initialize/whatever the pointer here
    (Here _, _) -> modify tgt
    -- Need to keep dereferencing as specified by the path
    (CursorExt (DereferencePtr @_ @_ @ts' idx rest), PtrTarget _ ms) ->
      case ms Seq.!? idx of
        Just (Pointer tag off subTgt) -> do
          C.Refl <- pure $ lastCons (Proxy @(Mem.LLVMPointerType w)) (Proxy @ts')
          subTgt' <- modifyPtrTarget proxy modify rest subTgt
          pure (ptrTarget Nothing (Seq.update idx (Pointer tag off subTgt') ms))
        Just _ -> throw $ GreaseException $ "Internal error: mismatched selector and pointer target!"
        Nothing -> throw $ GreaseException $ "Internal error: path index out of bounds!"
    (CursorExt (DereferenceByte _ _), _) ->
      throw $ GreaseException $ "Internal error: can't have pointer precondition on byte!"
