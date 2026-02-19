{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Shape.Pointer (
  ExtraStackSlots (..),
  PtrDataMode (..),
  PtrData (..),
  PtrModeRepr (..),
  KnownPtrMode (..),
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
  parseJsonPtrShape,
  x64StackPtrShape,
  ppcStackPtrShape,
  armStackPtrShape,
  bytesToPointers,
  growPtrTargetBy,
  growPtrTargetUpTo,
  growPtrTarget,
  initializePtrTarget,
  initializeOrGrowPtrTarget,
  ModifyPtrError (..),
  modifyPtrTarget,
) where

import Control.Lens qualified as Lens
import Data.Aeson ((.:))
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.BitVector.Sized (BV)
import Data.BitVector.Sized qualified as BV
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.Classes (ShowF)
import Data.Parameterized.NatRepr (NatRepr, natValue)
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some (Some (Some))
import Data.Parameterized.TraversableF qualified as TF
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import Data.Word (Word8)
import GHC.TypeLits (type Natural, type (<=))
import Grease.Cursor (Cursor (CursorExt, Here), CursorExt, Last, lastCons)
import Grease.Cursor.Pointer (Dereference (DereferenceByte, DereferencePtr))
import Grease.Shape.NoTag (NoTag (NoTag))
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Bytes (Bytes (Bytes))
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Prettyprinter qualified as PP

-- | Allocate this many pointer-sized stack slots beyond the return address,
-- which are reserved for stack-spilled arguments.
newtype ExtraStackSlots = ExtraStackSlots {getExtraStackSlots :: Int}
  -- See Note [Derive Read/Show instances with the newtype strategy]
  deriving newtype (Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | Mode indicating what pointer metadata is present
data PtrDataMode = Precond | NoData

-- | Pointer data that may or may not be present
data family PtrData (mode :: PtrDataMode) (w :: Natural) (tag :: C.CrucibleType -> Type)

-- | Precondition mode: contains offset and target
data instance PtrData 'Precond w tag
  = PrecondPtrData
  { precondOffset :: Offset
  , precondTarget :: PtrTarget w tag 'Precond
  }

data PtrModeRepr (tp :: PtrDataMode) where
  PrecondRepr :: PtrModeRepr 'Precond
  NoDataRepr :: PtrModeRepr 'NoData

-- | Typeclass for types with a known PtrDataMode at compile time
class KnownPtrMode (mode :: PtrDataMode) where
  knownPtrMode :: PtrModeRepr mode

instance KnownPtrMode 'Precond where
  knownPtrMode = PrecondRepr

instance KnownPtrMode 'NoData where
  knownPtrMode = NoDataRepr

-- | No data mode: empty, pointer data has been removed
data instance PtrData 'NoData w tag = NoPtrData

deriving instance
  ( Eq (tag (CLM.LLVMPointerType 8))
  , Eq (tag (C.VectorType (CLM.LLVMPointerType 8)))
  , Eq (tag (CLM.LLVMPointerType w))
  ) =>
  Eq (PtrData 'Precond w tag)

deriving instance Eq (PtrData 'NoData w tag)

newtype BlockId = BlockId {getBlockId :: Int}
  deriving (Eq, Ord, Show)

-- | A byte ('Word8') along with a @tag@ (see 'Grease.Shape.Shape').
data TaggedByte tag
  = TaggedByte
  { taggedByteTag :: tag (CLM.LLVMPointerType 8)
  , taggedByteValue :: Word8
  }

deriving instance Eq (tag (CLM.LLVMPointerType 8)) => Eq (TaggedByte tag)

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
  (tag (CLM.LLVMPointerType 8) -> m (tag' (CLM.LLVMPointerType 8))) ->
  TaggedByte tag ->
  m (TaggedByte tag')
traverseTaggedByte f (TaggedByte tag val) = TaggedByte <$> f tag <*> pure val

-- | Part of the content of an allocation (see 'PtrTarget')
--
-- Type parameters:
--
-- * @wptr@: Width of a pointer, in bits
-- * @tag@: See 'Grease.Shape.Shape'
-- * @ptrData@: Mode indicating whether pointer metadata is present
type MemShape :: Natural -> (C.CrucibleType -> Type) -> PtrDataMode -> Type
data MemShape wptr tag ptrData
  = -- | Some number of uninitialized bytes
    Uninitialized !Bytes
  | -- | Some number of symbolically-initialized bytes
    Initialized (tag (C.VectorType (CLM.LLVMPointerType 8))) !Bytes
  | -- | Several (generally 4 or 8) initialized bytes that form a pointer, plus
    -- an offset into that pointer
    Pointer (tag (CLM.LLVMPointerType wptr)) (PtrData ptrData wptr tag)
  | -- | Some concrete bytes
    Exactly [TaggedByte tag]

deriving instance
  ( Eq (tag (CLM.LLVMPointerType 8))
  , Eq (tag (C.VectorType (CLM.LLVMPointerType 8)))
  , Eq (tag (CLM.LLVMPointerType wptr))
  , Eq (PtrData ptrData wptr tag)
  ) =>
  Eq (MemShape wptr tag ptrData)

instance (KnownPtrMode ptrData, MC.PrettyF tag) => PP.Pretty (MemShape wptr tag ptrData) where
  pretty =
    \case
      Uninitialized bs -> "uninitialized x" PP.<+> PP.viaShow (CLB.bytesToInteger bs)
      Initialized tag bs ->
        PP.hcat
          [ "initialized"
          , ppTag tag
          , "x "
          , PP.viaShow (CLB.bytesToInteger bs)
          ]
      Pointer tag ptrData ->
        case knownPtrMode @ptrData of
          PrecondRepr ->
            case ptrData of
              PrecondPtrData offset tgt ->
                PP.hcat
                  [ "ptr"
                  , ppTag tag
                  , ":"
                  , PP.pretty tgt
                  , "+" PP.<> PP.viaShow offset
                  ]
          NoDataRepr ->
            case ptrData of
              NoPtrData ->
                PP.hcat
                  [ "ptr"
                  , ppTag tag
                  , ":removed"
                  ]
      Exactly bs -> "exactly:" PP.<+> PP.pretty bs

-- Note: FunctorF/FoldableF/TraversableF instances cannot be provided for MemShape
-- because after adding the ptrData parameter, MemShape wptr has kind
-- (C.CrucibleType -> Type) -> PtrDataMode -> Type, but these type classes
-- expect (k -> *) -> *. Use traverseMemShapeWithType instead.

-- | Like 'TF.traverseF', but with access to the appropriate 'C.TypeRepr'.
traverseMemShapeWithType ::
  CLM.HasPtrWidth wptr =>
  Applicative m =>
  PtrModeRepr ptrData ->
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  MemShape wptr tag ptrData ->
  m (MemShape wptr tag' ptrData)
traverseMemShapeWithType mode f =
  \case
    Uninitialized bs -> pure (Uninitialized bs)
    Initialized tag bs ->
      Initialized
        <$> f (C.VectorRepr (CLM.LLVMPointerRepr C.knownNat)) tag
        <*> pure bs
    Pointer tag ptrData ->
      Pointer
        <$> f (CLM.LLVMPointerRepr ?ptrWidth) tag
        <*> case mode of
          PrecondRepr ->
            case ptrData of
              PrecondPtrData offset tgt -> PrecondPtrData offset <$> traversePtrTargetWithType mode f tgt
          NoDataRepr ->
            case ptrData of
              NoPtrData -> pure NoPtrData
    Exactly bs ->
      Exactly
        <$> traverse (traverseTaggedByte (f (CLM.LLVMPointerRepr (C.knownNat @8)))) bs

merge ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  MemShape wptr tag ptrData ->
  MemShape wptr tag ptrData ->
  Maybe (MemShape wptr tag ptrData)
merge =
  \cases
    (Uninitialized i) (Uninitialized j) -> Just (Uninitialized (i + j))
    (Initialized tagi i) (Initialized tagj j) -> Just (Initialized (tagi <> tagj) (i + j))
    (Exactly i) (Exactly j) -> Just (Exactly (i List.++ j))
    _ _ -> Nothing

-- | Number of bytes required to store this 'MemShape'
memShapeSize ::
  CLM.HasPtrWidth w =>
  proxy w ->
  MemShape wptr tag ptrData ->
  Bytes
memShapeSize _proxy =
  \case
    Uninitialized bs -> bs
    Initialized _tag bs -> bs
    Pointer _ _ -> CLB.bitsToBytes (C.widthVal ?ptrWidth)
    Exactly bs -> CLB.Bytes (fromIntegral (List.length bs))

initializeMemShape ::
  tag (C.VectorType (CLM.LLVMPointerType 8)) ->
  MemShape wptr tag ptrData ->
  MemShape wptr tag ptrData
initializeMemShape tag =
  \case
    Uninitialized bs -> Initialized tag bs
    ms -> ms

{-
Note [Deduplicating Pointer Targets Based on BlockIDs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During parsing of shapes, a given block is parsed as many times as it is needed
into a 'PtrTarget'. This duplication of blocks causes 'setup' to lose track
of which 'PtrTarget's are identical and should be aliased. Indeed, the shapes
datastructure does not directly allow for 'PtrTarget's to be aliased. We create
aliasing by annotating 'PtrTarget's with an optional 'BlockId'.

'PtrTarget's with a 'BlockId' are deemed equivalent and during 'setup' each
use of a given 'BlockId' will be deduplicated into a single runtime value.
'PtrTarget's without a 'BlockId' can be thought of as fresh on-the-fly
'BlockId's which is how the printer handles them. Inside of setup, pointer
initialization is memoized such that the same result is returned for matching
'BlockId's. This behavior means that for a given list of shapes passed to
'setup' any 'PtrTarget' with a matching 'BlockId' is expected to have the same
shape, otherwise the first observed 'PtrTarget's shape will win.
-}

-- | The target of a pointer.
--
-- An empty sequence indicates that the pointer is not yet known to point to
-- allocated space. Otherwise, the pointer points to an allocation large enough
-- to hold all the 'MemShape's in the 'Seq' (see 'ptrTargetSize').
--
-- The `BlockId` is used to deduplicate target blocks during setup. If a
-- `BlockId` is present a single block will be allocated for that identifier.
-- If the identifier is `Nothing` then a fresh identifier will be generated.
-- Invariant: it is expected that any two 'PtrTarget's with the same 'BlockId'
-- used within the same context will have the same shape. Violating this
-- invariant will mean that 'setup' will produce incorrect blocks for the second
-- time a 'BlockId' is used.
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
-- * @ptrData@: Mode indicating whether pointer metadata is present
data PtrTarget wptr tag ptrData
  = PtrTarget
  { ptrTargetBlock :: Maybe BlockId
  , ptrTargetShapes :: Seq (MemShape wptr tag ptrData)
  }

-- Note: FunctorF/FoldableF/TraversableF instances cannot be provided for PtrTarget
-- because after adding the ptrData parameter, PtrTarget wptr has kind
-- (C.CrucibleType -> Type) -> PtrDataMode -> Type, but these type classes
-- expect (k -> *) -> *. Use traversePtrTargetWithType instead.

deriving instance
  ( Eq (tag (CLM.LLVMPointerType 8))
  , Eq (tag (C.VectorType (CLM.LLVMPointerType 8)))
  , Eq (tag (CLM.LLVMPointerType wptr))
  , Eq (PtrData ptrData wptr tag)
  ) =>
  Eq (PtrTarget wptr tag ptrData)

-- | Like 'TF.traverseF', but with access to the appropriate 'C.TypeRepr'.
traversePtrTargetWithType ::
  CLM.HasPtrWidth wptr =>
  Applicative m =>
  PtrModeRepr ptrData ->
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  PtrTarget wptr tag ptrData ->
  m (PtrTarget wptr tag' ptrData)
traversePtrTargetWithType mode f (PtrTarget bid tgt) =
  PtrTarget bid <$> traverse (traverseMemShapeWithType mode f) tgt

-- | Smart constructor that merges compatible adjacent shapes.
ptrTarget ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  Maybe BlockId ->
  Seq (MemShape wptr tag ptrData) ->
  PtrTarget wptr tag ptrData
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
  CLM.HasPtrWidth w =>
  proxy w ->
  PtrTarget wptr tag ptrData ->
  Bytes
ptrTargetSize proxy (PtrTarget _ s) = Foldable.sum (fmap (memShapeSize proxy) s)

-- | Grow an allocation by adding some uninitialized bytes to the end
growPtrTargetBy ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  Bytes ->
  PtrTarget wptr tag ptrData ->
  PtrTarget wptr tag ptrData
growPtrTargetBy amount (PtrTarget bid s) = ptrTarget bid (s Seq.|> Uninitialized amount)

-- | Grow an allocation by adding uninitialized bytes to the end, up to the
-- given size (or at least by 1).
growPtrTargetUpTo ::
  CLM.HasPtrWidth w =>
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  -- | Add uninitialized bytes to the allocation until it becomes this size
  Bytes ->
  PtrTarget wptr tag ptrData ->
  PtrTarget wptr tag ptrData
growPtrTargetUpTo amount t =
  growPtrTargetBy (max 1 (amount - ptrTargetSize ?ptrWidth t)) t

-- | Grow an allocation by adding an uninitialized byte to the end
growPtrTarget ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  PtrTarget wptr tag ptrData ->
  PtrTarget wptr tag ptrData
growPtrTarget = growPtrTargetBy (CLB.toBytes (1 :: Integer))

-- | Initialize all uninitialized parts of an allocation
initializePtrTarget ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  -- | Tag for newly-initialized bytes
  tag (C.VectorType (CLM.LLVMPointerType 8)) ->
  PtrTarget wptr tag ptrData ->
  PtrTarget wptr tag ptrData
initializePtrTarget tag (PtrTarget bid ms) =
  ptrTarget bid (fmap (initializeMemShape tag) ms)

-- | Initialize all uninitialized parts of an allocation, or if it is already
-- fully initialized, grow it by one uninitialized byte.
initializeOrGrowPtrTarget ::
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  -- | Tag for newly-initialized bytes
  tag (C.VectorType (CLM.LLVMPointerType 8)) ->
  PtrTarget wptr tag ptrData ->
  PtrTarget wptr tag ptrData
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
  CLM.HasPtrWidth wptr =>
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  proxy wptr ->
  tag (CLM.LLVMPointerType wptr) ->
  PtrTarget wptr tag 'Precond ->
  PtrTarget wptr tag 'Precond
ptrTargetToPtrs proxy tag tgt =
  let sz = ptrTargetSize proxy tgt
      ptrBytes = CLB.bitsToBytes (C.widthVal ?ptrWidth)
      (nPtrs, remBytes) = sz `divMod` ptrBytes
      nPtrs' = CLB.bytesToInteger $ if remBytes == 0 then nPtrs else nPtrs + 1
      genSeq n x = Seq.iterateN n id x
   in ( PtrTarget Nothing $
          genSeq (fromIntegral nPtrs') $
            Pointer tag (PrecondPtrData (Offset 0) (ptrTarget Nothing Seq.Empty))
      )

instance PP.Pretty BlockId where
  pretty bid = "blockid:" PP.<+> (PP.pretty $ getBlockId bid)

instance (KnownPtrMode ptrData, MC.PrettyF tag) => PP.Pretty (PtrTarget wptr tag ptrData) where
  pretty :: (KnownPtrMode ptrData, MC.PrettyF tag) => PtrTarget wptr tag ptrData -> PP.Doc ann
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

-- * @ptrData@: Mode indicating whether pointer metadata is present

-- * @t@: Crucible type corresponding to this shape
type PtrShape :: Type -> Natural -> (C.CrucibleType -> Type) -> PtrDataMode -> C.CrucibleType -> Type
data PtrShape (ext :: Type) w tag ptrData (t :: C.CrucibleType) where
  ShapePtrBV ::
    1 C.<= w' =>
    tag (CLM.LLVMPointerType w') ->
    NatRepr w' ->
    PtrShape ext w tag ptrData (CLM.LLVMPointerType w')
  ShapePtrBVLit ::
    1 C.<= w' =>
    tag (CLM.LLVMPointerType w') ->
    NatRepr w' ->
    BV w' ->
    PtrShape ext w tag ptrData (CLM.LLVMPointerType w')
  ShapePtr ::
    tag (CLM.LLVMPointerType w) ->
    PtrData ptrData w tag ->
    PtrShape ext w tag ptrData (CLM.LLVMPointerType w)

-- | Returns @'Just' 'Refl'@ iff the shapes are identical.
--
-- Disobeys the guidance
--
-- > Typically, only singleton types should inhabit this class.
--
-- on 'TestEquality', because 'PtrShape' is not a singleton.
instance
  ( Eq (tag (CLM.LLVMPointerType 8))
  , Eq (tag (CLM.LLVMPointerType w))
  , Eq (tag (C.VectorType (CLM.LLVMPointerType 8)))
  , Eq (PtrData ptrData w tag)
  , TestEquality tag
  ) =>
  TestEquality (PtrShape ext w tag ptrData)
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
      (ShapePtr tag ptrData, ShapePtr tag' ptrData') ->
        case (testEquality tag tag', ptrData == ptrData') of
          (Just Refl, True) -> Just Refl
          _ -> Nothing
      _ -> Nothing

instance (KnownPtrMode ptrData, MC.PrettyF tag) => Show (PtrShape ext w tag ptrData t) where
  show = show . MC.prettyF
instance (KnownPtrMode ptrData, MC.PrettyF tag) => ShowF (PtrShape ext w tag ptrData)
instance (KnownPtrMode ptrData, MC.PrettyF tag) => MC.PrettyF (PtrShape ext w tag ptrData) where
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
      ShapePtr tag ptrData ->
        prettyPtrData tag ptrData
   where
    prettyPtrData :: tag (CLM.LLVMPointerType w) -> PtrData ptrData w tag -> PP.Doc ann
    prettyPtrData tag dat =
      case knownPtrMode @ptrData of
        PrecondRepr ->
          case dat of
            PrecondPtrData offset tgt ->
              PP.pretty tgt PP.<> "+" PP.<> PP.viaShow offset PP.<> ppTag tag
        NoDataRepr ->
          case dat of
            NoPtrData -> "ptr" PP.<> ppTag tag PP.<> ":removed"

-- Note: FunctorFC/FoldableFC/TraversableFC instances cannot be provided for PtrShape
-- because after adding the ptrData parameter, PtrShape ext w has kind
-- (C.CrucibleType -> Type) -> PtrDataMode -> C.CrucibleType -> Type, but these type
-- classes expect (k -> *) -> k' -> *. Use traversePtrShapeWithType instead.

ptrShapeType :: CLM.HasPtrWidth w => PtrShape ext w tag ptrData t -> C.TypeRepr t
ptrShapeType =
  \case
    ShapePtrBV _tag w -> CLM.LLVMPointerRepr w
    ShapePtrBVLit _tag w _ -> CLM.LLVMPointerRepr w
    ShapePtr _ _ -> CLM.LLVMPointerRepr ?ptrWidth

-- | Like 'TF.traverseFC', but with access to the appropriate 'C.TypeRepr'.
traversePtrShapeWithType ::
  forall mode wptr m tag tag' ext t.
  CLM.HasPtrWidth wptr =>
  Applicative m =>
  PtrModeRepr mode ->
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  PtrShape ext wptr tag mode t ->
  m (PtrShape ext wptr tag' mode t)
traversePtrShapeWithType md f =
  \case
    ShapePtrBV tag w ->
      ShapePtrBV <$> f (CLM.LLVMPointerRepr w) tag <*> pure w
    ShapePtrBVLit tag w bv ->
      ShapePtrBVLit <$> f (CLM.LLVMPointerRepr w) tag <*> pure w <*> pure bv
    ShapePtr tag ptrData ->
      ShapePtr
        <$> f (CLM.LLVMPointerRepr ?ptrWidth) tag
        <*> traversePtrDataHelper f ptrData
 where
  traversePtrDataHelper :: Applicative m => (forall x. C.TypeRepr x -> tag x -> m (tag' x)) -> PtrData mode wptr tag -> m (PtrData mode wptr tag')
  traversePtrDataHelper g dat =
    case md of
      NoDataRepr -> pure NoPtrData
      PrecondRepr ->
        let PrecondPtrData{precondOffset, precondTarget} = dat
         in PrecondPtrData precondOffset <$> traversePtrTargetWithType md g precondTarget

-- | Get the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
getPtrTag :: PtrShape ext w tag ptrData t -> tag t
getPtrTag =
  \case
    ShapePtrBV tag _w -> tag
    ShapePtrBVLit tag _w _ -> tag
    ShapePtr tag _ -> tag

-- | Set the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
setPtrTag :: PtrShape ext w tag ptrData t -> tag t -> PtrShape ext w tag ptrData t
setPtrTag shape tag =
  case shape of
    ShapePtrBV _tag w -> ShapePtrBV tag w
    ShapePtrBVLit _tag w bv -> ShapePtrBVLit tag w bv
    ShapePtr _tag ptrData -> ShapePtr tag ptrData

-- | A 'Lens.Lens' for the @tag@ on this 'PtrShape'. (See 'Grease.Shape.Shape'.)
ptrShapeTag :: Lens.Lens' (PtrShape ext w tag ptrData t) (tag t)
ptrShapeTag = Lens.lens getPtrTag setPtrTag

minimalPtrShape ::
  ( 1 C.<= w
  , CLM.HasPtrWidth wptr
  , Applicative m
  , Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8)))
  ) =>
  (forall t'. C.TypeRepr t' -> m (tag t')) ->
  NatRepr w ->
  m (PtrShape ext wptr tag ptrData (CLM.LLVMPointerType w))
minimalPtrShape mkTag w =
  ShapePtrBV <$> mkTag (CLM.LLVMPointerRepr w) <*> pure w

-- | Parse a 'PtrShape' from JSON
parseJsonPtrShape ::
  forall ext w tag.
  Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
  -- | Parser for @tag@s
  (forall t. Aeson.KeyMap Aeson.Value -> Aeson.Parser (tag t)) ->
  Aeson.Value ->
  Aeson.Parser (Some (PtrShape ext w tag 'Precond))
parseJsonPtrShape parseTag =
  Aeson.withObject "Shape" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "bv" ->
        withWidth v $ \w -> do
          tag <- parseTag v
          pure (Some (ShapePtrBV tag w))
      "bv-lit" ->
        withWidth v $ \w -> do
          tag <- parseTag v
          val <- v .: "val"
          pure (Some (ShapePtrBVLit tag w (BV.mkBV w val)))
      "ptr" -> do
        tag <- parseTag v
        offset <- parseOffset v
        tgt <- parseJsonPtrTarget =<< v .: "target"
        pure (Some (ShapePtr tag (PrecondPtrData offset tgt)))
      t -> fail ("Unknown pointer type: " ++ Text.unpack t)
 where
  withWidth ::
    Aeson.KeyMap Aeson.Value ->
    (forall n. 1 <= n => NatRepr n -> Aeson.Parser r) ->
    Aeson.Parser r
  withWidth v k = do
    wNat <- v .: "width"
    Some w <- pure (NatRepr.mkNatRepr wNat)
    case NatRepr.testLeq (NatRepr.knownNat @1) w of
      Nothing -> fail "Cannot have zero-width bitvector"
      Just NatRepr.LeqProof -> k w

  parseOffset v = Offset . CLB.toBytes @Int <$> v .: "offset"

  parseJsonPtrTarget ::
    Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8))) =>
    Aeson.Value ->
    Aeson.Parser (PtrTarget w tag 'Precond)
  parseJsonPtrTarget v =
    ptrTarget Nothing . Seq.fromList <$> Aeson.listParser parseJsonMemShape v

  parseJsonMemShape ::
    Aeson.Value ->
    Aeson.Parser (MemShape w tag 'Precond)
  parseJsonMemShape =
    Aeson.withObject "MemShape" $ \v -> do
      ty <- (v .: "type" :: Aeson.Parser Text)
      case ty of
        "exactly" -> do
          let parseByte =
                Aeson.withObject "byte" $ \v' -> do
                  tag <- parseTag v
                  val <- v' .: "val"
                  pure (TaggedByte tag val)
          bytes <- Aeson.listParser parseByte =<< v .: "exactly"
          pure (Exactly bytes)
        "init" -> do
          tag <- parseTag v
          Initialized tag . CLB.toBytes @Int <$> v .: "init"
        "ptr" -> do
          tag <- parseTag v
          offset <- parseOffset v
          tgt <- parseJsonPtrTarget =<< v .: "target"
          pure (Pointer tag (PrecondPtrData offset tgt))
        "uninit" -> Uninitialized . CLB.toBytes @Int <$> v .: "uninit"
        t -> fail ("Unknown memory shape type: " ++ Text.unpack t)

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
  PtrShape ext wptr NoTag 'Precond (CLM.LLVMPointerType wptr)
x64StackPtrShape returnAddrBytes stackArgSlots =
  let ptrWidth = Bytes 8
      (returnAddr, returnAddrSize) = returnAddrMemShape returnAddrBytes ptrWidth
      (stackArgs, stackArgsSize) = stackArgMemShapes stackArgSlots ptrWidth
      uninit = stackSizeInMiB - returnAddrSize - stackArgsSize
      tgt =
        ptrTarget Nothing $
          Seq.fromList $
            [Uninitialized uninit, returnAddr] List.++ stackArgs
   in ShapePtr NoTag (PrecondPtrData (Offset uninit) tgt)

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
  CLM.HasPtrWidth wptr =>
  -- | Initialize the end of the stack to a concrete sequence of bytes if the
  -- value is @Just@, otherwise initialize the end of the stack to 8 fresh,
  -- symbolic bytes.
  Maybe [Word8] ->
  ExtraStackSlots ->
  PtrShape ext wptr NoTag 'Precond (CLM.LLVMPointerType wptr)
ppcStackPtrShape returnAddrBytes stackArgSlots =
  let ptrWidth = CLB.bitsToBytes (natValue ?ptrWidth)
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
   in ShapePtr NoTag (PrecondPtrData (Offset uninit) tgt)

-- | The AArch32 stack pointer points to the end of a large, fresh, mostly-
-- uninitialized allocation, which is typical for a stack pointer. The very end
-- is initialized with a number of pointer-sized stack slocks, which are
-- reserved for stack-spilled arguments.
--
-- Unlike the stack pointer in x86_64 and PowerPC, the AArch32 stack pointer
-- does /not/ store the return address on the stack.
armStackPtrShape ::
  ExtraStackSlots ->
  PtrShape ext wptr NoTag 'Precond (CLM.LLVMPointerType wptr)
armStackPtrShape stackArgSlots =
  let ptrWidth = Bytes 4
      (stackArgs, stackArgsSize) = stackArgMemShapes stackArgSlots ptrWidth
      uninit = stackSizeInMiB - stackArgsSize
      tgt =
        ptrTarget Nothing $
          Seq.fromList $
            Uninitialized uninit : stackArgs
   in ShapePtr NoTag (PrecondPtrData (Offset uninit) tgt)

-- Helper, not exported
stackSizeInMiB :: Bytes
stackSizeInMiB = 1024 * kib
 where
  kib = 1024

-- Helper, not exported
returnAddrMemShape :: Maybe [Word8] -> Bytes -> (MemShape wptr NoTag 'Precond, Bytes)
returnAddrMemShape returnAddrBytes ptrWidth =
  case returnAddrBytes of
    Just bs ->
      let bs' = List.map (TaggedByte NoTag) bs
       in (Exactly bs', Bytes (fromIntegral (List.length bs)))
    Nothing -> (Initialized NoTag ptrWidth, ptrWidth)

-- Helper, not exported
stackArgMemShapes :: ExtraStackSlots -> Bytes -> ([MemShape wptr NoTag 'Precond], Bytes)
stackArgMemShapes stackArgSlots ptrWidth =
  let stackArgs =
        List.replicate
          (getExtraStackSlots stackArgSlots)
          (Initialized NoTag ptrWidth)
      stackArgsSize = Bytes (toInteger stackArgSlots) * ptrWidth
   in (stackArgs, stackArgsSize)

-- | Error type for pointer modification operations
data ModifyPtrError
  = ModifyPtrPathOutOfBounds !Int !Int
  | ModifyPtrMismatchedSelector
  | ModifyPtrBytePrecondition

instance PP.Pretty ModifyPtrError where
  pretty =
    \case
      ModifyPtrPathOutOfBounds idx upperBound ->
        PP.hcat
          [ "Path index out of bounds: "
          , PP.pretty idx
          , " (upper bound: "
          , PP.pretty upperBound
          , ")"
          ]
      ModifyPtrMismatchedSelector ->
        "Mismatched selector and pointer target"
      ModifyPtrBytePrecondition ->
        "Can't have pointer precondition on byte"

-- This is far too broad, it makes the entire allocation consist of pointers
-- when it's likely that only one part of it needs to be.
bytesToPointers ::
  forall proxy ext tag w ts.
  ( CLM.HasPtrWidth w
  , CursorExt ext ~ Dereference ext w
  , Last ts ~ CLM.LLVMPointerType 8
  , Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8)))
  ) =>
  proxy w ->
  tag (CLM.LLVMPointerType w) ->
  Cursor ext (CLM.LLVMPointerType w ': ts) ->
  PtrTarget w tag 'Precond ->
  Either ModifyPtrError (PtrTarget w tag 'Precond)
bytesToPointers proxy tag path tgt =
  case (path, tgt) of
    -- At the end of the path, grow/initialize the pointer here
    (Here _, _) -> pure (ptrTargetToPtrs proxy tag tgt)
    -- Need to keep dereferencing as specified by the path
    (CursorExt (DereferencePtr @_ @_ @ts' idx rest), PtrTarget _ ms) ->
      case ms Seq.!? idx of
        Just (Pointer tag' ptrData) -> do
          C.Refl <- Right $ lastCons (Proxy @(CLM.LLVMPointerType w)) (Proxy @ts')
          case ptrData of
            PrecondPtrData offset subTgt -> do
              subTgt' <- bytesToPointers proxy tag rest subTgt
              Right (ptrTarget Nothing (Seq.update idx (Pointer tag' (PrecondPtrData offset subTgt')) ms))
        Just Exactly{} -> Right (ptrTargetToPtrs proxy tag tgt)
        Just Initialized{} -> Right (ptrTargetToPtrs proxy tag tgt)
        Just Uninitialized{} -> Right (ptrTargetToPtrs proxy tag tgt)
        Nothing -> Left (ModifyPtrPathOutOfBounds idx (Seq.length ms))
    (CursorExt (DereferenceByte _ _), _) ->
      Left ModifyPtrBytePrecondition

modifyPtrTarget ::
  forall proxy ext tag w ts.
  ( CLM.HasPtrWidth w
  , CursorExt ext ~ Dereference ext w
  , Last ts ~ CLM.LLVMPointerType w
  , Semigroup (tag (C.VectorType (CLM.LLVMPointerType 8)))
  ) =>
  proxy w ->
  -- | See 'growPtrTarget', 'initializePtrTarget', and
  -- 'initializeOrGrowPtrTarget'
  (PtrTarget w tag 'Precond -> Either ModifyPtrError (PtrTarget w tag 'Precond)) ->
  Cursor ext (CLM.LLVMPointerType w ': ts) ->
  PtrTarget w tag 'Precond ->
  Either ModifyPtrError (PtrTarget w tag 'Precond)
modifyPtrTarget proxy modify path tgt =
  case (path, tgt) of
    -- At the end of the path, grow/initialize/whatever the pointer here
    (Here _, _) -> modify tgt
    -- Need to keep dereferencing as specified by the path
    (CursorExt (DereferencePtr @_ @_ @ts' idx rest), PtrTarget _ ms) ->
      case ms Seq.!? idx of
        Just (Pointer tag ptrData) -> do
          C.Refl <- pure $ lastCons (Proxy @(CLM.LLVMPointerType w)) (Proxy @ts')
          case ptrData of
            PrecondPtrData offset subTgt -> do
              subTgt' <- modifyPtrTarget proxy modify rest subTgt
              Right (ptrTarget Nothing (Seq.update idx (Pointer tag (PrecondPtrData offset subTgt')) ms))
        Just _ -> Left ModifyPtrMismatchedSelector
        Nothing -> Left (ModifyPtrPathOutOfBounds idx (Seq.length ms))
    (CursorExt (DereferenceByte _ _), _) ->
      Left ModifyPtrBytePrecondition
