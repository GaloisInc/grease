{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Setup.Annotations (
  SomeBaseSelector (..),
  SomePtrSelector (..),
  Annotations,
  empty,

  -- * Annotate
  annotate,
  annotatePtr,

  -- * Lookup
  lookupPtrAnnotation,
) where

import Control.Applicative ((<|>))
import Control.Lens ((%=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (MonadState (..))
import Data.Macaw.CFG qualified as MC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.Map qualified as MapF
import Data.Parameterized.Some (Some (..))
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Shape.Selector
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Types qualified as C
import What4.Expr.Builder qualified as W4
import What4.Interface qualified as W4

data SomeBaseSelector ext argTys t
  = forall ts regTy.
    Cursor.Last ts ~ C.BaseToType t =>
    SomeBaseSelector (Selector ext argTys ts regTy)

data SomePtrSelector ext argTys w
  = forall ts regTy.
    Cursor.Last ts ~ Mem.LLVMPointerType w =>
    SomePtrSelector (Selector ext argTys ts regTy)

newtype BVAnn sym w = BVAnn (W4.SymAnnotation sym (W4.BaseBVType w))

instance C.TestEquality (W4.SymAnnotation sym) => C.TestEquality (BVAnn sym) where
  testEquality (BVAnn ann1) (BVAnn ann2) =
    case C.testEquality ann1 ann2 of
      Just C.Refl -> Just C.Refl
      Nothing -> Nothing
  {-# INLINE testEquality #-}

instance MapF.OrdF (W4.SymAnnotation sym) => MapF.OrdF (BVAnn sym) where
  compareF (BVAnn ann1) (BVAnn ann2) =
    case MapF.compareF ann1 ann2 of
      MapF.LTF -> MapF.LTF
      MapF.EQF -> MapF.EQF
      MapF.GTF -> MapF.GTF
  {-# INLINE compareF #-}

-- | A newtype around @'W4.SymAnnotation' sym 'W4.BaseIntegerType'@. This is
-- done so that the 'Ord' instance can be defined in terms of the underlying
-- `W4.SymAnnotation`'s 'OrdF' instance.
newtype IntegerAnn sym = IntegerAnn (W4.SymAnnotation sym W4.BaseIntegerType)

instance C.TestEquality (W4.SymAnnotation sym) => Eq (IntegerAnn sym) where
  IntegerAnn ann1 == IntegerAnn ann2 =
    Maybe.isJust (C.testEquality ann1 ann2)
  {-# INLINE (==) #-}

instance MapF.OrdF (W4.SymAnnotation sym) => Ord (IntegerAnn sym) where
  compare (IntegerAnn ann1) (IntegerAnn ann2) =
    case MapF.compareF ann1 ann2 of
      MapF.LTF -> LT
      MapF.EQF -> EQ
      MapF.GTF -> GT
  {-# INLINE compare #-}

-- | Track the provenance of symbolic values using 'W4.SymAnnotation's.
data Annotations sym ext argTys
  = Annotations
  { _baseAnns :: MapF.MapF (W4.SymAnnotation sym) (SomeBaseSelector ext argTys)
  , _blockAnns :: Map (IntegerAnn sym) (Some (SomePtrSelector ext argTys))
  , _offsetAnns :: MapF.MapF (BVAnn sym) (SomePtrSelector ext argTys)
  }

makeLenses ''Annotations

empty :: Annotations sym ext argTys
empty =
  Annotations
    { _baseAnns = MapF.empty
    , _blockAnns = Map.empty
    , _offsetAnns = MapF.empty
    }

---------------------------------------------------------------------

-- * Annotate

annotate ::
  ( MonadIO m
  , MonadState (Annotations sym ext argTys) m
  , C.IsSymInterface sym
  , Cursor.Last ts ~ C.BaseToType t
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  W4.SymExpr sym t ->
  m (W4.SymExpr sym t)
annotate sym sel e = do
  (ann, e') <- case W4.getAnnotation sym e of
    Just ann -> pure (ann, e)
    Nothing -> liftIO $ W4.annotateTerm sym e
  baseAnns %= MapF.insert ann (SomeBaseSelector sel)
  pure e'

annotatePtr ::
  forall m sym ext argTys ts regTy w.
  ( MonadIO m
  , MonadState (Annotations sym ext argTys) m
  , C.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ Mem.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  Mem.LLVMPtr sym w ->
  m (Mem.LLVMPtr sym w)
annotatePtr sym sel ptr = do
  block <- liftIO $ W4.natToInteger sym (Mem.llvmPointerBlock ptr)
  (blockann, ptr') <- case W4.getAnnotation sym block of
    Just ann -> pure (ann, ptr)
    Nothing -> liftIO $ Mem.annotatePointerBlock sym ptr
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  blockAnns %= Map.insert (IntegerAnn blockann) (Some (SomePtrSelector sel))
  let offset = Mem.llvmPointerOffset ptr'
  (offann, ptr'') <- case W4.getAnnotation sym offset of
    Just ann -> pure (ann, ptr')
    Nothing -> liftIO $ Mem.annotatePointerOffset sym ptr'
  offsetAnns %= MapF.insert (BVAnn offann) (SomePtrSelector sel)
  pure ptr''

---------------------------------------------------------------------

-- * Lookup

findAnnotations ::
  forall sym brand st fs t t'.
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  ) =>
  sym ->
  W4.BaseTypeRepr t' ->
  W4.SymExpr sym t ->
  [W4.SymAnnotation sym t']
findAnnotations sym repr e = case W4.asApp e of
  Just app -> do
    let anns :: [W4.SymAnnotation sym t']
        anns = MC.foldMapFC (Maybe.maybeToList . getAnn) app
    case getAnn e of
      Nothing -> anns
      Just ann -> ann : anns
  Nothing -> Maybe.maybeToList (getAnn e)
 where
  getAnn ::
    forall tp.
    W4.Expr brand tp ->
    Maybe (W4.SymAnnotation sym t')
  getAnn expr =
    case W4.exprType expr of
      repr' | Just C.Refl <- C.testEquality repr repr' -> W4.getAnnotation sym expr
      _ -> Nothing

lookupSomePtrBlockAnnotation ::
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  ) =>
  Annotations sym ext argTys ->
  sym ->
  Mem.LLVMPtr sym w ->
  Maybe (Some (SomePtrSelector ext argTys))
lookupSomePtrBlockAnnotation anns sym ptr = do
  let block = W4.natToIntegerPure (Mem.llvmPointerBlock ptr)
  ann <- Maybe.listToMaybe (findAnnotations sym W4.BaseIntegerRepr block)
  -- In this lookup, 'Nothing' may arise if this pointer was created by a
  -- "skip" override (see "Grease.Skip"), because such overrides use the
  -- "setup" machinery, but discard the map that tracks the annotations on
  -- values they create.
  Map.lookup (IntegerAnn ann) (_blockAnns anns)

lookupPtrBlockAnnotation ::
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  Mem.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrBlockAnnotation anns sym w ptr = do
  Some (SomePtrSelector @_ @_ @_ @ts @regTy sel) <-
    lookupSomePtrBlockAnnotation anns sym ptr
  let cursor = sel ^. selectorPath
  Mem.LLVMPointerRepr w' <- pure $ PtrCursor.cursorRepr cursor
  Refl <- C.testEquality w w'
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  pure $ SomePtrSelector sel

lookupPtrOffsetAnnotation ::
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  Mem.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrOffsetAnnotation anns sym w ptr =
  case findAnnotations sym (W4.BaseBVRepr w) (Mem.llvmPointerOffset ptr) of
    [] -> Nothing
    (ann : _) ->
      -- In this lookup, 'Nothing' may arise if this pointer was created by a
      -- "skip" override (see "Grease.Skip"), because such overrides use the
      -- "setup" machinery, but discard the map that tracks the annotations on
      -- values they create.
      MapF.lookup (BVAnn ann) (_offsetAnns anns)

lookupPtrAnnotation ::
  ( C.IsSymInterface sym
  , sym ~ W4.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  Mem.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrAnnotation anns sym w p = do
  let blockAnn = lookupPtrBlockAnnotation anns sym w p
  let offAnn = lookupPtrOffsetAnnotation anns sym w p
  blockAnn <|> offAnn
