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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState)
import Data.Macaw.CFG qualified as MC
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.Map qualified as MapF
import Data.Parameterized.Some (Some (Some))
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Shape.Selector (Selector, selectorPath)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.LLVM.MemModel.Pointer qualified as CLMP
import Lang.Crucible.Types qualified as C
import What4.Expr.Builder qualified as WEB
import What4.Interface qualified as WI

data SomeBaseSelector ext argTys t
  = forall ts regTy.
    Cursor.Last ts ~ C.BaseToType t =>
    SomeBaseSelector (Selector ext argTys ts regTy)

data SomePtrSelector ext argTys w
  = forall ts regTy.
    Cursor.Last ts ~ CLMP.LLVMPointerType w =>
    SomePtrSelector (Selector ext argTys ts regTy)

newtype BVAnn sym w = BVAnn (WI.SymAnnotation sym (WI.BaseBVType w))

instance C.TestEquality (WI.SymAnnotation sym) => C.TestEquality (BVAnn sym) where
  testEquality (BVAnn ann1) (BVAnn ann2) =
    case C.testEquality ann1 ann2 of
      Just C.Refl -> Just C.Refl
      Nothing -> Nothing
  {-# INLINE testEquality #-}

instance MapF.OrdF (WI.SymAnnotation sym) => MapF.OrdF (BVAnn sym) where
  compareF (BVAnn ann1) (BVAnn ann2) =
    case MapF.compareF ann1 ann2 of
      MapF.LTF -> MapF.LTF
      MapF.EQF -> MapF.EQF
      MapF.GTF -> MapF.GTF
  {-# INLINE compareF #-}

-- | A newtype around @'WI.SymAnnotation' sym 'WI.BaseIntegerType'@. This is
-- done so that the 'Ord' instance can be defined in terms of the underlying
-- `WI.SymAnnotation`'s 'OrdF' instance.
newtype IntegerAnn sym = IntegerAnn (WI.SymAnnotation sym WI.BaseIntegerType)

instance C.TestEquality (WI.SymAnnotation sym) => Eq (IntegerAnn sym) where
  IntegerAnn ann1 == IntegerAnn ann2 =
    Maybe.isJust (C.testEquality ann1 ann2)
  {-# INLINE (==) #-}

instance MapF.OrdF (WI.SymAnnotation sym) => Ord (IntegerAnn sym) where
  compare (IntegerAnn ann1) (IntegerAnn ann2) =
    case MapF.compareF ann1 ann2 of
      MapF.LTF -> LT
      MapF.EQF -> EQ
      MapF.GTF -> GT
  {-# INLINE compare #-}

-- | Track the provenance of symbolic values using 'WI.SymAnnotation's.
data Annotations sym ext argTys
  = Annotations
  { _baseAnns :: MapF.MapF (WI.SymAnnotation sym) (SomeBaseSelector ext argTys)
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
  , CB.IsSymInterface sym
  , Cursor.Last ts ~ C.BaseToType t
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  WI.SymExpr sym t ->
  m (WI.SymExpr sym t)
annotate sym sel e = do
  (ann, e') <- case WI.getAnnotation sym e of
    Just ann -> pure (ann, e)
    Nothing -> liftIO $ WI.annotateTerm sym e
  baseAnns %= MapF.insert ann (SomeBaseSelector sel)
  pure e'

annotatePtr ::
  forall m sym ext argTys ts regTy w.
  ( MonadIO m
  , MonadState (Annotations sym ext argTys) m
  , CB.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ CLMP.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  CLMP.LLVMPtr sym w ->
  m (CLMP.LLVMPtr sym w)
annotatePtr sym sel ptr = do
  block <- liftIO $ WI.natToInteger sym (CLMP.llvmPointerBlock ptr)
  (blockann, ptr') <- case WI.getAnnotation sym block of
    Just ann -> pure (ann, ptr)
    Nothing -> liftIO $ CLMP.annotatePointerBlock sym ptr
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  blockAnns %= Map.insert (IntegerAnn blockann) (Some (SomePtrSelector sel))
  let offset = CLMP.llvmPointerOffset ptr'
  (offann, ptr'') <- case WI.getAnnotation sym offset of
    Just ann -> pure (ann, ptr')
    Nothing -> liftIO $ CLMP.annotatePointerOffset sym ptr'
  offsetAnns %= MapF.insert (BVAnn offann) (SomePtrSelector sel)
  pure ptr''

---------------------------------------------------------------------

-- * Lookup

findAnnotations ::
  forall sym brand st fs t t'.
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder brand st fs
  ) =>
  sym ->
  WI.BaseTypeRepr t' ->
  WI.SymExpr sym t ->
  [WI.SymAnnotation sym t']
findAnnotations sym repr e = case WEB.asApp e of
  Just app -> do
    let anns :: [WI.SymAnnotation sym t']
        anns = MC.foldMapFC (Maybe.maybeToList . getAnn) app
    case getAnn e of
      Nothing -> anns
      Just ann -> ann : anns
  Nothing -> Maybe.maybeToList (getAnn e)
 where
  getAnn ::
    forall tp.
    WEB.Expr brand tp ->
    Maybe (WI.SymAnnotation sym t')
  getAnn expr =
    case WI.exprType expr of
      repr' | Just C.Refl <- C.testEquality repr repr' -> WI.getAnnotation sym expr
      _ -> Nothing

lookupSomePtrBlockAnnotation ::
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder brand st fs
  ) =>
  Annotations sym ext argTys ->
  sym ->
  CLMP.LLVMPtr sym w ->
  Maybe (Some (SomePtrSelector ext argTys))
lookupSomePtrBlockAnnotation anns sym ptr = do
  let block = WI.natToIntegerPure (CLMP.llvmPointerBlock ptr)
  ann <- Maybe.listToMaybe (findAnnotations sym WI.BaseIntegerRepr block)
  -- In this lookup, 'Nothing' may arise if this pointer was created by a
  -- "skip" override (see "Grease.Skip"), because such overrides use the
  -- "setup" machinery, but discard the map that tracks the annotations on
  -- values they create.
  Map.lookup (IntegerAnn ann) (_blockAnns anns)

lookupPtrBlockAnnotation ::
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  CLMP.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrBlockAnnotation anns sym w ptr = do
  Some (SomePtrSelector @_ @_ @_ @ts @regTy sel) <-
    lookupSomePtrBlockAnnotation anns sym ptr
  let cursor = sel ^. selectorPath
  CLMP.LLVMPointerRepr w' <- pure $ PtrCursor.cursorRepr cursor
  Refl <- C.testEquality w w'
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  pure $ SomePtrSelector sel

lookupPtrOffsetAnnotation ::
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  CLMP.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrOffsetAnnotation anns sym w ptr =
  case findAnnotations sym (WI.BaseBVRepr w) (CLMP.llvmPointerOffset ptr) of
    [] -> Nothing
    (ann : _) ->
      -- In this lookup, 'Nothing' may arise if this pointer was created by a
      -- "skip" override (see "Grease.Skip"), because such overrides use the
      -- "setup" machinery, but discard the map that tracks the annotations on
      -- values they create.
      MapF.lookup (BVAnn ann) (_offsetAnns anns)

lookupPtrAnnotation ::
  ( CB.IsSymInterface sym
  , sym ~ WEB.ExprBuilder brand st fs
  , 1 C.<= w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w'
  ) =>
  Annotations sym ext argTys ->
  sym ->
  C.NatRepr w ->
  CLMP.LLVMPtr sym w' ->
  Maybe (SomePtrSelector ext argTys w)
lookupPtrAnnotation anns sym w p = do
  let blockAnn = lookupPtrBlockAnnotation anns sym w p
  let offAnn = lookupPtrOffsetAnnotation anns sym w p
  blockAnn <|> offAnn
