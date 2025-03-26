{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
Module      : Grease.Shape

For an overview of refinement, see "Grease.Refinement".
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grease.Shape
  ( ExtShape
  , PrettyExt
  , Shape(..)
  , ArgShapes(..)
  , argShapes
  , shapeType
  , getTag
  , setTag
  , shapeTag
  , minimalShapeWithPtrs
  , minimalShapeWithPtrs'
  , traverseShapeWithType
  , tagWithType
  ) where

import Control.Applicative (Alternative(empty))
import Control.Exception.Safe (throw, MonadThrow)
import qualified Control.Lens as Lens
import Control.Lens.TH (makeLenses)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import qualified Data.List as List
import Data.Kind (Type)
import Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))

import qualified GHC.Show as GShow
import qualified Text.Show as Show

import qualified Prettyprinter as PP

-- parameterized-utils
import Data.Parameterized.Classes (ShowF(..))
import Data.Parameterized.TraversableFC (traverseFC, fmapFC)
import           Data.Parameterized.Ctx (Ctx)
import qualified Data.Parameterized.Context as Ctx

-- crucible
import qualified Lang.Crucible.CFG.Core as C

-- crucible-llvm
import Lang.Crucible.LLVM.Extension (LLVM)
import qualified Lang.Crucible.LLVM.MemModel as Mem

-- macaw-base
import qualified Data.Macaw.CFG as MC

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

import Grease.Shape.Pointer (PtrShape, minimalPtrShape, ptrShapeType, traversePtrShapeWithType)
import Grease.Utility (GreaseException(..))
import qualified Data.Parameterized.TraversableFC as TFC

data SomeTyped f = forall (t :: C.CrucibleType). SomeTyped (C.TypeRepr t) (f t)
instance ShowF f => Show (SomeTyped f) where

  showsPrec p (SomeTyped repr f) =
    Show.showParen (p > GShow.appPrec) $
      Show.showString "SomeTyped " .
      Show.showsPrec GShow.appPrec1 repr .
      GShow.showSpace .
      showsPrecF GShow.appPrec1 f

-- | The shape of a symbolic value.
--
-- This type is intentionally independent of the syntax extension in use.
--
-- Type parameters:
--
-- * @ext@: Crucible syntax extension (e.g., LLVM or Macaw)
-- * @tag@: Parameterized \"tag\" value for every constructor
-- * @t@: Crucible type of this 'Shape'
--
-- The @tag@ can be used to attach arbitrary (Crucible-type-parameterized) data
-- to each constructor. It is used to attach 'Lang.Crucible.Simulator.RegValue's
-- in "Grease.Setup", and then 'Lang.Crucible.Concretize.ConcRegValue's in
-- "Grease.Concretize".
type Shape :: Type -> (C.CrucibleType -> Type) -> C.CrucibleType -> Type
data Shape ext tag t where
  ShapeBool :: tag C.BoolType -> Shape ext tag C.BoolType
  ShapeExt ::
    ExtShape ext tag t ->
    Shape ext tag t
  ShapeStruct ::
    tag (C.StructType ctx) ->
    Ctx.Assignment (Shape ext tag) ctx ->
    Shape ext tag (C.StructType ctx)
  ShapeUnit ::
    tag C.UnitType ->
    Shape ext tag C.UnitType

-- | Type family for defining extensions to 'Shape' (via 'ShapeExt').
--
-- For documentation on these type parameters, see 'Shape'.
type ExtShape :: Type -> (C.CrucibleType -> Type) -> C.CrucibleType -> Type
type family ExtShape ext

class ShowF (ExtShape ext tag) => ShowExt ext tag
instance ShowF (ExtShape ext tag) => ShowExt ext tag

instance (ShowExt ext tag, ShowF tag) => Show (Shape ext tag t) where
  show = showF

-- | Returns @'Just' 'Refl'@ iff the shapes are identical.
--
-- Disobeys the guidance
--
-- > Typically, only singleton types should inhabit this class.
--
-- on 'TestEquality', because 'Shape' is not a singleton.
instance
  ( TestEquality tag
  , TestEquality (ExtShape ext tag)
  ) => TestEquality (Shape ext tag) where
  testEquality s s' =
    case (s, s') of
      (ShapeUnit tag, ShapeUnit tag') -> testEquality tag tag'
      (ShapeBool tag, ShapeBool tag') -> testEquality tag tag'
      (ShapeStruct tag fields, ShapeStruct tag' fields') ->
        case (testEquality tag tag', testEquality fields fields') of
          (Just Refl, Just Refl) -> Just Refl
          _ -> Nothing
      (ShapeExt ext, ShapeExt ext') -> testEquality ext ext'
      (_, _) -> Nothing

-- | Helper, not exported
showTag :: ShowF tag => tag x -> String
showTag tag = List.concat ["[", showF tag, "]"]

instance (ShowExt ext tag, ShowF tag) => ShowF (Shape ext tag) where
  showF =
    \case
      ShapeBool tag -> "bool" List.++ showTag tag
      ShapeStruct tag fields ->
        List.concat @[]
        [ "("
        , MC.foldlFC (\l s -> showF s List.++ ", " List.++ l) [] fields
        , ")"
        , showTag tag
        ]
      ShapeUnit tag -> "unit" List.++ showTag tag
      ShapeExt ext -> showF ext

class MC.PrettyF (ExtShape ext tag) => PrettyExt ext tag
instance MC.PrettyF (ExtShape ext tag) => PrettyExt ext tag

instance (MC.PrettyF tag, PrettyExt ext tag) => PP.Pretty (Shape ext tag t) where
  pretty = MC.prettyF

-- | Helper, not exported
ppTag :: MC.PrettyF tag => tag x -> PP.Doc ann
ppTag tag = PP.hcat ["[", MC.prettyF tag, "]"]

instance (MC.PrettyF tag, PrettyExt ext tag) => MC.PrettyF (Shape ext tag) where
  prettyF =
    \case
      ShapeBool tag -> "bool" PP.<> ppTag tag
      ShapeStruct tag fields ->
        PP.tupled (MC.foldlFC (\l s -> MC.prettyF s : l) [] fields) PP.<> ppTag tag
      ShapeUnit tag -> "unit" PP.<> ppTag tag
      ShapeExt ext -> MC.prettyF ext

instance TFC.TraversableFC (ExtShape ext) => TFC.FunctorFC (Shape ext) where
  fmapFC = TFC.fmapFCDefault

instance TFC.TraversableFC (ExtShape ext) => TFC.FoldableFC (Shape ext) where
  foldMapFC = TFC.foldMapFCDefault

instance TFC.TraversableFC (ExtShape ext) => TFC.TraversableFC (Shape ext) where
  traverseFC f =
    \case
      ShapeBool tag -> ShapeBool <$> f tag
      ShapeStruct tag fields ->
        ShapeStruct <$> f tag <*> traverseFC (traverseFC f) fields
      ShapeUnit tag -> ShapeUnit <$> f tag
      ShapeExt ext -> ShapeExt <$> traverseFC f ext

traverseShapeWithType ::
  Mem.HasPtrWidth wptr =>
  Applicative m =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  (forall x. C.TypeRepr x -> tag x -> m (tag' x)) ->
  Shape ext tag t ->
  m (Shape ext tag' t)
traverseShapeWithType f =
  \case
    ShapeBool tag -> ShapeBool <$> f C.BoolRepr tag
    ShapeStruct tag fields ->
      let fieldTypes = fmapFC (shapeType ptrShapeType) fields in
      ShapeStruct
      <$> f (C.StructRepr fieldTypes) tag
      <*> traverseFC (traverseShapeWithType f) fields
    ShapeUnit tag -> ShapeUnit <$> f C.UnitRepr tag
    ShapeExt ext -> ShapeExt <$> traversePtrShapeWithType f ext

tagWithType ::
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  Shape ext tag t ->
  Shape ext C.TypeRepr t
tagWithType = runIdentity . traverseShapeWithType (\typeRepr _tag -> Identity typeRepr)

type instance ExtShape (Symbolic.MacawExt arch) = PtrShape (Symbolic.MacawExt arch) (MC.ArchAddrWidth arch)
type instance ExtShape LLVM = PtrShape LLVM 64

getTag ::
  (forall t'. ExtShape ext tag t' -> tag t') ->
  Shape ext tag t ->
  tag t
getTag extTag =
  \case
    ShapeBool tag -> tag
    ShapeStruct tag _fields -> tag
    ShapeUnit tag -> tag
    ShapeExt ext -> extTag ext

setTag ::
  (forall t'. ExtShape ext tag t' -> tag t' -> ExtShape ext tag t') ->
  Shape ext tag t ->
  tag t ->
  Shape ext tag t
setTag extTag shape tag =
  case shape of
    ShapeBool _tag -> ShapeBool tag
    ShapeStruct _tag fields -> ShapeStruct tag fields
    ShapeUnit _tag -> ShapeUnit tag
    ShapeExt ext -> ShapeExt (extTag ext tag)

shapeTag ::
  (forall t'. Lens.Lens' (ExtShape ext tag t') (tag t')) ->
  Lens.Lens' (Shape ext tag t) (tag t)
shapeTag extLens =
  Lens.lens
    (getTag (Lens.view extLens))
    (setTag (flip (Lens.set extLens)))

shapeType ::
  (forall t'. ExtShape ext tag t' -> C.TypeRepr t') ->
  Shape ext tag t ->
  C.TypeRepr t
shapeType extType =
  \case
    ShapeBool{} -> C.BoolRepr
    ShapeStruct _tag fields ->
      C.StructRepr (TFC.fmapFC (shapeType extType) fields)
    ShapeUnit _tag -> C.UnitRepr
    ShapeExt ext -> extType ext

minimalShape ::
  Applicative m =>
  (forall t'. C.TypeRepr t' -> m (ExtShape ext tag t')) ->
  (forall t'. C.TypeRepr t' -> m (tag t')) ->
  C.TypeRepr t ->
  m (Shape ext tag t)
minimalShape ext mkTag =
  \case
    t@C.BoolRepr -> ShapeBool <$> mkTag t
    t@(C.StructRepr fields) ->
      ShapeStruct
      <$> mkTag t
      <*> traverseFC (minimalShape ext mkTag) fields
    t@C.UnitRepr -> ShapeUnit <$> mkTag t
    repr -> ShapeExt <$> ext repr

minimalShapeWithPtrs ::
  forall t m ext tag w.
  ( MonadThrow m
  , ExtShape ext ~ PtrShape ext w
  , Mem.HasPtrWidth w
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  (forall t'. C.TypeRepr t' -> m (tag t')) ->
  C.TypeRepr t ->
  m (Shape ext tag t)
minimalShapeWithPtrs mkTag =
  minimalShape
    (\case
      Mem.LLVMPointerRepr w -> minimalPtrShape mkTag w
      _ -> throw @m $ GreaseException "Could not determine minimal shape for argument")
    mkTag

-- | Like 'minimalShapeWithPtrs', but uses 'empty' instead of 'throw'.
minimalShapeWithPtrs' ::
  forall t m ext tag w.
  ( Alternative m
  , ExtShape ext ~ PtrShape ext w
  , Mem.HasPtrWidth w
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  (forall t'. C.TypeRepr t' -> m (tag t')) ->
  C.TypeRepr t ->
  m (Shape ext tag t)
minimalShapeWithPtrs' mkTag =
  minimalShape
    (\case
      Mem.LLVMPointerRepr w -> minimalPtrShape mkTag w
      _ -> empty)
    mkTag

type ArgShapes :: Type -> (C.CrucibleType -> Type) -> Ctx C.CrucibleType -> Type
newtype ArgShapes ext tag tys = ArgShapes
  { _argShapes :: Ctx.Assignment (Shape ext tag) tys
  }
makeLenses ''ArgShapes

deriving instance (ShowExt ext tag, ShowF tag) => Show (ArgShapes ext tag tys)

instance (MC.PrettyF tag, PrettyExt ext tag) => PP.Pretty (ArgShapes ext tag tys) where
  pretty (ArgShapes regs) =
    MC.foldlFC (\doc rShape -> PP.vcat [doc, PP.pretty rShape]) "" regs
