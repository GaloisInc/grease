{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
-- Module      : Grease.Shape
--
-- For an overview of refinement, see "Grease.Refinement".
module Grease.Shape (
  ExtShape,
  PrettyExt,
  Shape (..),
  ArgShapes (..),
  argShapes,
  shapeType,
  getTag,
  setTag,
  shapeTag,
  MinimalShapeError (..),
  minimalShapeWithPtrs,
  traverseShapeWithType,
  tagWithType,

  -- * Replacing
  ParsedShapes (..),
  TypeMismatch (..),
  replaceShapes,

  -- * JSON
  parseJsonShape,
  parseJsonShapeWithPtrs,
  parseJsonShapes,
) where

import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.Functor.Const qualified as Const
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Parameterized.Classes (ShowF (..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Ctx (Ctx)
import Data.Parameterized.Some (Some (Some))
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Traversable qualified as Traversable
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import GHC.Show qualified as GShow
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrShape, minimalPtrShape, parseJsonPtrShape, ptrShapeType, traversePtrShapeWithType)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.MemModel (HasPtrWidth)
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Types qualified as CT
import Prettyprinter qualified as PP
import Text.Show qualified as Show

data SomeTyped f = forall (t :: C.CrucibleType). SomeTyped (C.TypeRepr t) (f t)
instance ShowF f => Show (SomeTyped f) where
  showsPrec p (SomeTyped repr f) =
    Show.showParen (p > GShow.appPrec) $
      Show.showString "SomeTyped "
        . Show.showsPrec GShow.appPrec1 repr
        . GShow.showSpace
        . showsPrecF GShow.appPrec1 f

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
  ShapeFloat ::
    tag (C.FloatType fi) -> C.FloatInfoRepr fi -> Shape ext tag (C.FloatType fi)
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
  ) =>
  TestEquality (Shape ext tag)
  where
  testEquality =
    \cases
      (ShapeUnit tag) (ShapeUnit tag') -> testEquality tag tag'
      (ShapeBool tag) (ShapeBool tag') -> testEquality tag tag'
      (ShapeStruct tag fields) (ShapeStruct tag' fields') ->
        case (testEquality tag tag', testEquality fields fields') of
          (Just Refl, Just Refl) -> Just Refl
          _ -> Nothing
      (ShapeExt ext) (ShapeExt ext') -> testEquality ext ext'
      _ _ -> Nothing

-- | Helper, not exported
showTag :: ShowF tag => tag x -> String
showTag tag = List.concat ["[", showF tag, "]"]

-- | Intended for debugging
instance (ShowExt ext tag, ShowF tag) => ShowF (Shape ext tag) where
  showF =
    \case
      ShapeBool tag -> "bool" List.++ showTag tag
      ShapeFloat tag f -> show f List.++ showTag tag
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
      ShapeFloat tag f -> PP.pretty f PP.<> ppTag tag
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
      ShapeFloat tag fi -> ShapeFloat <$> f tag <*> pure fi
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
    ShapeFloat tag fi -> ShapeFloat <$> f (C.FloatRepr fi) tag <*> pure fi
    ShapeStruct tag fields ->
      let fieldTypes = fmapFC (shapeType ptrShapeType) fields
       in ShapeStruct
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
    ShapeFloat tag _fi -> tag
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
    ShapeFloat _tag fi -> ShapeFloat tag fi
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
    ShapeFloat _tag fi -> C.FloatRepr fi
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
    t@(C.FloatRepr fi) -> ShapeFloat <$> mkTag t <*> pure fi
    t@(C.StructRepr fields) ->
      ShapeStruct
        <$> mkTag t
        <*> traverseFC (minimalShape ext mkTag) fields
    t@C.UnitRepr -> ShapeUnit <$> mkTag t
    repr -> ShapeExt <$> ext repr

data MinimalShapeError = MinimalShapeError (Some C.TypeRepr)

instance PP.Pretty MinimalShapeError where
  pretty (MinimalShapeError (Some t)) =
    "Can't make minimal shape for type " <> PP.viaShow t

minimalShapeWithPtrs ::
  forall t ext tag w.
  ( ExtShape ext ~ PtrShape ext w
  , Mem.HasPtrWidth w
  , Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8)))
  ) =>
  (forall t'. C.TypeRepr t' -> tag t') ->
  C.TypeRepr t ->
  Either MinimalShapeError (Shape ext tag t)
minimalShapeWithPtrs mkTag =
  minimalShape
    ( \case
        Mem.LLVMPointerRepr w -> minimalPtrShape (pure . mkTag) w
        t -> Left (MinimalShapeError (Some t))
    )
    (pure . mkTag)

type ArgShapes :: Type -> (C.CrucibleType -> Type) -> Ctx C.CrucibleType -> Type
newtype ArgShapes ext tag tys = ArgShapes
  { _argShapes :: Ctx.Assignment (Shape ext tag) tys
  }
makeLenses ''ArgShapes

deriving instance (ShowExt ext tag, ShowF tag) => Show (ArgShapes ext tag tys)

instance (MC.PrettyF tag, PrettyExt ext tag) => PP.Pretty (ArgShapes ext tag tys) where
  pretty (ArgShapes regs) =
    MC.foldlFC (\doc rShape -> PP.vcat [doc, PP.pretty rShape]) "" regs

data TypeMismatch
  = TypeMismatch
  { typeMismatchName :: String
  , expectedType :: C.Some CT.TypeRepr
  , foundType :: C.Some CT.TypeRepr
  }
  deriving Show

instance PP.Pretty TypeMismatch where
  pretty tm =
    PP.hsep
      [ "Type mismatch for"
      , PP.pretty (typeMismatchName tm) PP.<> ":"
      , "expected:"
      , PP.viaShow (expectedType tm)
      , "but found:"
      , PP.viaShow (foundType tm)
      ]

-- | A mapping from argument name to the shape for that argument. Intended to be used with
-- 'replaceShapes' to initialize members of an initial 'ArgShape' with user or elsewhere defined shapes.
newtype ParsedShapes ext
  = ParsedShapes {_getParsedShapes :: Map.Map Text (C.Some (Shape ext NoTag))}

-- | Given an initial, provisional list of arguments and a set of replacements
-- for some of them, calculate a new list of arguments.
replaceShapes ::
  forall ext w tys.
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  ArgShapes ext NoTag tys ->
  -- | Replacement arguments
  ParsedShapes ext ->
  Either TypeMismatch (ArgShapes ext NoTag tys)
replaceShapes names (ArgShapes args) (ParsedShapes replacements) =
  -- TODO: Check that all the map keys are expected
  ArgShapes
    <$> Ctx.zipWithM (\(Const.Const nm) s -> replaceOne nm s) names args
 where
  replaceOne :: String -> Shape ext NoTag t -> Either TypeMismatch (Shape ext NoTag t)
  replaceOne nm s =
    case Map.lookup (Text.pack nm) replacements of
      Just (C.Some replace) ->
        let ty = shapeType ptrShapeType s
         in let ty' = shapeType ptrShapeType replace
             in case testEquality ty ty' of
                  Just Refl -> Right replace
                  Nothing ->
                    Left $
                      TypeMismatch
                        { typeMismatchName = nm
                        , expectedType = C.Some ty
                        , foundType = C.Some ty'
                        }
      Nothing -> Right s

-- | Given a parser for @tag@s and 'ShapeExt's, parse 'Shape's from JSON
parseJsonShape ::
  -- | Parser for @tag@s
  (forall t. Aeson.KeyMap Aeson.Value -> Aeson.Parser (tag t)) ->
  -- | Parser for 'ExtShape's
  (Aeson.Value -> Aeson.Parser (Some (ExtShape ext tag))) ->
  -- | JSON value to parse
  Aeson.Value ->
  Aeson.Parser (Some (Shape ext tag))
parseJsonShape parseTag parseExt =
  Aeson.withObject "Shape" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "bool" -> Some . ShapeBool <$> parseTag v
      "ext" -> doParseExt v
      "float" -> parseFloat v
      "struct" -> parseStruct v
      "unit" -> Some . ShapeUnit <$> parseTag v
      _ -> fail ("Unknown Shape type: " ++ Text.unpack ty)
 where
  doParseExt v = do
    Some s <- parseExt =<< v .: "val"
    pure (Some (ShapeExt s))

  parseFloat v = do
    Some fi <- parseFloatRepr =<< v .: "info"
    tag <- parseTag v
    pure (Some (ShapeFloat tag fi))
   where
    parseFloatRepr =
      Aeson.withText "info" $
        \case
          "half" -> pure (Some CT.HalfFloatRepr)
          "single" -> pure (Some CT.SingleFloatRepr)
          "double" -> pure (Some CT.DoubleFloatRepr)
          "quad" -> pure (Some CT.QuadFloatRepr)
          "x86_80" -> pure (Some CT.X86_80FloatRepr)
          "double double" -> pure (Some CT.DoubleDoubleFloatRepr)
          t -> fail ("Unexpected float info: " ++ Text.unpack t)

  parseStruct v = do
    fields <- traverse (parseJsonShape parseTag parseExt) =<< v .: "fields"
    Some fields' <- pure (Ctx.fromList fields)
    tag <- parseTag v
    pure (Some (ShapeStruct tag fields'))

-- | Given a parser for @tag@s, parse 'Shape's containing 'PtrShape's from JSON
parseJsonShapeWithPtrs ::
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Parser for @tag@s
  (forall t. Aeson.KeyMap Aeson.Value -> Aeson.Parser (tag t)) ->
  -- | JSON value to parse
  Aeson.Value ->
  Aeson.Parser (Some (Shape ext tag))
parseJsonShapeWithPtrs parseTag =
  parseJsonShape parseTag (parseJsonPtrShape parseTag)

-- | Parse a series of 'Shape's containing 'PtrShape's from JSON
parseJsonShapes ::
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  -- | Path to file containing JSON, used in error messages
  FilePath ->
  -- | JSON blob as 'Text'
  Text ->
  Either String (ParsedShapes ext)
parseJsonShapes path txt = do
  obj <-
    mapLeft (("In file " ++ path ++ ": ") ++) $
      Aeson.eitherDecode @Aeson.Object (BS.fromStrict (Text.encodeUtf8 txt))
  fmap (ParsedShapes . Map.fromList) $
    Traversable.for (Aeson.toList obj) $ \(k, v) -> do
      case Aeson.parse (parseJsonShapeWithPtrs (const (pure NoTag))) v of
        Aeson.Error e -> Left e
        Aeson.Success s -> Right (Aeson.toText k, s)
 where
  mapLeft f (Left l) = Left (f l)
  mapLeft _ r = r
