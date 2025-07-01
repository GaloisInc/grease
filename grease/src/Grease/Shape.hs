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
  minimalShapeWithPtrs,
  minimalShapeWithPtrs',
  traverseShapeWithType,
  tagWithType,
  ParsedShapes (..),
  TypeMismatch (..),
  replaceShapes,
  fromDwarfInfo,
) where

import Control.Applicative (Alternative (empty))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Control.Monad (foldM)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, evalStateT)
import Data.Coerce
import Data.Functor.Const qualified as Const
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Dwarf (CompileUnit (cuRanges, cuSubprograms), Range (rangeBegin, rangeEnd), Subprogram (subParamMap))
import Data.Macaw.Dwarf qualified as MDwarf
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Types qualified as MT
import Data.Map (toAscList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Parameterized.Classes (ShowF (..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Ctx (Ctx)
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import Data.Word (Word64)
import GHC.Show qualified as GShow
import Grease.Macaw.Arch (ArchContext, archABIParams)
import Grease.Macaw.RegName (RegName (..), mkRegName)
import Grease.Options (TypeUnrollingBound (..))
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (MemShape (Exactly, Initialized, Pointer, Uninitialized), Offset (Offset), PtrShape (ShapePtr, ShapePtrBV), PtrTarget, TaggedByte (..), memShapeSize, minimalPtrShape, ptrShapeType, ptrTarget, traversePtrShapeWithType)
import Grease.Utility (GreaseException (..))
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Bytes (toBytes)
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
    ( \case
        Mem.LLVMPointerRepr w -> minimalPtrShape mkTag w
        _ -> throw @m $ GreaseException "Could not determine minimal shape for argument"
    )
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
    ( \case
        Mem.LLVMPointerRepr w -> minimalPtrShape mkTag w
        _ -> empty
    )
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

-- * Replacing

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

isInSubProg ::
  -- | the program counter from the base of the target ELF
  Word64 ->
  Subprogram ->
  Bool
isInSubProg w sub =
  let entryMatch = (==) w <$> MDwarf.subEntryPC sub
      def = fromMaybe False
   in def
        ( do
            sdef <- MDwarf.subDef sub
            lpc <- MDwarf.subLowPC sdef
            hpc <- MDwarf.subHighPC sdef
            pure $ w >= lpc && w < (lpc + hpc)
        )
        || def entryMatch

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

extractType :: Subprogram -> MDwarf.TypeRef -> Maybe MDwarf.TypeApp
extractType sprog vrTy =
  do
    let mp = MDwarf.subTypeMap sprog
    (mbtypeApp, _) <- Map.lookup vrTy mp
    rightToMaybe mbtypeApp

constructPtrTarget ::
  HasPtrWidth w =>
  TypeUnrollingBound ->
  Subprogram ->
  MDwarf.TypeApp ->
  StateT VisitState Maybe (PtrTarget w NoTag)
constructPtrTarget tyUnrollBound sprog tyApp =
  ptrTarget Nothing <$> shapeSeq tyApp
 where
  ishape w = lift $ Just $ Seq.singleton $ Initialized NoTag (toBytes w)
  padding :: (Integral a) => a -> MemShape w NoTag
  padding w = Uninitialized (toBytes w)
  -- if we dont know where to place members we have to fail/just place some bytes
  buildMember :: HasPtrWidth w => Word64 -> MDwarf.Member -> StateT VisitState Maybe (Word64, Seq.Seq (MemShape w NoTag))
  buildMember loc member =
    do
      memLoc <- lift $ MDwarf.memberLoc member
      padd <- lift $ Just (if memLoc == loc then Seq.empty else Seq.singleton (padding $ memLoc - loc))
      memberShape <- shapeOfTyApp $ MDwarf.memberType member
      let memByteSize = sum $ memShapeSize ?ptrWidth <$> memberShape
      let nextLoc = memLoc + fromIntegral memByteSize
      lift $ Just (nextLoc, padd Seq.>< memberShape)

  shapeOfTyApp :: (HasPtrWidth w) => MDwarf.TypeRef -> StateT VisitState Maybe (Seq.Seq (MemShape w NoTag))
  shapeOfTyApp x = shapeSeq =<< lift . extractType sprog =<< pure x

  shapeSeq :: (HasPtrWidth w) => MDwarf.TypeApp -> StateT VisitState Maybe (Seq.Seq (MemShape w NoTag))
  shapeSeq (MDwarf.UnsignedIntType w) = ishape w
  shapeSeq (MDwarf.SignedIntType w) = ishape w
  shapeSeq MDwarf.SignedCharType = ishape (1 :: Int)
  shapeSeq MDwarf.UnsignedCharType = ishape (1 :: Int)
  -- Need modification to DWARF to collect DW_TAG_count and properly evaluate dwarf ops for upper bounds in subranges
  -- https://github.com/GaloisInc/grease/issues/263
  shapeSeq (MDwarf.ArrayType _elTy _ub) = lift $ Nothing
  -- need to compute padding between elements, in-front of the first element, and at end
  shapeSeq (MDwarf.StructType sdecl) =
    let structSize = MDwarf.structByteSize sdecl
     in do
          (endLoc, seqs) <-
            foldM
              ( \(currLoc, seqs) mem ->
                  do
                    (nextLoc, additionalShapes) <- buildMember currLoc mem
                    lift $ Just (nextLoc, seqs Seq.>< additionalShapes)
              )
              (0, Seq.empty)
              (MDwarf.structMembers sdecl)
          let endPad = if endLoc >= structSize then Seq.empty else Seq.singleton (padding $ structSize - endLoc)
          lift $ Just $ (seqs Seq.>< endPad)
  shapeSeq (MDwarf.PointerType _ maybeRef) =
    let mshape = constructPtrMemShapeFromRef tyUnrollBound sprog =<< lift maybeRef
     in Seq.singleton <$> mshape
  shapeSeq _ = lift $ Nothing

type VisitState = Map.Map MDwarf.TypeRef Int

nullPtr :: HasPtrWidth w => (MemShape w NoTag)
nullPtr =
  let zbyt = TaggedByte{taggedByteTag = NoTag, taggedByteValue = 0}
   in Exactly (take (fromInteger $ CT.intValue ?ptrWidth `div` 8) $ repeat $ zbyt)

constructPtrMemShapeFromRef :: HasPtrWidth w => TypeUnrollingBound -> Subprogram -> MDwarf.TypeRef -> StateT VisitState Maybe (MemShape w NoTag)
constructPtrMemShapeFromRef tyUnrollBound sprog ref =
  do
    mp <- get
    let ct = fromMaybe 0 $ Map.lookup ref mp
    _ <- put (Map.insert ref (ct + 1) mp)
    if ct > coerce tyUnrollBound
      then
        pure $ nullPtr
      else
        let memShape = constructPtrTarget tyUnrollBound sprog =<< (lift $ extractType sprog ref)
         in (\x -> pure $ Pointer NoTag (Offset 0) x) =<< memShape

intPtrShape ::
  Symbolic.SymArchConstraints arch =>
  MC.ArchReg arch tp ->
  Maybe (C.Some (PtrShape ext w NoTag))
intPtrShape reg =
  case MT.typeRepr reg of
    MT.BVTypeRepr w -> Just $ C.Some $ ShapePtrBV NoTag w
    _ -> Nothing

pointerShapeOfDwarf ::
  (HasPtrWidth w, Symbolic.SymArchConstraints arch) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.TypeApp ->
  Maybe (C.Some (PtrShape ext w NoTag))
pointerShapeOfDwarf _ _ r _ (MDwarf.SignedIntType _) = intPtrShape r
pointerShapeOfDwarf _ _ r _ (MDwarf.UnsignedIntType _) = intPtrShape r
pointerShapeOfDwarf _ tyUnrollBound _ sprog (MDwarf.PointerType _ tyRef) =
  let memShape = evalStateT (constructPtrTarget tyUnrollBound sprog =<< (lift $ (extractType sprog =<< tyRef))) Map.empty
      pointerShape = ShapePtr NoTag (Offset 0) <$> memShape
   in (C.Some <$> pointerShape)
pointerShapeOfDwarf _ _ _ _ _ = Nothing

-- Stops after the first nothing to avoid adding shapes after
-- failing to build some shape (this would result in shapes applying to incorrect registers)
takeJust :: (a -> Maybe b) -> [a] -> [b]
takeJust _ [] = []
takeJust f (h : tl) =
  case f h of
    Nothing -> []
    Just e -> e : takeJust f tl

shapeFromVar ::
  ( ExtShape ext ~ PtrShape ext wptr
  , Mem.HasPtrWidth wptr
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.Variable ->
  Maybe (C.Some (Shape ext NoTag))
shapeFromVar arch tyUnrollBound buildingForReg sprog vr =
  C.mapSome ShapeExt
    <$> ( pointerShapeOfDwarf arch tyUnrollBound buildingForReg sprog
            =<< extractType sprog
            =<< MDwarf.varType vr
        )

shapeFromDwarf :: (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, Mem.HasPtrWidth wptr) => ArchContext arch -> TypeUnrollingBound -> Subprogram -> ParsedShapes ext
shapeFromDwarf aContext tyUnrollBound sub =
  let
    abiRegs = aContext Lens.^. archABIParams
    args = (zip abiRegs $ snd <$> (toAscList $ subParamMap sub))
    regAssignmentFromDwarfVar reg var =
      do
        C.Some r <- pure reg
        shp <- shapeFromVar aContext tyUnrollBound r sub var
        pure (Text.pack $ coerce $ mkRegName r, shp)
    ascParams =
      takeJust
        (uncurry regAssignmentFromDwarfVar)
        args
   in
    ParsedShapes{_getParsedShapes = Map.fromList ascParams}

-- | Given a list of `Data.Macaw.Dwarf.CompileUnit` attempts to find a subprogram corresponding to
-- the provided PC and synthesize a shape from the DWARF provided prototype for the function.
-- The provided PC is relative to the base of the image (as is represented in DWARF).
fromDwarfInfo ::
  (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, Mem.HasPtrWidth wptr) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  -- | The entrypoint PC of the target subprogram relative to the target ELF object.
  Word64 ->
  [Data.Macaw.Dwarf.CompileUnit] ->
  Maybe (ParsedShapes ext)
fromDwarfInfo aContext tyUnrollBound addr cus =
  do
    targetCu <-
      List.find
        ( \x ->
            let rs = cuRanges x
             in let isInCU =
                      any
                        ( \range ->
                            let begin = rangeBegin range
                                end = rangeEnd range
                             in begin <= addr && addr < end
                        )
                        rs
                 in isInCU
        )
        cus
    targetSubProg <- List.find (isInSubProg addr) (cuSubprograms targetCu)
    pure $
      shapeFromDwarf
        aContext
        tyUnrollBound
        targetSubProg
