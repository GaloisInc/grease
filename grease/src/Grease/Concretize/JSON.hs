{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Grease.Concretize.JSON
  ( IntrinsicJsonFn(..)
  , jsonPtrFn
  , jsonPtrFnMap
  , concRegValueToJson
  , concArgsToJson
  ) where

import Data.Aeson qualified as Aeson
import Data.BitVector.Sized qualified as BV
import Data.List qualified as List
import           Data.Functor.Product (Product(Pair))
import           Data.Kind (Type)
import Data.Text.Encoding qualified as Text
import LibBF qualified as LibBF

import Data.Parameterized.Context qualified as Ctx
import           Data.Parameterized.Map (MapF)
import Data.Parameterized.Map qualified as MapF
import           Data.Parameterized.TraversableFC (FoldableFC(toListFC))
import           Data.Parameterized.SymbolRepr (SymbolRepr)

import What4.Expr.Builder qualified as W4
import           What4.FloatMode (FloatModeRepr)
import What4.FloatMode qualified as W4FM
import What4.Utils.Complex qualified as W4
import What4.Utils.StringLiteral qualified as W4SL
import What4.Utils.Word16String qualified as W4W16

import           Lang.Crucible.Concretize (ConcRV')
import           Lang.Crucible.Types (TypeRepr)
import Lang.Crucible.Types qualified as C
import Lang.Crucible.Concretize qualified as Conc

import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem

import Data.Macaw.Symbolic qualified as Symbolic

import Grease.Concretize (ConcArgs(..))
import Grease.Macaw.RegName qualified as RN
import Grease.Panic (panic)
import Grease.Shape (ExtShape, getTag)
import Grease.Shape.Pointer (PtrShape, getPtrTag)

-- | Function for serializing a concretized intrinsic type
type IntrinsicJsonFn :: Type -> C.Symbol -> Type
newtype IntrinsicJsonFn t nm
  = IntrinsicJsonFn
    (forall ctx.
      Ctx.Assignment TypeRepr ctx ->
      Conc.ConcIntrinsic nm ctx ->
      Aeson.Value)

-- | Helper, not exported
tryConcIntrinsic ::
  forall nm ctx t.
  MapF SymbolRepr (IntrinsicJsonFn t) ->
  SymbolRepr nm ->
  Ctx.Assignment TypeRepr ctx ->
  Conc.ConcIntrinsic nm ctx ->
  Maybe Aeson.Value
tryConcIntrinsic fns nm tyCtx v =
  case MapF.lookup nm fns of
    Nothing -> Nothing
    Just (IntrinsicJsonFn f) -> Just (f @ctx tyCtx v)

-- | An 'IntrinsicJsonFn' for LLVM pointers
jsonPtrFn :: IntrinsicJsonFn t "LLVM_pointer"
jsonPtrFn = IntrinsicJsonFn $ \tyCtx ptr ->
  case Ctx.viewAssign tyCtx of
    Ctx.AssignExtend (Ctx.viewAssign -> Ctx.AssignEmpty) (C.BVRepr _) -> do
      case ptr of
        Mem.ConcLLVMPtr blk off _w ->
          Aeson.object ["block" Aeson..= blk, "offset" Aeson..= BV.asUnsigned off]
    -- These are impossible by the definition of LLVMPointerImpl
    Ctx.AssignEmpty ->
       panic "jsonPtrFn"
         [ "Impossible: LLVMPointerType empty context" ]
    Ctx.AssignExtend _ _ ->
       panic "jsonPtrFn"
         [ "Impossible: LLVMPointerType ill-formed context" ]

-- | A singleton map suitable when LLVM pointers are the only intrinsic type
-- in use
jsonPtrFnMap :: MapF.MapF SymbolRepr (IntrinsicJsonFn t)
jsonPtrFnMap = MapF.singleton (C.knownSymbol @"LLVM_pointer") jsonPtrFn

bvToJson :: BV.BV w -> Aeson.Value
bvToJson = Aeson.toJSON . BV.asUnsigned

-- | Rounding mode to use for enoding 'LibBF.BigFloat's into 'Aeson.Value's.
--
-- The implications of this particular choice are not clear to me, but other
-- Galois tools use 'LibBF.NearEven':
-- https://github.com/GaloisInc/cryptol/blob/0ab12fa1cc307ff11360af57641b92507bdd6640/src/Cryptol/Eval/FFI.hs#L383
--
-- Not exported.
roundingMode :: LibBF.RoundMode
roundingMode = LibBF.NearEven

bigFloatToJson :: LibBF.BigFloat -> Maybe Aeson.Value
bigFloatToJson bf =
  case LibBF.bfToDouble roundingMode bf of
    (d, LibBF.Ok) -> Just (Aeson.toJSON d)
    _ -> Nothing

-- | Helper, not exported
anyToJson ::
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  MapF SymbolRepr (IntrinsicJsonFn t) ->
  FloatModeRepr fm ->
  Conc.ConcAnyValue sym ->
  Maybe Aeson.Value
anyToJson iFns fm (Conc.ConcAnyValue tp v) =
  concRegValueToJson iFns fm tp v

-- | Helper, not exported
maybeToJson ::
  forall sym scope st fm t tp.
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  MapF SymbolRepr (IntrinsicJsonFn t) ->
  FloatModeRepr fm ->
  C.TypeRepr tp ->
  Conc.ConcRV' sym (C.MaybeType tp) ->
  Maybe Aeson.Value
maybeToJson iFns fm tp (Conc.ConcRV' v) = do
  v' <- v
  concRegValueToJson iFns fm tp (Conc.ConcRV' @sym v')

-- | Helper, not exported
structToJson ::
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  MapF SymbolRepr (IntrinsicJsonFn t) ->
  FloatModeRepr fm ->
  C.CtxRepr tps ->
  Conc.ConcRV' sym (C.StructType tps) ->
  Maybe Aeson.Value
structToJson iFns fm tps (Conc.ConcRV' val) =
  let pairs = Ctx.zipWith Pair tps val
  in Aeson.toJSON <$>
       sequence (toListFC (\(Pair t v) -> concRegValueToJson iFns fm t v) pairs)

concRegValueToJson ::
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  MapF SymbolRepr (IntrinsicJsonFn t) ->
  FloatModeRepr fm ->
  TypeRepr tp ->
  ConcRV' sym tp ->
  Maybe Aeson.Value
concRegValueToJson iFns fm tp val'@(Conc.ConcRV' val) =
  case tp of
    C.BoolRepr -> Just (Aeson.toJSON val)
    C.BVRepr {} -> Just (bvToJson val)
    C.ComplexRealRepr ->
      Just (Aeson.object ["real" Aeson..= W4.realPart val, "imag" Aeson..= W4.imagPart val])
    C.FloatRepr {} ->
      case fm of
        W4FM.FloatIEEERepr -> bigFloatToJson val
        W4FM.FloatUninterpretedRepr -> Just (bvToJson val)
        W4FM.FloatRealRepr -> Just (Aeson.toJSON val)
    C.IEEEFloatRepr _ -> bigFloatToJson val
    C.IntegerRepr -> Just (Aeson.toJSON val)
    C.NatRepr -> Just (Aeson.toJSON val)
    C.RealValRepr -> Just (Aeson.toJSON val)
    C.UnitRepr -> Just (Aeson.toJSON val)
    C.CharRepr -> Just (Aeson.toJSON val)
    C.IntrinsicRepr nm ctx -> tryConcIntrinsic iFns nm ctx val
    C.AnyRepr {} -> anyToJson iFns fm val
    C.MaybeRepr tp' -> maybeToJson iFns fm tp' val'
    C.StructRepr tps -> structToJson iFns fm tps val'
    C.StringRepr {} ->
      Just $
        case val of
          W4SL.UnicodeLiteral s ->
            Aeson.toJSON s
          W4SL.Char8Literal s ->
            Aeson.toJSON (Text.decodeUtf8 s)
          W4SL.Char16Literal s ->
            Aeson.toJSON (Text.decodeUtf16LE (W4W16.toLEByteString s))

    -- TODO
    C.FunctionHandleRepr {} -> Nothing
    C.RecursiveRepr {} -> Nothing
    C.ReferenceRepr {} -> Nothing
    C.SequenceRepr {} -> Nothing
    C.StringMapRepr {} -> Nothing
    C.SymbolicArrayRepr {} -> Nothing
    C.SymbolicStructRepr {} -> Nothing
    C.VariantRepr {} -> Nothing
    C.VectorRepr {} -> Nothing
    C.WordMapRepr {} -> Nothing

concArgsToJson ::
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  FloatModeRepr fm ->
  RN.RegNames arch ->
  ConcArgs sym ext (Symbolic.MacawCrucibleRegTypes arch) ->
  Ctx.Assignment C.TypeRepr (Symbolic.MacawCrucibleRegTypes arch) ->
  [Aeson.Value]
concArgsToJson fm regNames (ConcArgs cArgs) argTys =
  let argsWithTypes = Ctx.zipWith (\argTy cArg -> Pair argTy (getTag getPtrTag cArg)) argTys cArgs in
  let argBlobs = toListFC (\(Pair ty cVal) -> concRegValueToJson jsonPtrFnMap fm ty cVal) argsWithTypes in
  let regNames' = List.map RN.regNameToString (RN.regNamesToList regNames) in
  List.zipWith (\name mval -> Aeson.object ["reg" Aeson..= name, "value" Aeson..= mval]) regNames' argBlobs
