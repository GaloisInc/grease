{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Syntax.Overrides (
  checkTypedOverrideHandleCompat,
  tryBindTypedOverride,
  freshBytesOverride,
  concBvOverride,
  tryConcBvOverride,
) where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class (liftIO)
import Data.BitVector.Sized qualified as BV
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Type.Ord (type (<=))
import Data.Vector qualified as Vec
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.FunctionHandle qualified as LCF
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as LCT
import What4.Concretize qualified as WC
import What4.Interface qualified as WI

-- | Check if a 'CS.TypedOverride' is compatible with a 'LCF.FnHandle'
checkTypedOverrideHandleCompat ::
  LCF.FnHandle args ret ->
  CS.TypedOverride p sym ext args' ret' ->
  Maybe (args :~: args', ret :~: ret')
checkTypedOverrideHandleCompat hdl ov = do
  rArgs <- testEquality (LCF.handleArgTypes hdl) (CS.typedOverrideArgs ov)
  rRet <- testEquality (LCF.handleReturnType hdl) (CS.typedOverrideRet ov)
  pure (rArgs, rRet)

-- | The return value indicates whether the override was bound.
tryBindTypedOverride ::
  LCF.FnHandle args ret ->
  CS.TypedOverride p sym ext args' ret' ->
  CS.OverrideSim p sym ext rtp args'' ret'' Bool
tryBindTypedOverride hdl ov =
  case checkTypedOverrideHandleCompat hdl ov of
    Nothing -> pure False
    Just (Refl, Refl) -> do
      CS.bindTypedOverride hdl ov
      pure True

---------------------------------------------------------------------

-- | Override for @fresh-bytes@.
--
-- The name must be concrete. If a symbolic name is passed this function will
-- generate an assertion failure.
--
-- The number of bytes must be concrete. If a symbolic number is passed this
-- function will generate an assertion failure.
freshBytesOverride ::
  ( 1 <= w
  , ToConc.HasToConcretize p
  , CB.IsSymInterface sym
  ) =>
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.StringType WI.Unicode Ctx.::> LCT.BVType w) (LCT.VectorType (LCT.BVType 8))
freshBytesOverride w =
  WI.withKnownNat w $
    CS.typedOverride (Ctx.uncurryAssignment freshBytes)

-- | Implementation of @fresh-bytes@ override.
--
-- The name must be concrete. If a symbolic name is passed this function will
-- generate an assertion failure.
--
-- The number of bytes must be concrete. If a symbolic number is passed this
-- function will generate an assertion failure.
freshBytes ::
  ( 1 <= w
  , ToConc.HasToConcretize p
  , CB.IsSymInterface sym
  ) =>
  CS.RegValue' sym (LCT.StringType WI.Unicode) ->
  CS.RegValue' sym (LCT.BVType w) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.VectorType (LCT.BVType 8)))
freshBytes name0 bv0 =
  case WI.asString (CS.unRV name0) of
    Nothing ->
      CS.overrideError $
        CS.AssertFailureSimError "Call to @fresh-bytes with symbolic name" ""
    Just (WI.UnicodeLiteral name) ->
      case WI.asBV (CS.unRV bv0) of
        Nothing ->
          CS.overrideError $
            CS.AssertFailureSimError "Call to @fresh-bytes with symbolic length" ""
        Just bv -> doFreshBytes name (BV.asUnsigned bv)

doFreshBytes ::
  ( ToConc.HasToConcretize p
  , CB.IsSymInterface sym
  ) =>
  Text ->
  Integer ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.VectorType (LCT.BVType 8)))
doFreshBytes name len =
  CS.ovrWithBackend $ \bak -> do
    let sym = CB.backendGetSym bak
    v <-
      fmap Vec.fromList $
        liftIO $
          Monad.forM [0 .. len - 1] $ \i -> do
            let nm = WI.safeSymbol (Text.unpack name ++ "_" ++ show i)
            WI.freshConstant sym nm (WI.BaseBVRepr (NatRepr.knownNat @8))

    let ty = LCT.VectorRepr (LCT.BVRepr (NatRepr.knownNat @8))
    let entry = CS.RegEntry ty v
    ToConc.addToConcretize name entry

    pure v

---------------------------------------------------------------------

-- | See if @conc-bv-*@ is compatible with the given handle.
tryConcBvOverride ::
  forall p sym ext bak scope st fs solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcBvOverride bak w hdl = do
  let ov = concBvOverride @p @ext bak w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-bv-*@.
--
-- The number of bytes must be concrete. If a symbolic number is passed this
-- function will generate an assertion failure.
concBvOverride ::
  forall p ext sym bak scope st fs solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.BVType w) (LCT.BVType w)
concBvOverride bak w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (concBv bak)))

-- | Implementation of @conc-bv-*@ override.
concBv ::
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  CS.RegValue' sym (LCT.BVType w) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.BVType w))
concBv bak (CS.RV bv) = do
  let w = WI.bvWidth bv
  mb <- liftIO (Conc.concRegValue bak MapF.empty (LCT.BVRepr w) bv)
  case mb of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-bv-*: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-bv-*: unsat initial assumptions")
    Right bvLit ->
      liftIO (WI.bvLit (CB.backendGetSym bak) w bvLit)
