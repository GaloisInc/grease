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
) where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class (liftIO)
import Data.BitVector.Sized qualified as BV
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Type.Ord (type (<=))
import Data.Vector qualified as Vec
import Grease.Concretize.ToConcretize qualified as ToConc
import Lang.Crucible.Backend qualified as LCB
import Lang.Crucible.FunctionHandle qualified as LCF
import Lang.Crucible.Simulator qualified as LCS
import Lang.Crucible.Types qualified as LCT
import What4.Interface qualified as WI

-- | Check if a 'LCS.TypedOverride' is compatible with a 'LCF.FnHandle'
checkTypedOverrideHandleCompat ::
  LCF.FnHandle args ret ->
  LCS.TypedOverride p sym ext args' ret' ->
  Maybe (args :~: args', ret :~: ret')
checkTypedOverrideHandleCompat hdl ov = do
  rArgs <- testEquality (LCF.handleArgTypes hdl) (LCS.typedOverrideArgs ov)
  rRet <- testEquality (LCF.handleReturnType hdl) (LCS.typedOverrideRet ov)
  pure (rArgs, rRet)

-- | The return value indicates whether the override was bound.
tryBindTypedOverride ::
  LCF.FnHandle args ret ->
  LCS.TypedOverride p sym ext args' ret' ->
  LCS.OverrideSim p sym ext rtp args'' ret'' Bool
tryBindTypedOverride hdl ov =
  case checkTypedOverrideHandleCompat hdl ov of
    Nothing -> pure False
    Just (Refl, Refl) -> do
      LCS.bindTypedOverride hdl ov
      pure True

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
  , LCB.IsSymInterface sym
  ) =>
  NatRepr.NatRepr w ->
  LCS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.StringType WI.Unicode Ctx.::> LCT.BVType w) (LCT.VectorType (LCT.BVType 8))
freshBytesOverride w =
  WI.withKnownNat w $
    LCS.typedOverride (Ctx.uncurryAssignment freshBytes)

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
  , LCB.IsSymInterface sym
  ) =>
  LCS.RegValue' sym (LCT.StringType WI.Unicode) ->
  LCS.RegValue' sym (LCT.BVType w) ->
  LCS.OverrideSim p sym ext r args ret (LCS.RegValue sym (LCT.VectorType (LCT.BVType 8)))
freshBytes name0 bv0 =
  case WI.asString (LCS.unRV name0) of
    Nothing ->
      LCS.overrideError $
        LCS.AssertFailureSimError "Call to @fresh-bytes with symbolic name" ""
    Just (WI.UnicodeLiteral name) ->
      case WI.asBV (LCS.unRV bv0) of
        Nothing ->
          LCS.overrideError $
            LCS.AssertFailureSimError "Call to @fresh-bytes with symbolic length" ""
        Just bv -> doFreshBytes name (BV.asUnsigned bv)

doFreshBytes ::
  ( ToConc.HasToConcretize p
  , LCB.IsSymInterface sym
  ) =>
  Text ->
  Integer ->
  LCS.OverrideSim p sym ext r args ret (LCS.RegValue sym (LCT.VectorType (LCT.BVType 8)))
doFreshBytes name len =
  LCS.ovrWithBackend $ \bak -> do
    let sym = LCB.backendGetSym bak
    v <-
      fmap Vec.fromList $
        liftIO $
          Monad.forM [0 .. len - 1] $ \i -> do
            let nm = WI.safeSymbol (Text.unpack name ++ "_" ++ show i)
            WI.freshConstant sym nm (WI.BaseBVRepr (NatRepr.knownNat @8))

    let ty = LCT.VectorRepr (LCT.BVRepr (NatRepr.knownNat @8))
    let entry = LCS.RegEntry ty v
    ToConc.addToConcretize name entry

    pure v
