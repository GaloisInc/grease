{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Concretization overrides that request models from the SMT solver.
module Grease.Syntax.Overrides.Concretize (
  -- * Individual type concretization (unsound)
  concBoolOverride,
  tryConcBoolOverride,
  concIntegerOverride,
  tryConcIntegerOverride,
  concNatOverride,
  tryConcNatOverride,
  concBvOverride,
  tryConcBvOverride,
  concPtrOverride,
  tryConcPtrOverride,

  -- * Individual type concretization (sound)
  uniqueConcBoolOverride,
  tryUniqueConcBoolOverride,
  uniqueConcIntegerOverride,
  tryUniqueConcIntegerOverride,
  uniqueConcNatOverride,
  tryUniqueConcNatOverride,
  uniqueConcBvOverride,
  tryUniqueConcBvOverride,
  uniqueConcPtrOverride,
  tryUniqueConcPtrOverride,

  -- * Composite type concretization
  concVectorBvOverride,
  tryConcVectorBvOverride,
  uniqueConcVectorBvOverride,
  tryUniqueConcVectorBvOverride,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Type.Equality (testEquality, (:~:))
import Data.Type.Ord (type (<=))
import Data.Vector qualified as Vec
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.FunctionHandle qualified as LCF
import Lang.Crucible.LLVM.MemModel.Pointer qualified as CLMP
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as LCT
import What4.Concretize qualified as WC
import What4.Expr.Builder qualified as WEB
import What4.Expr.GroundEval qualified as W4GE
import What4.FloatMode qualified as W4FM
import What4.Interface qualified as WI

---------------------------------------------------------------------

-- * Helper

-- | Check if a 'CS.TypedOverride' is compatible with a 'LCF.FnHandle'
checkTypedOverrideHandleCompat ::
  LCF.FnHandle args ret ->
  CS.TypedOverride p sym ext args' ret' ->
  Maybe (args :~: args', ret :~: ret')
checkTypedOverrideHandleCompat hdl ov = do
  rArgs <- testEquality (LCF.handleArgTypes hdl) (CS.typedOverrideArgs ov)
  rRet <- testEquality (LCF.handleReturnType hdl) (CS.typedOverrideRet ov)
  pure (rArgs, rRet)

---------------------------------------------------------------------

-- * Bool

-- | See if @conc-bool@ is compatible with the given handle.
tryConcBoolOverride ::
  forall p sym ext bak scope st fs solver args ret.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcBoolOverride bak hdl = do
  let ov = concBoolOverride @p @ext bak
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-bool@.
concBoolOverride ::
  forall p ext sym bak scope st fs solver.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.BoolType) LCT.BoolType
concBoolOverride bak =
  CS.typedOverride (Ctx.uncurryAssignment (concBool bak))

-- | Implementation of @conc-bool@ override.
concBool ::
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.RegValue' sym LCT.BoolType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.BoolType)
concBool bak (CS.RV b) = do
  mb <- liftIO (Conc.concRegValue bak MapF.empty LCT.BoolRepr b)
  case mb of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-bool: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-bool: unsat initial assumptions")
    Right bLit ->
      liftIO (W4GE.groundToSym (CB.backendGetSym bak) WI.BaseBoolRepr bLit)

-- | See if @unique-conc-bool@ is compatible with the given handle.
tryUniqueConcBoolOverride ::
  forall p sym ext bak scope st fs fm solver args ret.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcBoolOverride bak fm hdl = do
  let ov = uniqueConcBoolOverride @p @ext bak fm
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-bool@.
uniqueConcBoolOverride ::
  forall p ext sym bak scope st fs fm solver.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.BoolType) LCT.BoolType
uniqueConcBoolOverride bak fm =
  CS.typedOverride (Ctx.uncurryAssignment (uniqueConcBool bak fm))

-- | Implementation of @unique-conc-bool@ override.
uniqueConcBool ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.RegValue' sym LCT.BoolType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.BoolType)
uniqueConcBool bak fm (CS.RV b) = do
  mb <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty LCT.BoolRepr b)
  case mb of
    Left (WC.GroundingFailure WC.SolverUnknown) ->
      CS.overrideError (CS.GenericSimError "unique-conc-bool: solver returned UNKNOWN")
    Left (WC.GroundingFailure WC.UnsatInitialAssumptions) ->
      CS.overrideError (CS.GenericSimError "unique-conc-bool: unsat initial assumptions")
    Left _ ->
      pure b -- Not unique, return symbolic
    Right bLit ->
      liftIO (W4GE.groundToSym (CB.backendGetSym bak) WI.BaseBoolRepr bLit)

---------------------------------------------------------------------

-- * Integer

-- | See if @conc-integer@ is compatible with the given handle.
tryConcIntegerOverride ::
  forall p sym ext bak scope st fs solver args ret.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcIntegerOverride bak hdl = do
  let ov = concIntegerOverride @p @ext bak
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-integer@.
concIntegerOverride ::
  forall p ext sym bak scope st fs solver.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.IntegerType) LCT.IntegerType
concIntegerOverride bak =
  CS.typedOverride (Ctx.uncurryAssignment (concInteger bak))

-- | Implementation of @conc-integer@ override.
concInteger ::
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.RegValue' sym LCT.IntegerType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.IntegerType)
concInteger bak (CS.RV i) = do
  mb <- liftIO (Conc.concRegValue bak MapF.empty LCT.IntegerRepr i)
  case mb of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-integer: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-integer: unsat initial assumptions")
    Right iLit ->
      liftIO (WI.intLit (CB.backendGetSym bak) iLit)

-- | See if @unique-conc-integer@ is compatible with the given handle.
tryUniqueConcIntegerOverride ::
  forall p sym ext bak scope st fs fm solver args ret.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcIntegerOverride bak fm hdl = do
  let ov = uniqueConcIntegerOverride @p @ext bak fm
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-integer@.
uniqueConcIntegerOverride ::
  forall p ext sym bak scope st fs fm solver.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.IntegerType) LCT.IntegerType
uniqueConcIntegerOverride bak fm =
  CS.typedOverride (Ctx.uncurryAssignment (uniqueConcInteger bak fm))

-- | Implementation of @unique-conc-integer@ override.
uniqueConcInteger ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.RegValue' sym LCT.IntegerType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.IntegerType)
uniqueConcInteger bak fm (CS.RV i) = do
  mb <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty LCT.IntegerRepr i)
  case mb of
    Left (WC.GroundingFailure WC.SolverUnknown) ->
      CS.overrideError (CS.GenericSimError "unique-conc-integer: solver returned UNKNOWN")
    Left (WC.GroundingFailure WC.UnsatInitialAssumptions) ->
      CS.overrideError (CS.GenericSimError "unique-conc-integer: unsat initial assumptions")
    Left _ ->
      pure i -- Not unique, return symbolic
    Right iLit ->
      liftIO (WI.intLit (CB.backendGetSym bak) iLit)

---------------------------------------------------------------------

-- * Nat

-- | See if @conc-nat@ is compatible with the given handle.
tryConcNatOverride ::
  forall p sym ext bak scope st fs solver args ret.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcNatOverride bak hdl = do
  let ov = concNatOverride @p @ext bak
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-nat@.
concNatOverride ::
  forall p ext sym bak scope st fs solver.
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.NatType) LCT.NatType
concNatOverride bak =
  CS.typedOverride (Ctx.uncurryAssignment (concNat bak))

-- | Implementation of @conc-nat@ override.
concNat ::
  OnlineSolverAndBackend solver sym bak scope st fs =>
  bak ->
  CS.RegValue' sym LCT.NatType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.NatType)
concNat bak (CS.RV n) = do
  let sym = CB.backendGetSym bak
  -- Nat is represented as an Integer when concretized
  mb <- liftIO (Conc.concRegValue bak MapF.empty LCT.NatRepr n)
  case mb of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-nat: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-nat: unsat initial assumptions")
    Right nLit ->
      liftIO (WI.integerToNat sym =<< W4GE.groundToSym sym WI.BaseIntegerRepr nLit)

-- | See if @unique-conc-nat@ is compatible with the given handle.
tryUniqueConcNatOverride ::
  forall p sym ext bak scope st fs fm solver args ret.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcNatOverride bak fm hdl = do
  let ov = uniqueConcNatOverride @p @ext bak fm
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-nat@.
uniqueConcNatOverride ::
  forall p ext sym bak scope st fs fm solver.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.NatType) LCT.NatType
uniqueConcNatOverride bak fm =
  CS.typedOverride (Ctx.uncurryAssignment (uniqueConcNat bak fm))

-- | Implementation of @unique-conc-nat@ override.
uniqueConcNat ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.RegValue' sym LCT.NatType ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym LCT.NatType)
uniqueConcNat bak fm (CS.RV n) = do
  let sym = CB.backendGetSym bak
  mb <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty LCT.NatRepr n)
  case mb of
    Left (WC.GroundingFailure WC.SolverUnknown) ->
      CS.overrideError (CS.GenericSimError "unique-conc-nat: solver returned UNKNOWN")
    Left (WC.GroundingFailure WC.UnsatInitialAssumptions) ->
      CS.overrideError (CS.GenericSimError "unique-conc-nat: unsat initial assumptions")
    Left _ ->
      pure n -- Not unique, return symbolic
    Right nLit ->
      liftIO (WI.integerToNat sym =<< W4GE.groundToSym sym WI.BaseIntegerRepr nLit)

---------------------------------------------------------------------

-- * Bitvector

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

-- | See if @unique-conc-bv-*@ is compatible with the given handle.
tryUniqueConcBvOverride ::
  forall p sym ext bak scope st fs fm solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcBvOverride bak fm w hdl = do
  let ov = uniqueConcBvOverride @p @ext bak fm w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-bv-*@.
uniqueConcBvOverride ::
  forall p ext sym bak scope st fs fm solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.BVType w) (LCT.BVType w)
uniqueConcBvOverride bak fm w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (uniqueConcBv bak fm)))

-- | Implementation of @unique-conc-bv-*@ override.
uniqueConcBv ::
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  CS.RegValue' sym (LCT.BVType w) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.BVType w))
uniqueConcBv bak fm (CS.RV bv) = do
  let w = WI.bvWidth bv
  mb <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty (LCT.BVRepr w) bv)
  case mb of
    Left (WC.GroundingFailure WC.SolverUnknown) ->
      CS.overrideError (CS.GenericSimError "unique-conc-bv-*: solver returned UNKNOWN")
    Left (WC.GroundingFailure WC.UnsatInitialAssumptions) ->
      CS.overrideError (CS.GenericSimError "unique-conc-bv-*: unsat initial assumptions")
    Left _ ->
      pure bv -- Not unique, return symbolic
    Right bvLit ->
      liftIO (WI.bvLit (CB.backendGetSym bak) w bvLit)

---------------------------------------------------------------------

-- * Pointer

-- | See if @conc-ptr-*@ is compatible with the given handle.
tryConcPtrOverride ::
  forall p sym ext bak scope st fs solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcPtrOverride bak w hdl = do
  let ov = concPtrOverride @p @ext bak w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-ptr-*@.
--
-- Concretizes both the block number and offset of a pointer.
concPtrOverride ::
  forall p ext sym bak scope st fs solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> CLMP.LLVMPointerType w) (CLMP.LLVMPointerType w)
concPtrOverride bak w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (concPtr bak w)))

-- | Implementation of @conc-ptr-*@ override.
--
-- Concretizes both the block number and offset of the pointer using concRegMap
-- to ensure both components are concretized in the same model.
concPtr ::
  forall p ext sym bak scope st fs solver w r args ret.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  NatRepr.NatRepr w ->
  CS.RegValue' sym (CLMP.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (CLMP.LLVMPointerType w))
concPtr bak w (CS.RV ptr) = do
  let CLMP.LLVMPointer blk off = ptr
      sym = CB.backendGetSym bak

  -- Create a RegMap with both the block and offset to concretize them in the same model
  let blkEntry = CS.RegEntry LCT.NatRepr blk
      offEntry = CS.RegEntry (LCT.BVRepr w) off
      regMap = CS.RegMap (Ctx.Empty Ctx.:> blkEntry Ctx.:> offEntry)

  -- Concretize both components in the same model
  mbConc <- liftIO (Conc.concRegMap bak MapF.empty regMap)
  case mbConc of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-ptr-*: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-ptr-*: unsat initial assumptions")
    Right (Ctx.Empty Ctx.:> Conc.ConcRV' blkLit Ctx.:> Conc.ConcRV' offLit) -> do
      -- Reconstruct the pointer with concrete values
      blkSym <- liftIO (WI.integerToNat sym =<< W4GE.groundToSym sym WI.BaseIntegerRepr blkLit)
      offSym <- liftIO (WI.bvLit sym w offLit)
      pure (CLMP.LLVMPointer blkSym offSym)

-- | See if @unique-conc-ptr-*@ is compatible with the given handle.
tryUniqueConcPtrOverride ::
  forall p sym ext bak scope st fs fm solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcPtrOverride bak fm w hdl = do
  let ov = uniqueConcPtrOverride @p @ext bak fm w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-ptr-*@.
--
-- Checks if the pointer has a unique value (both block and offset).
-- Only concretizes the offset if the block is already concrete.
uniqueConcPtrOverride ::
  forall p ext sym bak scope st fs fm solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> CLMP.LLVMPointerType w) (CLMP.LLVMPointerType w)
uniqueConcPtrOverride bak fm w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (uniqueConcPtr bak fm w)))

-- | Implementation of @unique-conc-ptr-*@ override.
--
-- Only attempts to uniquely concretize the offset when the block is concrete.
uniqueConcPtr ::
  forall p ext sym bak scope st fs fm solver w r args ret.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  NatRepr.NatRepr w ->
  CS.RegValue' sym (CLMP.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (CLMP.LLVMPointerType w))
uniqueConcPtr bak fm w (CS.RV ptr) = do
  let CLMP.LLVMPointer blk off = ptr
      sym = CB.backendGetSym bak
  case (WI.asNat blk, WI.asBV off) of
    (Just _, Just _) ->
      pure ptr -- Already fully concrete
    (Just _concreteBlk, Nothing) -> do
      -- Block is concrete but offset is symbolic, check if offset is unique
      let offsetTy = LCT.BVRepr w
      result <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty offsetTy off)
      case result of
        Right concreteOffset -> do
          -- Offset is unique, convert back to symbolic and rebuild pointer
          symOffset <- liftIO (WI.bvLit sym w concreteOffset)
          pure (CLMP.LLVMPointer blk symOffset)
        Left _ ->
          -- Not unique or couldn't get model, keep symbolic
          pure ptr
    _ ->
      -- Block is symbolic, can't use unique concretization
      pure ptr

---------------------------------------------------------------------

-- * Vector (Bitvector elements)

-- | See if @conc-vector-bv-*@ is compatible with the given handle.
tryConcVectorBvOverride ::
  forall p sym ext bak scope st fs solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryConcVectorBvOverride bak w hdl = do
  let ov = concVectorBvOverride @p @ext bak w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @conc-vector-bv-*@.
--
-- Concretizes all elements of a vector of bitvectors in the same model.
concVectorBvOverride ::
  forall p ext sym bak scope st fs solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.VectorType (LCT.BVType w)) (LCT.VectorType (LCT.BVType w))
concVectorBvOverride bak w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (concVectorBv bak w)))

-- | Implementation of @conc-vector-bv-*@ override.
--
-- Concretizes the entire vector in the same model.
concVectorBv ::
  forall p ext sym bak scope st fs solver w r args ret.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  ) =>
  bak ->
  NatRepr.NatRepr w ->
  CS.RegValue' sym (LCT.VectorType (LCT.BVType w)) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.VectorType (LCT.BVType w)))
concVectorBv bak w (CS.RV vec) = do
  let vecRepr = LCT.VectorRepr (LCT.BVRepr w)
  mb <- liftIO (Conc.concRegValue bak MapF.empty vecRepr vec)
  case mb of
    Left WC.SolverUnknown ->
      CS.overrideError (CS.GenericSimError "conc-vector-bv-*: solver returned UNKNOWN")
    Left WC.UnsatInitialAssumptions ->
      CS.overrideError (CS.GenericSimError "conc-vector-bv-*: unsat initial assumptions")
    Right concVec -> do
      -- Convert the concretized vector back to symbolic
      let sym = CB.backendGetSym bak
      liftIO $ Vec.mapM (\(Conc.ConcRV' bvLit) -> WI.bvLit sym w bvLit) concVec

-- | See if @unique-conc-vector-bv-*@ is compatible with the given handle.
tryUniqueConcVectorBvOverride ::
  forall p sym ext bak scope st fs fm solver args ret w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  WI.NatRepr w ->
  LCF.FnHandle args ret ->
  Maybe (CS.Override p sym ext args ret)
tryUniqueConcVectorBvOverride bak fm w hdl = do
  let ov = uniqueConcVectorBvOverride @p @ext bak fm w
  (WI.Refl, WI.Refl) <- checkTypedOverrideHandleCompat hdl ov
  Just (CS.runTypedOverride (LCF.handleName hdl) ov)

-- | Override for @unique-conc-vector-bv-*@.
--
-- Checks if all elements of the vector have unique values.
uniqueConcVectorBvOverride ::
  forall p ext sym bak scope st fs fm solver w.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  NatRepr.NatRepr w ->
  CS.TypedOverride p sym ext (Ctx.EmptyCtx Ctx.::> LCT.VectorType (LCT.BVType w)) (LCT.VectorType (LCT.BVType w))
uniqueConcVectorBvOverride bak fm w =
  WI.withKnownNat w (CS.typedOverride (Ctx.uncurryAssignment (uniqueConcVectorBv bak fm w)))

-- | Implementation of @unique-conc-vector-bv-*@ override.
--
-- Attempts to uniquely concretize the entire vector.
uniqueConcVectorBv ::
  forall p ext sym bak scope st fs fm solver w r args ret.
  ( 1 <= w
  , OnlineSolverAndBackend solver sym bak scope st fs
  , fs ~ WEB.Flags fm
  ) =>
  bak ->
  W4FM.FloatModeRepr fm ->
  NatRepr.NatRepr w ->
  CS.RegValue' sym (LCT.VectorType (LCT.BVType w)) ->
  CS.OverrideSim p sym ext r args ret (CS.RegValue sym (LCT.VectorType (LCT.BVType w)))
uniqueConcVectorBv bak fm w (CS.RV vec) = do
  let vecRepr = LCT.VectorRepr (LCT.BVRepr w)
  mb <- liftIO (Conc.uniquelyConcRegValue bak fm MapF.empty MapF.empty vecRepr vec)
  case mb of
    Left (WC.GroundingFailure WC.SolverUnknown) ->
      CS.overrideError (CS.GenericSimError "unique-conc-vector-bv-*: solver returned UNKNOWN")
    Left (WC.GroundingFailure WC.UnsatInitialAssumptions) ->
      CS.overrideError (CS.GenericSimError "unique-conc-vector-bv-*: unsat initial assumptions")
    Left _ ->
      pure vec -- Not unique, return symbolic
    Right concVec -> do
      -- Convert the concretized vector back to symbolic
      let sym = CB.backendGetSym bak
      liftIO $ Vec.mapM (\(Conc.ConcRV' bvLit) -> WI.bvLit sym w bvLit) concVec

---------------------------------------------------------------------
