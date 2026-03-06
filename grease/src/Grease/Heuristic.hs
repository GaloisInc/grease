{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module      : Grease.Heuristic
--
-- For an overview of refinement, see "Grease.Refinement".
--
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Heuristic (
  OnlineSolverAndBackend,
  InitialMem (..),
  CantRefine (..),
  HeuristicResult (..),
  RefineHeuristic,
  mustFailHeuristic,
  llvmHeuristics,
  macawHeuristics,
  ErrorDescription (..),
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception.Safe (MonadThrow)
import Control.Lens (Lens', (.~), (^.))
import Data.BitVector.Sized qualified as BV
import Data.Bool qualified as Bool
import Data.Function ((&))
import Data.Functor.Const (Const (Const))
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Memory (MacawError (UnmappedGlobalMemoryAccess))
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Maybe qualified as Maybe
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Bug qualified as Bug
import Grease.Bug.UndefinedBehavior qualified as UB
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer (Dereference)
import Grease.Diagnostic (Diagnostic (HeuristicDiagnostic), GreaseLogAction)
import Grease.ErrorDescription (ErrorDescription (CrucibleLLVMError, MacawMemError))
import Grease.Heuristic.Diagnostic qualified as Diag
import Grease.Heuristic.Result (CantRefine (Exhausted, Exit, MissingFunc, MissingSemantics, MutableGlobal, SolverTimeout, SolverUnknown, Timeout, Unsupported), HeuristicResult (CantRefine, PossibleBug, RefinedPrecondition, Unknown), mergeResultsOptimistic)
import Grease.Macaw.RegName (RegNames, getRegName, mkRegName)
import Grease.MustFail qualified as MustFail
import Grease.Panic (panic)
import Grease.Setup (InitialMem (InitialMem))
import Grease.Setup.Annotations qualified as Anns
import Grease.Shape (ArgShapes, ExtShape, Shape (ShapeExt), argShapes)
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (MemShape (Uninitialized), ModifyPtrError, Offset (Offset), PtrShape (ShapePtr, ShapePtrBV), PtrTarget, bytesToPointers, growPtrTarget, growPtrTargetUpTo, initializeOrGrowPtrTarget, modifyPtrTarget, ptrTarget)
import Grease.Shape.Pointer qualified as ShapePtr
import Grease.Shape.Selector (ArgSelector, Selector (SelectArg, SelectRet), argSelectorIndex, argSelectorPath)
import Grease.Utility (OnlineSolverAndBackend, ppProgramLoc, tshow)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.Errors.MemoryError qualified as Mem
import Lang.Crucible.LLVM.Errors.UndefinedBehavior qualified as Mem
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.MemModel qualified as CLM hiding (Mem)
import Lang.Crucible.LLVM.MemModel.CallStack qualified as Mem
import Lang.Crucible.LLVM.MemModel.Generic qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as CLMP
import Lang.Crucible.Simulator qualified as CS
import Lumberjack qualified as LJ
import Numeric.Natural (Natural)
import Prettyprinter qualified as PP
import Text.LLVM.AST qualified as L
import What4.Expr qualified as WE
import What4.Expr.App qualified as W4
import What4.Expr.WeightedSum qualified as WSum
import What4.Interface qualified as WI
import What4.LabeledPred qualified as W4
import What4.ProgramLoc qualified as WPL
import What4.SemiRing qualified as SR

doLog :: GreaseLogAction -> Diag.Diagnostic -> IO ()
doLog la diag = LJ.writeLog la (HeuristicDiagnostic diag)

-- | Aggregates any error that should be processed by a memory error heuristic.
-- Currently, all Macaw errors are memory errors but this may change.
data AnyMemError sym
  = AnyMemErrorLLVM
      (Mem.MemoryError sym)
  | AnyMemErrorMacaw
      (MSM.MacawError sym)

type RefineHeuristic sym bak ext tys =
  bak ->
  Anns.Annotations sym ext tys ->
  InitialMem sym ->
  CB.ProofObligation sym ->
  Maybe (ErrorDescription sym) ->
  -- Argument names
  Ctx.Assignment (Const String) tys ->
  ArgShapes ext NoTag tys ->
  IO (HeuristicResult ext tys)

selectArg :: ArgSelector ext argTys ts t -> Lens' (ArgShapes ext NoTag argTys) (Shape ext 'ShapePtr.Precond NoTag t)
selectArg sel = argShapes . ixF' (sel ^. argSelectorIndex)

newtype MemoryErrorHeuristic sym ext w argTys
  = MemoryErrorHeuristic
      ( forall ts t.
        ( CB.IsSymInterface sym
        , Cursor.Last ts ~ CLMP.LLVMPointerType w
        ) =>
        sym ->
        WPL.ProgramLoc ->
        -- Argument names
        Ctx.Assignment (Const String) argTys ->
        ArgShapes ext NoTag argTys ->
        AnyMemError sym ->
        ArgSelector ext argTys ts t ->
        IO (HeuristicResult ext argTys)
      )

-- | Refine a pointer argument's shape by modifying its target allocation.
-- When 'Maybe Offset' is 'Nothing', the offset is left unchanged (defaulting
-- to 'Offset 0' for new pointers). When 'Just', the offset is overridden,
-- which is used to handle negative access offsets.
refinePtrArg ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , CLMP.HasPtrWidth w
  , Cursor.Last ts ~ CLMP.LLVMPointerType w'
  ) =>
  GreaseLogAction ->
  ArgShapes ext NoTag argTys ->
  ((regTy ~ CLMP.LLVMPointerType w) => PtrTarget w 'ShapePtr.Precond NoTag -> Either ModifyPtrError (PtrTarget w 'ShapePtr.Precond NoTag)) ->
  Maybe Offset ->
  ArgSelector ext argTys ts regTy ->
  IO (HeuristicResult ext argTys)
refinePtrArg la args modify mOffset sel =
  case args ^. selectArg sel of
    ShapeExt (ShapePtrBV _tag w) | Just Refl <- testEquality w ?ptrWidth -> do
      let
        pt :: ShapePtr.PtrTarget w 'ShapePtr.Precond NoTag
        pt = ptrTarget Nothing Seq.empty
      doLog la $ Diag.HeuristicPtrTarget pt
      let result = modify pt
      -- A note on the `panic`s: These errors are impossible, because we always
      -- use valid `ArgSelector`s in these heuristics.
      case result of
        Left err -> panic "refinePtrArg" [show (PP.pretty err)]
        Right pt' -> do
          let offset = Maybe.fromMaybe (Offset 0) mOffset
          let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag (ShapePtr.PrecondPtrData offset pt'))
          pure $ RefinedPrecondition args'
    ShapeExt (ShapePtr _tag (ShapePtr.PrecondPtrData existingOffset pt)) -> do
      doLog la $ Diag.HeuristicPtrTarget pt
      let result = modify pt
      case result of
        Left err -> panic "refinePtrArg" [show (PP.pretty err)]
        Right pt' -> do
          let offset = Maybe.fromMaybe existingOffset mOffset
          let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag (ShapePtr.PrecondPtrData offset pt'))
          pure $ RefinedPrecondition args'
    _ -> pure Unknown

-- | If a byte was treated as a pointer, turn it into one
newPointer ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , CLMP.HasPtrWidth w
  , Cursor.Last ts ~ CLMP.LLVMPointerType 8
  ) =>
  GreaseLogAction ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  ArgSelector ext argTys ts regTy ->
  IO (HeuristicResult ext argTys)
newPointer la argNames args sel = do
  let Const argName = argNames Ctx.! (sel ^. argSelectorIndex)
  doLog la $ Diag.DefaultHeuristicsBytesToPtr argName sel
  let path = sel ^. argSelectorPath
  refinePtrArg la args (bytesToPointers ?ptrWidth NoTag path) Nothing sel

-- | Grow and/or initialize an allocation
handleMemErr ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , CLMP.HasPtrWidth w
  , Cursor.Last ts ~ CLMP.LLVMPointerType w
  , CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder t st fs
  ) =>
  GreaseLogAction ->
  sym ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  AnyMemError sym ->
  ArgSelector ext argTys ts regTy ->
  IO (HeuristicResult ext argTys)
handleMemErr la sym argNames args err sel =
  case err of
    -- Most of the time, expanding an allocation backing a function won't help
    -- with calling the function. The one exception to this rule is when the
    -- function pointer value is a raw bitvector: crucible-llvm will always fail
    -- if this is the case, and expanding the function pointer value to an
    -- actual pointer might make some progress. (As explained in
    -- Note [Initializing empty pointer shapes] in Grease.Setup, this scenario
    -- can actually happen when refining a function pointer argument to a
    -- function.)
    (AnyMemErrorLLVM (Mem.MemoryError _op (Mem.BadFunctionPointer fle)))
      | fle /= Mem.RawBitvector ->
          pure Unknown
    AnyMemErrorLLVM (Mem.MemoryError op _reason) -> do
      let Const argName = argNames Ctx.! (sel ^. argSelectorIndex)
      let accessSize = memOpAccessSize op
      let ptrs = memOpPtrs op
      case ptrs of
        [ptr] | Just Refl <- testPtrWidth ptr -> do
          -- Single pointer operation - try to extract a constant offset
          let offset = CLMP.llvmPointerOffset ptr
          case tryExtractConstantOffset sym offset of
            Just constantOffset -> do
              let signedBytes = BV.asSigned ?ptrWidth constantOffset
              -- The Offset field controls where the pointer sits within its
              -- backing allocation. For non-negative offsets the pointer is at
              -- the start (Offset 0) and we grow forward. For negative offsets
              -- the access is *before* the pointer, so we place it further in:
              --
              --   Non-negative (e.g. ptr[2], 4-byte load):
              --
              --     Offset = 0
              --     |
              --     v
              --     +--+--+--+--+--+--+
              --     |  |  |XX|XX|XX|XX|  totalSize = offset + accessSize
              --     +--+--+--+--+--+--+
              --           ^-----------^
              --           access at ptr+2, 4 bytes
              --
              --   Negative (e.g. ptr[-3], 1-byte load):
              --
              --              Offset = 3
              --              |
              --              v
              --     +--+--+--+
              --     |XX|  |  |  totalSize = max(abs(offset), accessSize)
              --     +--+--+--+
              --     ^--^
              --     access at ptr-3, 1 byte
              --
              let (mOffset, totalSize) =
                    if signedBytes < 0
                      then
                        let absOff = abs signedBytes
                         in (Just (Offset (CLB.toBytes absOff)), max absOff accessSize)
                      else (Nothing, BV.asUnsigned constantOffset + accessSize)
              doLog la $ Diag.DecomposingOffset argName signedBytes sel
              doLog la $ Diag.DefaultHeuristicsGrowAndInitMem argName sel
              let path = sel ^. argSelectorPath
              let modifyTarget :: ShapePtr.PtrTarget w 'ShapePtr.Precond NoTag -> ShapePtr.PtrTarget w 'ShapePtr.Precond NoTag
                  modifyTarget tgt =
                    let currentSize = CLB.bytesToInteger (ShapePtr.ptrTargetSize ?ptrWidth tgt)
                     in if currentSize >= totalSize
                          then initializeOrGrowPtrTarget NoTag tgt
                          else growPtrTargetUpTo (CLB.toBytes totalSize) tgt
              refinePtrArg la args (modifyPtrTarget ?ptrWidth (Right . modifyTarget) path) mOffset sel
            Nothing -> growIncrementally la argName args sel
        _ -> growIncrementally la argName args sel
    _ -> do
      let Const argName = argNames Ctx.! (sel ^. argSelectorIndex)
      growIncrementally la argName args sel
 where
  growIncrementally ::
    forall ext w argTys ts regTy.
    ( ExtShape ext ~ PtrShape ext w
    , Cursor.CursorExt ext ~ Dereference ext w
    , CLMP.HasPtrWidth w
    , Cursor.Last ts ~ CLMP.LLVMPointerType w
    ) =>
    GreaseLogAction ->
    String ->
    ArgShapes ext NoTag argTys ->
    ArgSelector ext argTys ts regTy ->
    IO (HeuristicResult ext argTys)
  growIncrementally la' argName args' sel' = do
    doLog la' $ Diag.DefaultHeuristicsGrowAndInitMem argName sel'
    let modifyInner = Right . initializeOrGrowPtrTarget NoTag
    let path = sel' ^. argSelectorPath
    refinePtrArg la' args' (modifyPtrTarget ?ptrWidth modifyInner path) Nothing sel'

testPtrWidth ::
  ( CLMP.HasPtrWidth wptr
  , CB.IsSymInterface sym
  ) =>
  CLMP.LLVMPtr sym w ->
  Maybe (w C.:~: wptr)
testPtrWidth ptr = testEquality (CLMP.ptrWidth ptr) ?ptrWidth

assertPtrWidth ::
  forall sym wptr w m.
  ( MonadThrow m
  , CLMP.HasPtrWidth wptr
  , CB.IsSymInterface sym
  ) =>
  CLMP.LLVMPtr sym w ->
  m (w C.:~: wptr)
assertPtrWidth ptr =
  case testPtrWidth ptr of
    Nothing -> panic "assertPtrWidth" ["Non-pointer-width pointer in memory op?"]
    Just r -> pure r

allocInfoLoc ::
  WI.IsExprBuilder sym =>
  sym ->
  Mem.Mem sym ->
  CLMP.LLVMPtr sym w ->
  Maybe Text
allocInfoLoc sym mem ptr = do
  Mem.AllocInfo _aTy _sz _mut _align loc <-
    allocInfoFromPtr sym mem ptr
  let allocLoc = Text.pack loc
  if "grease setup" `Text.isPrefixOf` allocLoc
    then Nothing -- not helpful to report "internal" locations like this
    else Just ("Allocated at " <> allocLoc)

-- | See if a pointer came from an argument, and if so, grow/initialize it,
-- otherwise return 'Unknown'.
modPtr ::
  forall sym ext t st fs w w0 argTys.
  ( ExtShape ext ~ PtrShape ext w
  , CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder t st fs
  , Cursor.CursorExt ext ~ Dereference ext w
  , CLMP.HasPtrWidth w
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  ArgShapes ext NoTag argTys ->
  (PtrTarget w 'ShapePtr.Precond NoTag -> PtrTarget w 'ShapePtr.Precond NoTag) ->
  CLMP.LLVMPtr sym w0 ->
  IO (HeuristicResult ext argTys)
modPtr la anns sym args modify ptr = do
  Refl <- assertPtrWidth ptr
  case Anns.lookupPtrAnnotation anns sym ?ptrWidth ptr of
    Just (Anns.SomePtrSelector (SelectArg sel)) -> do
      let path = sel ^. argSelectorPath
      refinePtrArg la args (modifyPtrTarget ?ptrWidth (Right . modify) path) Nothing sel
    Just (Anns.SomePtrSelector (SelectRet{})) ->
      pure Unknown
    Nothing ->
      pure Unknown

-- | Try to extract a constant offset from a pointer offset expression.
-- Returns the constant if the offset is concrete or can be decomposed as base + constant.
tryExtractConstantOffset ::
  forall sym w t st fs.
  ( WI.IsExprBuilder sym
  , sym ~ WE.ExprBuilder t st fs
  , 1 C.<= w
  ) =>
  sym ->
  WI.SymBV sym w ->
  Maybe (BV.BV w)
tryExtractConstantOffset sym offset =
  let w = WI.bvWidth offset
      sr = SR.SemiRingBVRepr SR.BVArithRepr w
   in WI.asBV (Maybe.fromMaybe offset (WI.getUnannotatedTerm sym offset))
        <|> case W4.asSemiRingSum sr offset of
          Just ws -> case WSum.asAffineVar ws of
            Just (coeff, _base, constantOffset)
              | coeff == BV.one w -> Just constantOffset
            _ -> Nothing
          Nothing -> Nothing

-- | Extract the access size in bytes from a memory operation
memOpAccessSize ::
  WI.IsExprBuilder sym =>
  Mem.MemoryOp sym w ->
  Integer
memOpAccessSize = \case
  Mem.MemLoadOp storTy _ _ _ -> CLB.bytesToInteger (CLM.storageTypeSize storTy)
  Mem.MemStoreOp storTy _ _ _ -> CLB.bytesToInteger (CLM.storageTypeSize storTy)
  Mem.MemStoreBytesOp _ _ mLen _ ->
    case mLen >>= WI.asBV of
      Just bv -> BV.asUnsigned bv
      Nothing -> 0
  Mem.MemCopyOp _ _ len _ ->
    case WI.asBV len of
      Just bv -> BV.asUnsigned bv
      Nothing -> 0
  Mem.MemInvalidateOp _ _ _ len _ ->
    case WI.asBV len of
      Just bv -> BV.asUnsigned bv
      Nothing -> 0
  Mem.MemLoadHandleOp _ _ _ _ -> 0 -- Unknown size, fall back to incremental growth

-- | Helper, not exported
growPtrTargetUpToBv ::
  WI.IsExprBuilder sym =>
  CLMP.HasPtrWidth w =>
  Semigroup (tag (C.VectorType (CLMP.LLVMPointerType 8))) =>
  sym ->
  WI.SymBV sym w' ->
  PtrTarget w 'ShapePtr.Precond tag ->
  PtrTarget w 'ShapePtr.Precond tag
growPtrTargetUpToBv sym symBv =
  case WI.asBV (Maybe.fromMaybe symBv (WI.getUnannotatedTerm sym symBv)) of
    Nothing -> growPtrTarget
    Just bv ->
      let minSz = CLB.toBytes (BV.asUnsigned bv)
       in growPtrTargetUpTo minSz

-- | Helper, not exported
ptrBytes :: CLMP.HasPtrWidth w => CLB.Bytes
ptrBytes = CLB.toBytes (NatRepr.widthVal ?ptrWidth `div` 8)

-- | Heuristics for dealing with undefined behavior.
--
-- * If a non-heap pointer in an argument was freed, initialize that pointer
handleUB ::
  forall sym ext t st fs w argTys.
  ( ExtShape ext ~ PtrShape ext w
  , CB.IsSymInterface sym
  , sym ~ WE.ExprBuilder t st fs
  , Cursor.CursorExt ext ~ Dereference ext w
  , CLMP.HasPtrWidth w
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  WPL.ProgramLoc ->
  ArgShapes ext NoTag argTys ->
  Mem.UndefinedBehavior (CS.RegValue' sym) ->
  IO (HeuristicResult ext argTys)
handleUB la anns sym _loc args =
  \case
    Mem.FreeBadOffset (CS.RV ptr) ->
      makeIntoPointer ptr
    Mem.FreeUnallocated (CS.RV ptr) ->
      modPtr la anns sym args (growPtrTargetUpTo ptrBytes) ptr
    Mem.MemsetInvalidRegion (CS.RV ptr) _val (CS.RV len) ->
      modPtr la anns sym args (growPtrTargetUpToBv sym len) ptr
    Mem.PtrAddOffsetOutOfBounds (CS.RV ptr) (CS.RV offset) ->
      modPtr la anns sym args (growPtrTargetUpToBv sym offset) ptr
    _ -> pure Unknown
 where
  -- Turn an argument that was not previous a pointer into one
  makeIntoPointer ::
    forall w'.
    CLMP.LLVMPtr sym w' ->
    IO (HeuristicResult ext argTys)
  makeIntoPointer ptr = do
    Refl <- assertPtrWidth ptr
    case Anns.lookupPtrAnnotation anns sym ?ptrWidth ptr of
      Just (Anns.SomePtrSelector (SelectArg sel)) -> do
        case args ^. selectArg sel of
          ShapeExt (ShapePtrBV _tag w) | Just Refl <- testEquality w ?ptrWidth -> do
            let
              pt :: ShapePtr.PtrTarget wptr 'ShapePtr.Precond NoTag
              pt = ptrTarget Nothing (Seq.singleton (Uninitialized 1))
            doLog la $ Diag.HeuristicPtrTarget pt
            let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag $ ShapePtr.PrecondPtrData (Offset 0) pt)
            pure $ RefinedPrecondition args'
          _ -> pure Unknown
      Just (Anns.SomePtrSelector (SelectRet{})) -> pure Unknown
      Nothing -> pure Unknown

-- | Apply some heuristics to a single pointer.
applyMemoryHeuristics ::
  ( CB.IsSymInterface sym
  , C.IsSyntaxExtension ext
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ Dereference ext wptr
  , sym ~ WE.ExprBuilder t st fs
  , CLMP.HasPtrWidth wptr
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  MemoryErrorHeuristic sym ext wptr argTys ->
  MemoryErrorHeuristic sym ext 8 argTys ->
  WPL.ProgramLoc ->
  InitialMem sym ->
  AnyMemError sym ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  CLMP.LLVMPtr sym w ->
  IO (HeuristicResult ext argTys)
applyMemoryHeuristics _la anns sym ptrHeuristic byteHeuristic loc (InitialMem memImpl) memErr argNames args ptr = do
  Refl <- assertPtrWidth ptr
  let msel = Anns.lookupPtrAnnotation anns sym ?ptrWidth ptr
  let msel' = Anns.lookupPtrAnnotation anns sym (C.knownNat @8) ptr
  case (msel, msel') of
    (_, Just (Anns.SomePtrSelector (SelectRet{}))) ->
      pure Unknown
    (Just (Anns.SomePtrSelector (SelectRet{})), _) ->
      pure Unknown
    (Just (Anns.SomePtrSelector (SelectArg sel)), _) ->
      let MemoryErrorHeuristic h = ptrHeuristic
       in h sym loc argNames args memErr sel
    (_, Just (Anns.SomePtrSelector (SelectArg sel))) ->
      let MemoryErrorHeuristic h = byteHeuristic
       in h sym loc argNames args memErr sel
    -- No matching selector, so this was a pointer to an allocation made by the
    -- program, not by GREASE.
    (Nothing, Nothing) ->
      case memErr of
        AnyMemErrorLLVM llMemErr ->
          do
            Mem.MemoryError op reason <- pure llMemErr
            let mem = Mem.memOpMem op
            let ptrAllocType = do
                  Mem.AllocInfo aTy _sz _mut _align _loc <-
                    allocInfoFromPtr sym mem ptr
                  Just aTy
            case reason of
              Mem.NoSatisfyingWrite{}
                | ptrAllocType == Just Mem.StackAlloc ->
                    let bug =
                          Bug.BugInstance
                            { Bug.bugType = Bug.UninitStackRead
                            , Bug.bugLoc = ppProgramLoc loc
                            , Bug.bugDetails = allocInfoLoc sym (Mem.memOpMem op) ptr
                            , Bug.bugUb = Nothing
                            }
                     in pure (PossibleBug bug)
              Mem.NoSatisfyingWrite{}
                | Just name <- globalPtrName sym memImpl Mem.Mutable ptr ->
                    pure (CantRefine (MutableGlobal name))
              Mem.BadFunctionPointer Mem.NoOverride ->
                let maybeName = globalPtrName sym memImpl Mem.Immutable ptr
                 in pure (CantRefine (MissingFunc maybeName))
              _ -> pure Unknown
        AnyMemErrorMacaw _ -> pure Unknown

-- | Must-fail heuristic (see UC-KLEE paper)
--
-- Paper and video: https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/ramos
-- PDF: https://www.usenix.org/system/files/conference/usenixsecurity15/sec15-paper-ramos.pdf
mustFailHeuristic ::
  forall wptr solver sym bak t st fs ext argTys fm.
  ( OnlineSolverAndBackend solver sym bak t st fs
  , 16 C.<= wptr
  , CLMP.HasPtrWidth wptr
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , fs ~ WE.Flags fm
  ) =>
  RefineHeuristic sym bak ext argTys
mustFailHeuristic bak _anns _initMem obligation minfo _argNames _args =
  if MustFail.excludeMustFail obligation minfo
    then pure Unknown
    else do
      mustFail <- MustFail.oneMustFail bak [obligation]
      let lp = CB.proofGoal obligation
      let simError = lp ^. W4.labeledPredMsg
      let loc = CS.simErrorLoc simError
      let bug =
            Bug.BugInstance
              { Bug.bugType = Bug.MustFail
              , Bug.bugLoc = ppProgramLoc loc
              , Bug.bugDetails = do
                  let txt = tshow (CS.ppSimError simError)
                  case minfo of
                    Just (CrucibleLLVMError badBehavior callStack) ->
                      let txt' = tshow (Mem.ppBB badBehavior)
                       in Just $
                            if Mem.null callStack
                              then txt'
                              else txt' <> "\nin context:\n" <> tshow (Mem.ppCallStack callStack)
                    Just (MacawMemError (UnmappedGlobalMemoryAccess _)) -> Just txt
                    Nothing -> Just txt
              , Bug.bugUb = do
                  -- Maybe
                  (CrucibleLLVMError (Mem.BBUndefinedBehavior ub) _) <- minfo
                  Just (UB.makeUb ub)
              }
      if Bool.not mustFail
        then pure Unknown
        else pure (PossibleBug bug)

pointerHeuristic ::
  forall wptr solver sym bak t st fs ext argTys.
  ( C.IsSyntaxExtension ext
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ Dereference ext wptr
  , OnlineSolverAndBackend solver sym bak t st fs
  , 16 C.<= wptr
  , CLMP.HasPtrWidth wptr
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  GreaseLogAction ->
  MemoryErrorHeuristic sym ext wptr argTys ->
  MemoryErrorHeuristic sym ext 8 argTys ->
  RefineHeuristic sym bak ext argTys
pointerHeuristic la ptrHeuristic byteHeuristic bak anns initMem obligation minfo argNames args =
  case minfo of
    Just (CrucibleLLVMError (Mem.BBUndefinedBehavior ub) _cs) ->
      handleUB la anns sym loc args ub
    Just (CrucibleLLVMError (Mem.BBMemoryError memErr@(Mem.MemoryError op reason)) _cs) -> do
      let onePtrHeuristics =
            applyToErr (AnyMemErrorLLVM memErr)
      case op of
        Mem.MemLoadOp _ _ ptr _ -> onePtrHeuristics ptr
        Mem.MemStoreOp _ _ ptr _ -> onePtrHeuristics ptr
        Mem.MemStoreBytesOp _ ptr _ _ -> onePtrHeuristics ptr
        Mem.MemLoadHandleOp _ _ ptr _ -> onePtrHeuristics ptr
        Mem.MemInvalidateOp _ _ ptr _ _ -> onePtrHeuristics ptr
        Mem.MemCopyOp (_, dst) (_, src) _len _ -> do
          case reason of
            Mem.UnreadableRegion -> onePtrHeuristics src
            Mem.UnwritableRegion -> onePtrHeuristics dst
            _ -> do
              r1 <- onePtrHeuristics dst
              r2 <- onePtrHeuristics src
              pure (mergeResultsOptimistic r1 r2)
    Just (MacawMemError mmErr@(UnmappedGlobalMemoryAccess ptr)) ->
      applyToErr (AnyMemErrorMacaw mmErr) ptr
    Nothing -> pure Unknown
 where
  sym = CB.backendGetSym bak
  labeledPred = CB.proofGoal obligation
  loc = CS.simErrorLoc (labeledPred ^. W4.labeledPredMsg)
  applyToErr :: AnyMemError sym -> CLMP.LLVMPtr sym w -> IO (HeuristicResult ext argTys)
  applyToErr e ptr = applyMemoryHeuristics la anns sym ptrHeuristic byteHeuristic loc initMem e argNames args ptr

pointerHeuristics ::
  forall wptr solver sym bak t st fs ext argTys.
  ( C.IsSyntaxExtension ext
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ Dereference ext wptr
  , OnlineSolverAndBackend solver sym bak t st fs
  , 16 C.<= wptr
  , CLMP.HasPtrWidth wptr
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  GreaseLogAction ->
  MemoryErrorHeuristic sym ext wptr argTys ->
  MemoryErrorHeuristic sym ext 8 argTys ->
  [RefineHeuristic sym bak ext argTys]
pointerHeuristics la ptrHeuristic byteHeuristic =
  [ pointerHeuristic la ptrHeuristic byteHeuristic
  ]
 where

getConcretePointerBlock :: WI.IsExprBuilder sym => sym -> CLMP.LLVMPtr sym w -> Maybe Natural
getConcretePointerBlock sym ptr = tryAnnotated <|> tryUnannotated
 where
  int = WI.natToIntegerPure (CLMP.llvmPointerBlock ptr)
  tryAnnotated = fromIntegral <$> WI.asInteger int
  tryUnannotated = do
    term <- WI.getUnannotatedTerm sym int
    fromIntegral <$> WI.asInteger term

allocInfoFromPtr :: WI.IsExprBuilder sym => sym -> Mem.Mem sym -> CLMP.LLVMPtr sym w -> Maybe (Mem.AllocInfo sym)
allocInfoFromPtr sym mem ptr = do
  int <- getConcretePointerBlock sym ptr
  Mem.possibleAllocInfo int (Mem.memAllocs mem)

-- | Check if this pointer is a global with the given mutability, and if so,
-- return the name of the associated global variable. Returns 'Nothing' if the
-- mutability is wrong, if the pointer is not a global, or if the pointer\'s
-- block or offset aren\'t concrete.
globalPtrName :: CB.IsSymInterface sym => sym -> CLM.MemImpl sym -> Mem.Mutability -> CLMP.LLVMPtr sym w -> Maybe String
globalPtrName sym mem mut ptr =
  case allocInfoFromPtr sym (CLM.memImplHeap mem) ptr of
    Just (Mem.AllocInfo Mem.GlobalAlloc _sz actualMut _align _loc) | mut == actualMut -> do
      L.Symbol nm <- CLMP.isGlobalPointer (CLM.memImplSymbolMap mem) ptr
      Just nm
    _ -> Nothing

memOpPtrs :: Mem.MemoryOp sym w -> [CLMP.LLVMPtr sym w]
memOpPtrs =
  \case
    Mem.MemLoadOp _ _ ptr _ -> [ptr]
    Mem.MemStoreOp _ _ ptr _ -> [ptr]
    Mem.MemStoreBytesOp _ ptr _ _ -> [ptr]
    Mem.MemLoadHandleOp _ _ ptr _ -> [ptr]
    Mem.MemInvalidateOp _ _ ptr _ _ -> [ptr]
    Mem.MemCopyOp (_, dst) (_, src) _ _ -> [dst, src]

llvmHeuristics ::
  forall solver sym bak t st fs wptr argTys.
  ( OnlineSolverAndBackend solver sym bak t st fs
  , CLMP.HasPtrWidth wptr
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , wptr ~ 64 -- TODO(lb): Why is this necessary?
  ) =>
  GreaseLogAction ->
  [RefineHeuristic sym bak LLVM argTys]
llvmHeuristics la =
  let ptrHeuristic :: MemoryErrorHeuristic sym LLVM wptr argTys
      ptrHeuristic = MemoryErrorHeuristic (\sym _loc -> handleMemErr la sym)
      byteHeuristic :: MemoryErrorHeuristic sym LLVM 8 argTys
      byteHeuristic = MemoryErrorHeuristic (\_sym _loc argNames args _err sel -> newPointer la argNames args sel)
   in pointerHeuristics la ptrHeuristic byteHeuristic

-- | 'macawHeuristics' differs from 'llvmHeuristics' in that it explicitly
-- handles bad reads and writes to the stack.
macawHeuristics ::
  forall solver sym bak t st fs arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , OnlineSolverAndBackend solver sym bak t st fs
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , CLMP.HasPtrWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  GreaseLogAction ->
  RegNames arch ->
  [RefineHeuristic sym bak (Symbolic.MacawExt arch) (Symbolic.MacawCrucibleRegTypes arch)]
macawHeuristics la rNames =
  let ptrHeuristic = MemoryErrorHeuristic (\sym -> handleMemErr' sym)
      byteHeuristic = MemoryErrorHeuristic (\_sym _loc argNames args _err sel -> newPointer la argNames args sel)
   in pointerHeuristics la ptrHeuristic byteHeuristic
 where
  handleMemErr' ::
    forall ts regTy.
    (Cursor.Last ts ~ CLMP.LLVMPointerType (MC.ArchAddrWidth arch)) =>
    sym ->
    Cursor.Last ts ~ CLMP.LLVMPointerType (MC.ArchAddrWidth arch) =>
    WPL.ProgramLoc ->
    -- Argument names
    Ctx.Assignment (Const String) (Symbolic.MacawCrucibleRegTypes arch) ->
    ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.MacawCrucibleRegTypes arch) ->
    AnyMemError sym ->
    ArgSelector (Symbolic.MacawExt arch) (Symbolic.MacawCrucibleRegTypes arch) ts regTy ->
    IO (HeuristicResult (Symbolic.MacawExt arch) (Symbolic.MacawCrucibleRegTypes arch))
  handleMemErr' sym loc argNames args e@(AnyMemErrorLLVM (Mem.MemoryError op reason)) sel =
    if getRegName rNames (sel ^. argSelectorIndex) == mkRegName @arch MC.sp_reg
      then case reason of
        Mem.NoSatisfyingWrite _ ->
          let bug =
                Bug.BugInstance
                  { Bug.bugType = Bug.UninitStackRead
                  , Bug.bugLoc = ppProgramLoc loc
                  , Bug.bugDetails = do
                      let ptrs = memOpPtrs op
                      let details = Maybe.mapMaybe (allocInfoLoc sym (Mem.memOpMem op)) ptrs
                      Maybe.listToMaybe details
                  , Bug.bugUb = Nothing
                  }
           in pure (PossibleBug bug)
        _ ->
          -- In this case, we don't want to apply the default memory
          -- heuristics. These heuristics generally grow and initialize
          -- allocations, which isn't appropriate for the stack. We
          -- shouldn't pre-initialize the stack because it isn't realistic,
          -- and it causes performance issues (we can end up creating 1MiB =
          -- 1048576 fresh, bound variables, one for each byte in the stack).
          -- See !218 for details.
          pure Unknown
      else handleMemErr la sym argNames args e sel
  handleMemErr' sym _ argNames args e@(AnyMemErrorMacaw _) sel = handleMemErr la sym argNames args e sel
