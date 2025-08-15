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

import Control.Applicative (Alternative ((<|>)), Const (..))
import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens (Lens', (.~), (^.))
import Data.BitVector.Sized qualified as BV
import Data.Bool qualified as Bool
import Data.Function ((&))
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Memory (MacawError (UnmappedGlobalMemoryAccess))
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Maybe qualified as Maybe
import Data.Parameterized.Classes (IxedF' (..))
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
import Grease.Diagnostic
import Grease.ErrorDescription (ErrorDescription (..))
import Grease.Heuristic.Diagnostic qualified as Diag
import Grease.Heuristic.Result
import Grease.Macaw.RegName (RegNames, getRegName, mkRegName)
import Grease.MustFail qualified as MustFail
import Grease.Setup
import Grease.Setup.Annotations qualified as Anns
import Grease.Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer
import Grease.Shape.Selector
import Grease.Utility (GreaseException (..), OnlineSolverAndBackend, ppProgramLoc, tshow)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.Errors.MemoryError qualified as Mem
import Lang.Crucible.LLVM.Errors.UndefinedBehavior qualified as Mem
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.MemModel qualified as Mem hiding (Mem)
import Lang.Crucible.LLVM.MemModel.CallStack qualified as Mem
import Lang.Crucible.LLVM.MemModel.Generic qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lumberjack qualified as LJ
import Numeric.Natural (Natural)
import Text.LLVM.AST qualified as L
import What4.Expr qualified as W4
import What4.Interface qualified as W4
import What4.LabeledPred qualified as W4
import What4.ProgramLoc qualified as W4

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
  C.ProofObligation sym ->
  Maybe (ErrorDescription sym) ->
  -- Argument names
  Ctx.Assignment (Const String) tys ->
  ArgShapes ext NoTag tys ->
  IO (HeuristicResult ext tys)

selectArg :: ArgSelector ext argTys ts t -> Lens' (ArgShapes ext NoTag argTys) (Shape ext NoTag t)
selectArg sel = argShapes . ixF' (sel ^. argSelectorIndex)

newtype MemoryErrorHeuristic sym ext w argTys
  = MemoryErrorHeuristic
      ( forall ts t.
        ( C.IsSymInterface sym
        , Cursor.Last ts ~ Mem.LLVMPointerType w
        ) =>
        sym ->
        W4.ProgramLoc ->
        -- Argument names
        Ctx.Assignment (Const String) argTys ->
        ArgShapes ext NoTag argTys ->
        AnyMemError sym ->
        ArgSelector ext argTys ts t ->
        IO (HeuristicResult ext argTys)
      )

refinePtrArg ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , Mem.HasPtrWidth w
  , Cursor.Last ts ~ Mem.LLVMPointerType w'
  ) =>
  GreaseLogAction ->
  ArgShapes ext NoTag argTys ->
  ((regTy ~ Mem.LLVMPointerType w) => PtrTarget w NoTag -> IO (PtrTarget w NoTag)) ->
  ArgSelector ext argTys ts regTy ->
  IO (HeuristicResult ext argTys)
refinePtrArg la args modify sel =
  case args ^. selectArg sel of
    ShapeExt (ShapePtrBV _tag w) | Just Refl <- testEquality w ?ptrWidth -> do
      let pt = ptrTarget Nothing Seq.empty
      doLog la $ Diag.HeuristicPtrTarget pt
      pt' <- modify pt
      let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag (Offset 0) pt')
      pure $ RefinedPrecondition args'
    ShapeExt (ShapePtr _tag offset pt) -> do
      doLog la $ Diag.HeuristicPtrTarget pt
      pt' <- modify pt
      let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag offset pt')
      pure $ RefinedPrecondition args'
    _ -> pure Unknown

-- | If a byte was treated as a pointer, turn it into one
newPointer ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , Mem.HasPtrWidth w
  , Cursor.Last ts ~ Mem.LLVMPointerType 8
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
  refinePtrArg la args (bytesToPointers ?ptrWidth NoTag path) sel

-- | Grow and/or initialize an allocation
handleMemErr ::
  ( ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ Dereference ext w
  , Mem.HasPtrWidth w
  , Cursor.Last ts ~ Mem.LLVMPointerType w
  ) =>
  GreaseLogAction ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  AnyMemError sym ->
  ArgSelector ext argTys ts regTy ->
  IO (HeuristicResult ext argTys)
handleMemErr la argNames args err sel =
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
    _ -> do
      let Const argName = argNames Ctx.! (sel ^. argSelectorIndex)
      doLog la $ Diag.DefaultHeuristicsGrowAndInitMem argName sel
      let modifyInner = pure @IO . initializeOrGrowPtrTarget NoTag
      let path = sel ^. argSelectorPath
      refinePtrArg la args (modifyPtrTarget ?ptrWidth modifyInner path) sel

testPtrWidth ::
  ( Mem.HasPtrWidth wptr
  , C.IsSymInterface sym
  ) =>
  Mem.LLVMPtr sym w ->
  Maybe (w C.:~: wptr)
testPtrWidth ptr = testEquality (Mem.ptrWidth ptr) ?ptrWidth

assertPtrWidth ::
  forall sym wptr w m.
  ( MonadThrow m
  , Mem.HasPtrWidth wptr
  , C.IsSymInterface sym
  ) =>
  Mem.LLVMPtr sym w ->
  m (w C.:~: wptr)
assertPtrWidth ptr =
  case testPtrWidth ptr of
    Nothing -> throw (GreaseException "Non-pointer-width pointer in memory op?")
    Just r -> pure r

allocInfoLoc ::
  W4.IsExprBuilder sym =>
  sym ->
  Mem.Mem sym ->
  Mem.LLVMPtr sym w ->
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
  , C.IsSymInterface sym
  , sym ~ W4.ExprBuilder t st fs
  , Cursor.CursorExt ext ~ Dereference ext w
  , Mem.HasPtrWidth w
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  ArgShapes ext NoTag argTys ->
  (PtrTarget w NoTag -> PtrTarget w NoTag) ->
  Mem.LLVMPtr sym w0 ->
  IO (HeuristicResult ext argTys)
modPtr la anns sym args modify ptr = do
  Refl <- assertPtrWidth ptr
  case Anns.lookupPtrAnnotation anns sym ?ptrWidth ptr of
    Just (Anns.SomePtrSelector (SelectArg sel)) -> do
      let path = sel ^. argSelectorPath
      refinePtrArg la args (modifyPtrTarget ?ptrWidth (pure . modify) path) sel
    Just (Anns.SomePtrSelector (SelectRet{})) ->
      pure Unknown
    Nothing ->
      pure Unknown

-- | Helper, not exported
growPtrTargetUpToBv ::
  W4.IsExprBuilder sym =>
  Mem.HasPtrWidth w =>
  Semigroup (tag (C.VectorType (Mem.LLVMPointerType 8))) =>
  sym ->
  W4.SymBV sym w' ->
  PtrTarget w tag ->
  PtrTarget w tag
growPtrTargetUpToBv sym symBv =
  case W4.asBV (Maybe.fromMaybe symBv (W4.getUnannotatedTerm sym symBv)) of
    Nothing -> growPtrTarget
    Just bv ->
      let minSz = Bytes.toBytes (BV.asUnsigned bv)
       in growPtrTargetUpTo minSz

-- | Helper, not exported
ptrBytes :: Mem.HasPtrWidth w => Bytes.Bytes
ptrBytes = Bytes.toBytes (NatRepr.widthVal ?ptrWidth `div` 8)

-- | Heuristics for dealing with undefined behavior.
--
-- * If a non-heap pointer in an argument was freed, initialize that pointer
handleUB ::
  forall sym ext t st fs w argTys.
  ( ExtShape ext ~ PtrShape ext w
  , C.IsSymInterface sym
  , sym ~ W4.ExprBuilder t st fs
  , Cursor.CursorExt ext ~ Dereference ext w
  , Mem.HasPtrWidth w
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  W4.ProgramLoc ->
  ArgShapes ext NoTag argTys ->
  Mem.UndefinedBehavior (C.RegValue' sym) ->
  IO (HeuristicResult ext argTys)
handleUB la anns sym _loc args =
  \case
    Mem.FreeBadOffset (C.RV ptr) ->
      makeIntoPointer ptr
    Mem.FreeUnallocated (C.RV ptr) ->
      modPtr la anns sym args (growPtrTargetUpTo ptrBytes) ptr
    Mem.MemsetInvalidRegion (C.RV ptr) _val (C.RV len) ->
      modPtr la anns sym args (growPtrTargetUpToBv sym len) ptr
    Mem.PtrAddOffsetOutOfBounds (C.RV ptr) (C.RV offset) ->
      modPtr la anns sym args (growPtrTargetUpToBv sym offset) ptr
    _ -> pure Unknown
 where
  -- Turn an argument that was not previous a pointer into one
  makeIntoPointer ::
    forall w'.
    Mem.LLVMPtr sym w' ->
    IO (HeuristicResult ext argTys)
  makeIntoPointer ptr = do
    Refl <- assertPtrWidth ptr
    case Anns.lookupPtrAnnotation anns sym ?ptrWidth ptr of
      Just (Anns.SomePtrSelector (SelectArg sel)) -> do
        case args ^. selectArg sel of
          ShapeExt (ShapePtrBV _tag w) | Just Refl <- testEquality w ?ptrWidth -> do
            let pt = ptrTarget Nothing (Seq.singleton (Uninitialized 1))
            doLog la $ Diag.HeuristicPtrTarget pt
            let args' = args & selectArg sel .~ ShapeExt (ShapePtr NoTag (Offset 0) pt)
            pure $ RefinedPrecondition args'
          _ -> pure Unknown
      Just (Anns.SomePtrSelector (SelectRet{})) -> pure Unknown
      Nothing -> pure Unknown

-- | Apply some heuristics to a single pointer.
applyMemoryHeuristics ::
  ( C.IsSymInterface sym
  , C.IsSyntaxExtension ext
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ Dereference ext wptr
  , sym ~ W4.ExprBuilder t st fs
  , Mem.HasPtrWidth wptr
  ) =>
  GreaseLogAction ->
  Anns.Annotations sym ext argTys ->
  sym ->
  MemoryErrorHeuristic sym ext wptr argTys ->
  MemoryErrorHeuristic sym ext 8 argTys ->
  W4.ProgramLoc ->
  InitialMem sym ->
  AnyMemError sym ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  ArgShapes ext NoTag argTys ->
  Mem.LLVMPtr sym w ->
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
  , Mem.HasPtrWidth wptr
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  , fs ~ W4.Flags fm
  ) =>
  RefineHeuristic sym bak ext argTys
mustFailHeuristic bak _anns _initMem obligation minfo _argNames _args =
  if MustFail.excludeMustFail obligation minfo
    then pure Unknown
    else do
      mustFail <- MustFail.oneMustFail bak [obligation]
      let lp = C.proofGoal obligation
      let simError = lp ^. W4.labeledPredMsg
      let loc = C.simErrorLoc simError
      let bug =
            Bug.BugInstance
              { Bug.bugType = Bug.MustFail
              , Bug.bugLoc = ppProgramLoc loc
              , Bug.bugDetails = do
                  let txt = tshow (C.ppSimError simError)
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
  , Mem.HasPtrWidth wptr
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
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
  sym = C.backendGetSym bak
  labeledPred = C.proofGoal obligation
  loc = C.simErrorLoc (labeledPred ^. W4.labeledPredMsg)
  applyToErr :: AnyMemError sym -> Mem.LLVMPtr sym w -> IO (HeuristicResult ext argTys)
  applyToErr e ptr = applyMemoryHeuristics la anns sym ptrHeuristic byteHeuristic loc initMem e argNames args ptr

pointerHeuristics ::
  forall wptr solver sym bak t st fs ext argTys.
  ( C.IsSyntaxExtension ext
  , ExtShape ext ~ PtrShape ext wptr
  , Cursor.CursorExt ext ~ Dereference ext wptr
  , OnlineSolverAndBackend solver sym bak t st fs
  , 16 C.<= wptr
  , Mem.HasPtrWidth wptr
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  MemoryErrorHeuristic sym ext wptr argTys ->
  MemoryErrorHeuristic sym ext 8 argTys ->
  [RefineHeuristic sym bak ext argTys]
pointerHeuristics la ptrHeuristic byteHeuristic =
  [ pointerHeuristic la ptrHeuristic byteHeuristic
  ]
 where

getConcretePointerBlock :: W4.IsExprBuilder sym => sym -> Mem.LLVMPtr sym w -> Maybe Natural
getConcretePointerBlock sym ptr = tryAnnotated <|> tryUnannotated
 where
  int = W4.natToIntegerPure (Mem.llvmPointerBlock ptr)
  tryAnnotated = fromIntegral <$> W4.asInteger int
  tryUnannotated = do
    term <- W4.getUnannotatedTerm sym int
    fromIntegral <$> W4.asInteger term

allocInfoFromPtr :: W4.IsExprBuilder sym => sym -> Mem.Mem sym -> Mem.LLVMPtr sym w -> Maybe (Mem.AllocInfo sym)
allocInfoFromPtr sym mem ptr = do
  int <- getConcretePointerBlock sym ptr
  Mem.possibleAllocInfo int (Mem.memAllocs mem)

-- | Check if this pointer is a global with the given mutability, and if so,
-- return the name of the associated global variable. Returns 'Nothing' if the
-- mutability is wrong, if the pointer is not a global, or if the pointer\'s
-- block or offset aren\'t concrete.
globalPtrName :: C.IsSymInterface sym => sym -> Mem.MemImpl sym -> Mem.Mutability -> Mem.LLVMPtr sym w -> Maybe String
globalPtrName sym mem mut ptr =
  case allocInfoFromPtr sym (Mem.memImplHeap mem) ptr of
    Just (Mem.AllocInfo Mem.GlobalAlloc _sz actualMut _align _loc) | mut == actualMut -> do
      L.Symbol nm <- Mem.isGlobalPointer (Mem.memImplSymbolMap mem) ptr
      Just nm
    _ -> Nothing

memOpPtrs :: Mem.MemoryOp sym w -> [Mem.LLVMPtr sym w]
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
  , Mem.HasPtrWidth wptr
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  , wptr ~ 64 -- TODO(lb): Why is this necessary?
  ) =>
  GreaseLogAction ->
  [RefineHeuristic sym bak LLVM argTys]
llvmHeuristics la =
  let ptrHeuristic :: MemoryErrorHeuristic sym LLVM wptr argTys
      ptrHeuristic = MemoryErrorHeuristic (\_sym _loc -> handleMemErr la)
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
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
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
    (Cursor.Last ts ~ Mem.LLVMPointerType (MC.ArchAddrWidth arch)) =>
    sym ->
    Cursor.Last ts ~ Mem.LLVMPointerType (MC.ArchAddrWidth arch) =>
    W4.ProgramLoc ->
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
      else handleMemErr la argNames args e sel
  handleMemErr' _ _ argNames args e@(AnyMemErrorMacaw _) sel = handleMemErr la argNames args e sel
