{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Setup (
  InitialMem (..),
  SetupMem (..),
  Args (..),
  argTypes,
  argVals,
  argRegMap,
  setupShape,
  runSetup,
  setup,
  ValueName (..),
) where

import Control.Exception.Safe (MonadCatch)
import Control.Lens (use, (%~), (.=), (^.))
import Control.Lens.TH (makeLenses)
import Control.Lens.Zoom (zoom)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State (StateT (..), evalStateT, get, put)
import Data.BitVector.Sized qualified as BV
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (NatRepr, natValue)
import Data.Parameterized.TraversableFC (fmapFC)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Type.Equality ((:~:) (Refl))
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Diagnostic (Diagnostic (SetupDiagnostic), GreaseLogAction)
import Grease.Setup.Annotations qualified as Anns
import Grease.Setup.Diagnostic qualified as Diag
import Grease.Shape
import Grease.Shape.Pointer
import Grease.Shape.Selector
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Bytes (Bytes)
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.DataLayout qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as C
import Lumberjack qualified as LJ
import What4.Interface qualified as WI

-- | Name for fresh symbolic values, passed to 'WI.safeSymbol'. The phantom
-- type parameter prevents making recursive calls without changing the name.
newtype ValueName (t :: C.CrucibleType) = ValueName {getValueName :: String}

addSuffix :: ValueName t -> String -> ValueName t'
addSuffix (ValueName nm) suf = ValueName (nm <> suf)

-- | Add an index (representing a byte offset or field index) to a value name.
--
-- Unfortunately, the list of allowed characters in solver symbols is quite
-- limited. If we use a disallowed character, the names get Z-encoded (e.g.,
-- @arg0[207]@ becomes @arg0ZM207ZN@), which can be confusing when they appear
-- in debug output. We separate names from indices with @_@, as it is allowed.
-- See gitlab#102.
addIndex :: ValueName t -> Int -> ValueName t'
addIndex (ValueName nm) i = ValueName (nm <> "_" <> show i)

safeSymbol :: ValueName t -> WI.SolverSymbol
safeSymbol = WI.safeSymbol . getValueName

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (SetupDiagnostic diag)

-- | The result of setting up a pointer. `setupResPtr` represents the runtime
-- value of the pointer and `setupResTgt` stores the runtime representation of
-- what is stored in that pointer. These results are memoized during setup based
-- on `BlockId` so that a given block is only given one runtime value.
data SetupRes sym w
  = SetupRes
  { setupResPtr :: CS.RegValue sym (CLM.LLVMPointerType w)
  , setupResTgt :: PtrTarget w (CS.RegValue' sym)
  }

data SetupState sym ext argTys w = SetupState
  { _setupMem :: CLM.MemImpl sym
  , _setupAnns :: Anns.Annotations sym ext argTys
  , _setupRes :: Map.Map BlockId (SetupRes sym w)
  -- ^ Memoization table that maps observed 'BlockId's from 'setupPtrMem' to
  -- observed 'SetupRes' results.
  }
makeLenses ''SetupState

-- | Setup monad
type Setup sym ext argTys w a = StateT (SetupState sym ext argTys w) IO a

annotatePtrBv ::
  forall sym ext argTys ts regTy w w'.
  ( CB.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ CLM.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  CS.RegValue sym (C.BVType w) ->
  Setup sym ext argTys w' (Mem.LLVMPtr sym w)
annotatePtrBv sym sel bv = do
  ptr <- liftIO (Mem.llvmPointer_bv sym bv)
  zoom setupAnns (Anns.annotatePtr sym sel ptr)

freshPtrBv ::
  forall sym ext argTys ts regTy w w'.
  ( CB.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ CLM.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  ValueName (CLM.LLVMPointerType w) ->
  NatRepr w ->
  Setup sym ext argTys w' (Mem.LLVMPtr sym w)
freshPtrBv sym sel nm w =
  annotatePtrBv sym sel
    =<< liftIO (WI.freshConstant sym (safeSymbol nm) (WI.BaseBVRepr w))

-- | Memoizes calls to `setupPtr`. Results are stored in the `SetupState` based on the `BlockId`.
-- Only a single runtime value is produced per `BlockId` allowing for mutliple pointers to the same block in a
-- shape.
setupPtrMem ::
  forall sym bak ext tag w argTys ts regTy.
  ( CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  , Cursor.Last (regTy ': ts) ~ CLM.LLVMPointerType w
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  ValueName (CLM.LLVMPointerType w) ->
  Selector ext argTys ts regTy ->
  PtrTarget w tag ->
  Setup sym ext argTys w (CS.RegValue sym (CLM.LLVMPointerType w), PtrTarget w (CS.RegValue' sym))
setupPtrMem la bak dl nm sel tgt@(PtrTarget bid _) =
  let unseenFallback = setupPtr la bak dl nm sel tgt
   in case bid of
        Just bid' -> do
          resMap <- use setupRes
          case Map.lookup bid' resMap of
            Just memoizeRes -> pure (setupResPtr memoizeRes, setupResTgt memoizeRes)
            Nothing -> do
              (ptr, rTgt) <- unseenFallback
              s <- get
              let res = SetupRes{setupResPtr = ptr, setupResTgt = rTgt}
              let newMap = Map.insert bid' res resMap
              _ <- put (s{_setupRes = newMap})
              pure (ptr, rTgt)
        Nothing -> unseenFallback

-- | Ignores tags.
setupPtr ::
  forall sym bak ext tag w argTys ts regTy.
  ( CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  , Cursor.Last (regTy ': ts) ~ CLM.LLVMPointerType w
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  ValueName (CLM.LLVMPointerType w) ->
  Selector ext argTys ts regTy ->
  PtrTarget w tag ->
  Setup sym ext argTys w (CS.RegValue sym (CLM.LLVMPointerType w), PtrTarget w (CS.RegValue' sym))
setupPtr la bak layout nm sel target = do
  let align = Mem.maxAlignment layout
  let sym = CB.backendGetSym bak
  case target of
    PtrTarget bid Seq.Empty -> do
      -- See Note [Initializing empty pointer shapes]
      ptr <- liftIO $ do
        offset <- WI.freshConstant sym (safeSymbol (addSuffix nm "_offset")) (WI.BaseBVRepr ?ptrWidth)
        Mem.llvmPointer_bv sym offset
      p <- zoom setupAnns (Anns.annotatePtr sym sel ptr)
      pure (p, PtrTarget bid Seq.Empty)
    PtrTarget bid ms -> do
      mem <- use setupMem
      let bytes = ptrTargetSize ?ptrWidth target
      sz <- liftIO (WI.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral bytes)))
      let loc = "grease setup (" <> show (ppSelector (PtrCursor.ppDereference @ext) sel) <> ")"
      (ptr, mem') <- liftIO $ CLM.doMalloc bak CLM.HeapAlloc CLM.Mutable loc mem sz align
      setupMem .= mem'
      -- write nested shapes to memory
      (_, _, ms') <- foldM go (0, ptr, Seq.empty) ms
      p <- zoom setupAnns (Anns.annotatePtr sym sel ptr)
      pure (p, PtrTarget bid ms')
 where
  makeKnownBytes ::
    forall ts'.
    Cursor.Last (regTy ': ts') ~ CLM.LLVMPointerType w =>
    sym ->
    Selector ext argTys ts' regTy ->
    [Word8] ->
    Setup sym ext argTys w (Vec.Vector (Mem.LLVMPtr sym 8))
  makeKnownBytes sym sel' bytes =
    flip Vec.unfoldrM (List.zip [(1 :: Int) ..] bytes) $
      \case
        [] -> pure Nothing
        (i, b) : rest -> do
          let sel'' = sel' & selectorPath %~ PtrCursor.addByteIndex i
          Refl <- pure $ Cursor.lastSnoc (Proxy @(CLM.LLVMPointerType 8)) (Proxy @ts')
          Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @(Cursor.Snoc ts' (CLM.LLVMPointerType 8)))
          bv <- liftIO (WI.bvLit sym (C.knownNat @8) (BV.mkBV (C.knownNat @8) (fromIntegral b)))
          bv' <- annotatePtrBv sym sel'' bv
          pure (Just (bv', rest))

  writeKnownBytes ::
    forall ts'.
    Cursor.Last (regTy ': ts') ~ CLM.LLVMPointerType w =>
    CB.IsSymBackend sym bak =>
    sym ->
    CLM.MemImpl sym ->
    Selector ext argTys ts' regTy ->
    -- Pointer to write bytes to
    CS.RegValue sym (CLM.LLVMPointerType w) ->
    [Word8] ->
    Setup sym ext argTys w (CLM.MemImpl sym, Vec.Vector (Mem.LLVMPtr sym 8))
  writeKnownBytes sym m sel' ptr bytes = do
    let i8 = CLM.bitvectorType (Bytes.toBytes (1 :: Int))
    vals <- makeKnownBytes sym sel' bytes
    let mkInt bv = CLM.LLVMValInt (Mem.llvmPointerBlock bv) (Mem.llvmPointerOffset bv)
    let val = CLM.LLVMValArray i8 (Vec.map mkInt vals)
    let storTy = CLM.arrayType (fromIntegral (List.length bytes)) i8
    m' <- liftIO $ CLM.storeRaw bak m ptr storTy Mem.noAlignment val
    pure (m', vals)

  makeFreshBytes ::
    forall ts'.
    Cursor.Last (regTy ': ts') ~ CLM.LLVMPointerType w =>
    sym ->
    Selector ext argTys ts' regTy ->
    Bytes ->
    Setup sym ext argTys w (Vec.Vector (Mem.LLVMPtr sym 8))
  makeFreshBytes sym sel' bytes =
    Vec.generateM (fromIntegral (Bytes.bytesToInteger bytes)) $ \i -> do
      let sel'' = sel' & selectorPath %~ PtrCursor.addByteIndex i
      Refl <- pure $ Cursor.lastSnoc (Proxy @(CLM.LLVMPointerType 8)) (Proxy @ts')
      Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @(Cursor.Snoc ts' (CLM.LLVMPointerType 8)))
      let nm' = addIndex nm i
      freshPtrBv sym sel'' nm' (WI.knownNat @8)

  writeFreshBytes ::
    forall ts'.
    Cursor.Last (regTy ': ts') ~ CLM.LLVMPointerType w =>
    CB.IsSymBackend sym bak =>
    sym ->
    CLM.MemImpl sym ->
    Selector ext argTys ts' regTy ->
    -- Pointer to write bytes to
    CS.RegValue sym (CLM.LLVMPointerType w) ->
    Bytes ->
    Setup sym ext argTys w (CLM.MemImpl sym, Vec.Vector (Mem.LLVMPtr sym 8))
  writeFreshBytes sym m sel' ptr bytes = do
    let i8 = CLM.bitvectorType (Bytes.toBytes (1 :: Int))
    vals <- makeFreshBytes sym sel' bytes
    let mkInt bv = CLM.LLVMValInt (Mem.llvmPointerBlock bv) (Mem.llvmPointerOffset bv)
    let val = CLM.LLVMValArray i8 (Vec.map mkInt vals)
    let storTy = CLM.arrayType (fromIntegral bytes) i8
    m' <- liftIO $ CLM.storeRaw bak m ptr storTy Mem.noAlignment val
    pure (m', vals)

  go ::
    CLM.HasPtrWidth w =>
    ( -- Index from base pointer, used for names
      Int
    , -- Base pointer plus current offset
      CS.RegValue sym (CLM.LLVMPointerType w)
    , -- Values written so far
      Seq.Seq (MemShape w (CS.RegValue' sym))
    ) ->
    MemShape w tag ->
    Setup sym ext argTys w (Int, CS.RegValue sym (CLM.LLVMPointerType w), Seq.Seq (MemShape w (CS.RegValue' sym)))
  go (idx, ptr, written) memShape = do
    let sym = CB.backendGetSym bak
    let sel' = sel & selectorPath %~ PtrCursor.addIndex idx
    Refl <- pure $ Cursor.lastSnoc (Proxy @(CLM.LLVMPointerType w)) (Proxy @(regTy ': ts))
    memShape' <-
      case memShape of
        Exactly bytes -> do
          let bytes' = List.map taggedByteValue bytes
          m <- use setupMem
          (m', byteVals) <- writeKnownBytes sym m sel' ptr bytes'
          setupMem .= m'
          pure (Exactly (List.zipWith (\v -> TaggedByte (CS.RV v)) (Vec.toList byteVals) bytes'))
        Uninitialized bytes -> pure (Uninitialized bytes)
        Initialized _tag bytes -> do
          m <- use setupMem
          if bytes == 0
            then pure (Initialized (CS.RV Vec.empty) 0)
            else do
              (m', byteVals) <- writeFreshBytes sym m sel' ptr bytes
              setupMem .= m'
              pure (Initialized (CS.RV byteVals) bytes)
        Pointer _tag off tgt -> do
          -- recursive case
          let nm' = addIndex nm idx
          (val, tgt') <- setupPtrMem la bak layout nm' sel' tgt
          let storTy = CLM.bitvectorType (Bytes.bitsToBytes (natValue ?ptrWidth))
          m <- use setupMem
          offsetBv <- liftIO (WI.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral (getOffset off))))
          val' <- liftIO $ Mem.ptrAdd sym ?ptrWidth val offsetBv
          m' <- liftIO $ CLM.doStore bak m ptr (CLM.LLVMPointerRepr ?ptrWidth) storTy Mem.noAlignment val'
          setupMem .= m'
          pure (Pointer (CS.RV val) off tgt')

    let offset = memShapeSize ?ptrWidth memShape
    offsetBv <- liftIO (WI.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral offset)))
    ptr' <- liftIO $ Mem.ptrAdd sym ?ptrWidth ptr offsetBv
    pure (idx + 1, ptr', memShape' Seq.<| written)

{-
Note [Initializing empty pointer shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During setup, we have a choice as how to initialize a PtrTarget with an empty
sequence of MemShapes:

1. Initialize it with a symbolic pointer (i.e., a symbolic block number and
   symbolic offset).

2. Initialize it with a symbolic bitvector (i.e., a block number of 0 and a
   symbolic offset).

We could make either choice. There are certain operations that will fail if
supplied a pointer or a bitvector, but grease has heuristics that can catch the
errors that these operations throw and re-initialize them with a different
PtrTarget shape. (For example, crucible-llvm will fail if it attempts to invoke
a function pointer where the function pointer value is a bitvector, but we can
catch the BadFunctionPointer error it throws, convert the function pointer
value from a bitvector into an actual pointer, and retry the refinement loop.)

In practice, we pick option (2). There are a couple of reasons for this:

\* Ease of implementation. It is easier to refine bitvectors into pointers than
  the opposite direction. Moreover, there are some operations which fail for
  pointers (e.g., adding two pointers in macaw-symbolic) that are somewhat
  difficult to intercept and trace back to the pointer arguments.

\* Performance. crucible-llvm's memory model's performance on memory reads from
  pointers with symbolic block numbers can be quite bad in the general case, as
  crucible-llvm must construct a mux tree over all possible pointer values that
  are live at a given moment. Starting off with a bitvector value (with a
  concrete block number) mitigates this issue somewhat, as we can avoid symbolic
  block numbers at least some of the time.

It is worth noting that GREASE's default heuristics do not make empty sequences
of MemShapes exactly because their treatment is ambiguous in this way.
-}

-- | Create 'CS.RegValue's from a 'Shape'.
--
-- Ignores @tag@s.
setupShape ::
  forall sym bak ext tag w argTys ts regTy t.
  ( CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  , Cursor.Last (regTy ': ts) ~ t
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  ValueName t ->
  C.TypeRepr t ->
  Selector ext argTys ts regTy ->
  Shape ext tag t ->
  Setup sym ext argTys w (Shape ext (CS.RegValue' sym) t)
setupShape la bak layout nm tRepr sel s = do
  let sym = CB.backendGetSym bak
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  case s of
    ShapeBool _tag -> do
      b <- liftIO (WI.freshConstant sym (safeSymbol nm) WI.BaseBoolRepr)
      b' <- zoom setupAnns (Anns.annotate sym sel b)
      pure (ShapeBool (CS.RV b'))
    ShapeFloat _tag fi -> do
      f <- liftIO $ C.freshFloatConstant sym (safeSymbol nm) fi
      pure (ShapeFloat (CS.RV f) fi)
    ShapeExt (ShapePtrBV _tag w) -> do
      bv <- freshPtrBv sym sel nm w
      pure (ShapeExt (ShapePtrBV (CS.RV bv) w))
    ShapeExt (ShapePtrBVLit _tag w bv) -> do
      bv' <- liftIO (Mem.llvmPointer_bv sym =<< WI.bvLit sym w bv)
      pure (ShapeExt (ShapePtrBV (CS.RV bv') w))
    ShapeExt (ShapePtr _tag offset target) -> do
      (basePtr, target') <- setupPtrMem la bak layout nm sel target
      offsetBv <- liftIO (WI.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral (getOffset offset))))
      p <- liftIO (Mem.ptrAdd sym ?ptrWidth basePtr offsetBv)
      pure (ShapeExt (ShapePtr (CS.RV p) offset target'))
    ShapeStruct _tag fs -> do
      fieldShapes <-
        Ctx.traverseWithIndex
          ( \idx s' -> do
              let ty' = case tRepr of C.StructRepr fields -> fields ^. ixF' idx
              Refl <- pure $ Cursor.lastSnoc ty' (Proxy @(regTy ': ts))
              let sel' = sel & selectorPath %~ PtrCursor.addField ty' idx
              let nm' = addIndex nm (Ctx.indexVal idx)
              setupShape la bak layout nm' ty' sel' s'
          )
          fs
      let vals = fmapFC (getTag getPtrTag) fieldShapes
      pure (ShapeStruct (CS.RV vals) fieldShapes)
    ShapeUnit _tag -> pure (ShapeUnit (CS.RV ()))

-- | Create 'CS.RegValue's from 'Shape's.
--
-- Ignores @tag@s.
setupArgs ::
  forall sym bak ext tag argTys w.
  ( CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  Ctx.Assignment ValueName argTys ->
  Ctx.Assignment C.TypeRepr argTys ->
  Ctx.Assignment (Shape ext tag) argTys ->
  Setup sym ext argTys w (Args sym ext argTys)
setupArgs la bak layout argNames argTys =
  fmap Args
    . Ctx.traverseWithIndex
      ( \idx s ->
          let nm = argNames ^. ixF' idx
              ty = argTys ^. ixF' idx
              sel = SelectArg (ArgSelector idx (Cursor.Here ty))
           in setupShape la bak layout nm ty sel s
      )

-- | Memory before execution
--
-- Used by heuristics to look up the names of globals.
newtype InitialMem sym = InitialMem {getInitialMem :: CLM.MemImpl sym}

-- | Memory after running 'setup'
newtype SetupMem sym = SetupMem {getSetupMem :: CLM.MemImpl sym}

-- | Arguments used for an execution of the target
--
-- Used by refinement loop to print concrete examples.
newtype Args sym ext argTys
  = Args {getArgs :: Ctx.Assignment (Shape ext (CS.RegValue' sym)) argTys}

argTypes ::
  ( CLM.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  Ctx.Assignment C.TypeRepr argTys
argTypes (Args args) = fmapFC (shapeType ptrShapeType) args

argVals ::
  ( CLM.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  Ctx.Assignment (CS.RegValue' sym) argTys
argVals (Args args) = fmapFC (getTag getPtrTag) args

argRegMap ::
  forall sym ext w argTys.
  ( CLM.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  CS.RegMap sym argTys
argRegMap args =
  CS.RegMap (Ctx.zipWith (\ty (CS.RV v) -> CS.RegEntry ty v) (argTypes args) (argVals args))

runSetup ::
  ( MonadIO m
  , MonadCatch m
  ) =>
  InitialMem sym ->
  Setup sym ext argTys w a ->
  m a
runSetup mem act =
  liftIO (evalStateT act initial)
 where
  initial =
    SetupState
      { _setupMem = getInitialMem mem
      , _setupAnns = Anns.empty
      , _setupRes = Map.empty
      }

-- | Create symbolic values ('Args') from 'Shape's.
--
-- Ignores @tag@s.
setup ::
  ( MonadIO m
  , MonadCatch m
  , CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  Ctx.Assignment ValueName argTys ->
  Ctx.Assignment C.TypeRepr argTys ->
  ArgShapes ext tag argTys ->
  InitialMem sym ->
  m
    ( Args sym ext argTys
    , SetupMem sym
    , Anns.Annotations sym ext argTys
    )
setup la bak layout argNames argTys (ArgShapes shapes) mem = do
  let initial =
        SetupState
          { _setupMem = getInitialMem mem
          , _setupAnns = Anns.empty
          , _setupRes = Map.empty
          }
  (result, state) <-
    liftIO . flip runStateT initial $ do
      args <- setupArgs la bak layout argNames argTys shapes
      mem' <- use setupMem
      liftIO (doLog la (Diag.SetupMem mem'))
      pure args
  pure (result, SetupMem (state ^. setupMem), state ^. setupAnns)
