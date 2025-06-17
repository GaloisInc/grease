{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Grease.Setup
  ( InitialMem(..)
  , SetupMem(..)
  , Args(..)
  , argTypes
  , argVals
  , argRegMap
  , setupShape
  , runSetup
  , setup
  , ValueName(..)
  ) where

import Control.Applicative (pure)
import Control.Exception.Safe (MonadCatch)
import Control.Lens (use, (^.), (%~), (.=))
import Control.Lens.TH (makeLenses)
import Control.Lens.Zoom (zoom)
import Control.Monad (foldM, (=<<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Data.BitVector.Sized qualified as BV
import Data.Eq (Eq((==)))
import Data.Function (($), (.), (&), flip)
import Data.Functor (fmap)
import Data.List qualified as List
import Data.Maybe (Maybe(..))
import Data.Parameterized.Classes (ixF')
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (NatRepr, natValue)
import Data.Parameterized.TraversableFC (fmapFC)
import Data.Proxy (Proxy(Proxy))
import Data.Semigroup ((<>))
import Data.Sequence qualified as Seq
import Data.String (String)
import Data.Type.Equality (type (~), (:~:)(Refl))
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Diagnostic (GreaseLogAction, Diagnostic(SetupDiagnostic))
import Grease.Setup.Annotations qualified as Anns
import Grease.Setup.Diagnostic qualified as Diag
import Grease.Shape
import Grease.Shape.Pointer
import Grease.Shape.Selector
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.Bytes (Bytes)
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.DataLayout qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Types qualified as C
import Lumberjack qualified as LJ
import Prelude (Int, Num(..), fromIntegral)
import System.IO (IO)
import Text.Show (show)
import What4.Interface qualified as W4
import qualified Data.Map as Map

-- | Name for fresh symbolic values, passed to 'W4.safeSymbol'. The phantom
-- type parameter prevents making recursive calls without changing the name.
newtype ValueName (t :: C.CrucibleType) = ValueName { getValueName :: String }

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

safeSymbol :: ValueName t -> W4.SolverSymbol
safeSymbol = W4.safeSymbol . getValueName

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (SetupDiagnostic diag)


type SetupRes sym w = (C.RegValue sym (Mem.LLVMPointerType w), PtrTarget w (C.RegValue' sym))
data SetupState sym ext argTys w = SetupState
  { _setupMem :: Mem.MemImpl sym
  , _setupAnns :: Anns.Annotations sym ext argTys
  , _setupRes :: Map.Map BlockId (SetupRes sym w)
  }
makeLenses ''SetupState

-- | Setup monad
type Setup sym ext argTys w a = StateT (SetupState sym ext argTys w) IO a

annotatePtrBv ::
  forall sym ext argTys ts regTy w  w'.
  ( C.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ Mem.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  C.RegValue sym (C.BVType w) ->
  Setup sym ext argTys w' (Mem.LLVMPtr sym w)
annotatePtrBv sym sel bv = do
  ptr <- liftIO (Mem.llvmPointer_bv sym bv)
  zoom setupAnns (Anns.annotatePtr sym sel ptr)

freshPtrBv ::
  forall sym ext argTys ts regTy w w'.
  ( C.IsSymInterface sym
  , Cursor.Last (regTy ': ts) ~ Mem.LLVMPointerType w
  , 1 C.<= w
  ) =>
  sym ->
  Selector ext argTys ts regTy ->
  ValueName (Mem.LLVMPointerType w) ->
  NatRepr w ->
  Setup sym ext argTys w' (Mem.LLVMPtr sym w)
freshPtrBv sym sel nm w =
  annotatePtrBv sym sel 
    =<< liftIO (W4.freshConstant sym (safeSymbol nm) (W4.BaseBVRepr w))

-- | Ignores tags.
setupPtr ::
  forall sym bak ext tag w argTys ts regTy.
  ( C.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  , Cursor.Last (regTy ': ts) ~ Mem.LLVMPointerType w
  ) =>
  GreaseLogAction ->
  bak ->
  Mem.DataLayout ->
  ValueName (Mem.LLVMPointerType w) ->
  Selector ext argTys ts regTy ->
  PtrTarget w tag ->
  Setup sym ext argTys w (C.RegValue sym (Mem.LLVMPointerType w), PtrTarget w (C.RegValue' sym))
setupPtr la bak layout nm sel target = do
  let align = Mem.maxAlignment layout
  let sym = C.backendGetSym bak
  case target of
    PtrTarget Seq.Empty bid -> do
      -- See Note [Initializing empty pointer shapes]
      ptr <- liftIO $ do
        offset <- W4.freshConstant sym (safeSymbol (addSuffix nm "_offset")) (W4.BaseBVRepr ?ptrWidth)
        Mem.llvmPointer_bv sym offset
      p <- zoom setupAnns (Anns.annotatePtr sym sel ptr)
      pure (p, PtrTarget Seq.Empty bid)
    PtrTarget ms bid -> do
      mem <- use setupMem 
      let bytes = ptrTargetSize ?ptrWidth target
      sz <- liftIO (W4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral bytes)))
      let loc = "grease setup (" <> show (ppSelector (PtrCursor.ppDereference @ext) sel) <> ")"
      (ptr, mem') <- liftIO $ Mem.doMalloc bak Mem.HeapAlloc Mem.Mutable loc mem sz align
      setupMem .= mem'
      -- write nested shapes to memory
      (_, _, ms') <- foldM go (0, ptr, Seq.empty) ms
      p <- zoom setupAnns (Anns.annotatePtr sym sel ptr)
      pure (p, PtrTarget ms' bid)
  where
    makeKnownBytes ::
      forall ts'.
      Cursor.Last (regTy ': ts') ~ Mem.LLVMPointerType w =>
      sym ->
      Selector ext argTys ts' regTy ->
      [Word8] ->
      Setup sym ext argTys w (Vec.Vector (Mem.LLVMPtr sym 8))
    makeKnownBytes sym sel' bytes =
      flip Vec.unfoldrM (List.zip [(1 :: Int)..] bytes) $
        \case
          [] -> pure Nothing
          (i, b):rest -> do
            let sel'' = sel' & selectorPath %~ PtrCursor.addByteIndex i
            Refl <- pure $ Cursor.lastSnoc (Proxy @(Mem.LLVMPointerType 8)) (Proxy @ts')
            Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @(Cursor.Snoc ts' (Mem.LLVMPointerType 8)))
            bv <- liftIO (W4.bvLit sym (C.knownNat @8) (BV.mkBV (C.knownNat @8) (fromIntegral b)))
            bv' <- annotatePtrBv sym sel'' bv
            pure (Just (bv', rest))

    writeKnownBytes ::
      forall ts'.
      Cursor.Last (regTy ': ts') ~ Mem.LLVMPointerType w =>
      C.IsSymBackend sym bak =>
      sym ->
      Mem.MemImpl sym ->
      Selector ext argTys ts' regTy ->
      -- | Pointer to write bytes to
      C.RegValue sym (Mem.LLVMPointerType w) ->
      [Word8] ->
      Setup sym ext argTys w (Mem.MemImpl sym, Vec.Vector (Mem.LLVMPtr sym 8))
    writeKnownBytes sym m sel' ptr bytes = do
      let i8 = Mem.bitvectorType (Bytes.toBytes (1 :: Int))
      vals <- makeKnownBytes sym sel' bytes
      let mkInt bv = Mem.LLVMValInt (Mem.llvmPointerBlock bv) (Mem.llvmPointerOffset bv)
      let val = Mem.LLVMValArray i8 (Vec.map mkInt vals)
      let storTy = Mem.arrayType (fromIntegral (List.length bytes)) i8
      m' <- liftIO $ Mem.storeRaw bak m ptr storTy Mem.noAlignment val
      pure (m', vals)

    makeFreshBytes ::
      forall ts'.
      Cursor.Last (regTy ': ts') ~ Mem.LLVMPointerType w =>
      sym ->
      Selector ext argTys ts' regTy ->
      Bytes ->
      Setup sym ext argTys w (Vec.Vector (Mem.LLVMPtr sym 8))
    makeFreshBytes sym sel' bytes =
      Vec.generateM (fromIntegral (Bytes.bytesToInteger bytes)) $ \i -> do
        let sel'' = sel' & selectorPath %~ PtrCursor.addByteIndex i
        Refl <- pure $ Cursor.lastSnoc (Proxy @(Mem.LLVMPointerType 8)) (Proxy @ts')
        Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @(Cursor.Snoc ts' (Mem.LLVMPointerType 8)))
        let nm' = addIndex nm i
        freshPtrBv sym sel'' nm' (W4.knownNat @8)

    writeFreshBytes ::
      forall ts'.
      Cursor.Last (regTy ': ts') ~ Mem.LLVMPointerType w =>
      C.IsSymBackend sym bak =>
      sym ->
      Mem.MemImpl sym ->
      Selector ext argTys ts' regTy ->
      -- | Pointer to write bytes to
      C.RegValue sym (Mem.LLVMPointerType w) ->
      Bytes ->
      Setup sym ext argTys  w (Mem.MemImpl sym, Vec.Vector (Mem.LLVMPtr sym 8))
    writeFreshBytes sym m sel' ptr bytes = do
      let i8 = Mem.bitvectorType (Bytes.toBytes (1 :: Int))
      vals <- makeFreshBytes sym sel' bytes
      let mkInt bv = Mem.LLVMValInt (Mem.llvmPointerBlock bv) (Mem.llvmPointerOffset bv)
      let val = Mem.LLVMValArray i8 (Vec.map mkInt vals)
      let storTy = Mem.arrayType (fromIntegral bytes) i8
      m' <- liftIO $ Mem.storeRaw bak m ptr storTy Mem.noAlignment val
      pure (m', vals)

    go ::
      Mem.HasPtrWidth w =>
      ( -- Index from base pointer, used for names
        Int
        -- Base pointer plus current offset
      , C.RegValue sym (Mem.LLVMPointerType w)
        -- Values written so far
      , Seq.Seq (MemShape w (C.RegValue' sym))
      ) ->
      MemShape w tag ->
      Setup sym ext argTys w (Int, C.RegValue sym (Mem.LLVMPointerType w), Seq.Seq (MemShape w (C.RegValue' sym)))
    go (idx, ptr, written) memShape = do
      let sym = C.backendGetSym bak
      let sel' = sel & selectorPath %~ PtrCursor.addIndex idx
      Refl <- pure $ Cursor.lastSnoc (Proxy @(Mem.LLVMPointerType w)) (Proxy @(regTy ': ts))
      memShape' <-
        case memShape of
          Exactly bytes -> do
            let bytes' = List.map taggedByteValue bytes
            m <- use setupMem
            (m', byteVals) <- writeKnownBytes sym m sel' ptr bytes'
            setupMem .= m'
            pure (Exactly (List.zipWith (\v -> TaggedByte (C.RV v)) (Vec.toList byteVals) bytes'))
          Uninitialized bytes -> pure (Uninitialized bytes)
          Initialized _tag bytes -> do
            m <- use setupMem
            if bytes == 0
            then pure (Initialized (C.RV Vec.empty) 0)
            else do
              (m', byteVals) <- writeFreshBytes sym m sel' ptr bytes
              setupMem .= m'
              pure (Initialized (C.RV byteVals) bytes)
          Pointer _tag off tgt -> do  -- recursive case
            let nm' = addIndex nm idx
            (val, tgt') <- setupPtr la bak layout nm' sel' tgt
            let storTy = Mem.bitvectorType (Bytes.bitsToBytes (natValue ?ptrWidth))
            m <- use setupMem
            m' <- liftIO $ Mem.doStore bak m ptr (Mem.LLVMPointerRepr ?ptrWidth) storTy Mem.noAlignment val
            setupMem .= m'
            pure (Pointer (C.RV val) off tgt')

      let offset = memShapeSize ?ptrWidth memShape
      offsetBv <- liftIO (W4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral offset)))
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

* Ease of implementation. It is easier to refine bitvectors into pointers than
  the opposite direction. Moreover, there are some operations which fail for
  pointers (e.g., adding two pointers in macaw-symbolic) that are somewhat
  difficult to intercept and trace back to the pointer arguments.

* Performance. crucible-llvm's memory model's performance on memory reads from
  pointers with symbolic block numbers can be quite bad in the general case, as
  crucible-llvm must construct a mux tree over all possible pointer values that
  are live at a given moment. Starting off with a bitvector value (with a
  concrete block number) mitigates this issue somewhat, as we can avoid symbolic
  block numbers at least some of the time.
-}

-- | Create 'C.RegValue's from a 'Shape'.
--
-- Ignores @tag@s.
setupShape ::
  forall sym bak ext tag w argTys ts regTy t.
  ( C.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
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
  Setup sym ext argTys w (Shape ext (C.RegValue' sym) t)
setupShape la bak layout nm tRepr sel s = do
  let sym = C.backendGetSym bak
  Refl <- pure $ Cursor.lastCons (Proxy @regTy) (Proxy @ts)
  case s of
    ShapeBool _tag -> do
      b <- liftIO (W4.freshConstant sym (safeSymbol nm) W4.BaseBoolRepr)
      b' <- zoom setupAnns (Anns.annotate sym sel b)
      pure (ShapeBool (C.RV b'))
    ShapeExt (ShapePtrBV _tag w) -> do
      bv <- freshPtrBv sym sel nm w
      pure (ShapeExt (ShapePtrBV (C.RV bv) w))
    ShapeExt (ShapePtrBVLit _tag w bv) -> do
      bv' <- liftIO (Mem.llvmPointer_bv sym =<< W4.bvLit sym w bv)
      pure (ShapeExt (ShapePtrBV (C.RV bv') w))
    ShapeExt (ShapePtr _tag offset target) -> do
      (basePtr, target') <- setupPtr la bak layout nm sel target
      offsetBv <- liftIO (W4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral (getOffset offset))))
      p <- liftIO (Mem.ptrAdd sym ?ptrWidth basePtr offsetBv)
      pure (ShapeExt (ShapePtr (C.RV p) offset target'))
    ShapeStruct _tag fs -> do
      fieldShapes <-
        Ctx.traverseWithIndex
          (\idx s' -> do
            let ty' = case tRepr of C.StructRepr fields -> fields ^. ixF' idx
            Refl <- pure $ Cursor.lastSnoc ty' (Proxy @(regTy ': ts))
            let sel' = sel & selectorPath %~ PtrCursor.addField ty' idx
            let nm' = addIndex nm (Ctx.indexVal idx)
            setupShape la bak layout nm' ty' sel' s')
          fs
      let vals = fmapFC (getTag getPtrTag) fieldShapes
      pure (ShapeStruct (C.RV vals) fieldShapes)
    ShapeUnit _tag -> pure (ShapeUnit (C.RV ()))

-- | Create 'C.RegValue's from 'Shape's.
--
-- Ignores @tag@s.
setupArgs ::
  forall sym bak ext tag argTys w.
  ( C.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
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
  fmap Args .
    Ctx.traverseWithIndex
      (\idx s ->
        let nm = argNames ^. ixF' idx
            ty = argTys ^. ixF' idx
            sel = SelectArg (ArgSelector idx (Cursor.Here ty))
        in setupShape la bak layout nm ty sel s)

-- | Memory before execution
--
-- Used by heuristics to look up the names of globals.
newtype InitialMem sym = InitialMem { getInitialMem :: Mem.MemImpl sym }

-- | Memory after running 'setup'
newtype SetupMem sym = SetupMem { getSetupMem :: Mem.MemImpl sym }

-- | Arguments used for an execution of the target
--
-- Used by refinement loop to print concrete examples.
newtype Args sym ext argTys
  = Args { getArgs :: Ctx.Assignment (Shape ext (C.RegValue' sym)) argTys }

argTypes ::
  ( Mem.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  Ctx.Assignment C.TypeRepr argTys
argTypes (Args args) = fmapFC (shapeType ptrShapeType) args

argVals ::
  ( Mem.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  Ctx.Assignment (C.RegValue' sym) argTys
argVals (Args args) = fmapFC (getTag getPtrTag) args

argRegMap ::
  forall sym ext w argTys.
  ( Mem.HasPtrWidth w
  , ExtShape ext ~ PtrShape ext w
  ) =>
  Args sym ext argTys ->
  C.RegMap sym argTys
argRegMap args =
  C.RegMap (Ctx.zipWith (\ty (C.RV v) -> C.RegEntry ty v) (argTypes args) (argVals args))

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
    initial = SetupState
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
  , C.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
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
  m ( Args sym ext argTys
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
