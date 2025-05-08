{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Grease.FunctionOverride
  ( builtinStubsOverrides
  , basicLLVMOverrides
  , builtinLLVMOverrides
  ) where

import Control.Lens ((^.), to)
import Data.Bool (Bool(..))
import Data.Maybe (Maybe(..), mapMaybe)
import Data.Vector qualified as Vec
import Prelude (($), (.), otherwise, toInteger)
import System.IO (IO)

import Control.Applicative (pure)
import Control.Monad.State (MonadState(..), StateT(..), evalStateT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>), fmap)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Semigroup ((<>))
import Text.Show (show)

-- bv-sized
import Data.BitVector.Sized qualified as BV

-- llvm-pretty
import Text.LLVM.AST qualified as L

-- parameterized-utils
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (toListFC)

-- crucible
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.Simulator qualified as C

-- crucible-llvm
import Lang.Crucible.LLVM.Intrinsics qualified as Mem
import Lang.Crucible.LLVM.Intrinsics.Cast qualified as Cast
import Lang.Crucible.LLVM.Intrinsics.Libc qualified as Libc
import Lang.Crucible.LLVM.Intrinsics.LLVM qualified as LLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Printf qualified as Printf
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.TypeContext qualified as TCtx

-- what4
import What4.Interface qualified as W4
import What4.FunctionName qualified as W4

-- macaw-base
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM

-- macaw-symbolic
import Data.Macaw.Symbolic qualified as Symbolic

-- stubs
import Stubs.FunctionOverride qualified as Stubs

import Grease.Macaw.Arch (ArchContext, archInfo)
import Grease.Macaw.Memory (loadConcreteString)
import Grease.Panic qualified as Panic
import Grease.Utility (OnlineSolverAndBackend, llvmOverrideName)

-- | All of the @stubs@ overrides that work across all supported configurations.
--
-- c.f. 'builtinLLVMOverrides', which includes all of the functions here (i.e.,
-- from libc) and then some (i.e., LLVM intrinsics).
builtinStubsOverrides ::
  forall sym bak p arch.
  ( C.IsSymBackend sym bak
  , ?memOpts :: Mem.MemOptions
  , ?lc :: TCtx.TypeContext
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  SymIO.LLVMFileSystem (MC.ArchAddrWidth arch) ->
  Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
builtinStubsOverrides bak mvar mmConf archCtx fs =
  customOvs <> fromLlvmOvs
  where
    -- Custom overrides that are only applicable at the machine code level (and
    -- therefore do not belong in crucible-llvm).
    customOvs :: Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
    customOvs =
      Seq.fromList
        [ -- Functions that need to read strings using the macaw-symbolic memory
          -- model
          Stubs.SomeFunctionOverride (buildAssertFailOverride mvar mmConf archCtx)
        , Stubs.SomeFunctionOverride (buildAssertRtnOverride mvar mmConf archCtx)
        , Stubs.SomeFunctionOverride (buildPutsOverride mvar mmConf archCtx)
        , Stubs.SomeFunctionOverride (buildPrintfOverride mvar mmConf archCtx)
        , Stubs.SomeFunctionOverride (buildPrintfChkOverride mvar mmConf archCtx)
          -- Functions that do not appear at the LLVM level but do appear at the
          -- machine code level
        , Stubs.SomeFunctionOverride buildStackChkFailOverride
        , Stubs.SomeFunctionOverride buildStackChkFailLocalOverride
        ]

    -- Overrides that arise from crucible-llvm.
    fromLlvmOvs :: Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
    fromLlvmOvs =
      -- We never need to make use of any non-standard IntrinsicsOptions.
      let ?intrinsicsOpts = Mem.defaultIntrinsicsOptions in
      Seq.fromList $
      mapMaybe
        (\(Mem.SomeLLVMOverride ov) -> llvmToStubsOverride bak mvar ov) $
      List.filter
        (\(Mem.SomeLLVMOverride ov) ->
          L.decName (Mem.llvmOverride_declare ov) `Set.notMember`
          excludedLibcOverrides)
        (libcOverrides fs)

    -- Overrides that we do not want to use at the binary level. If you add an
    -- override to this list, make sure to include a comment with the reason why
    -- the override is excluded.
    excludedLibcOverrides :: Set L.Symbol
    excludedLibcOverrides =
      -- These overrides read strings from memory in a way that is at odds with
      -- macaw-symbolic's lazy memory model (see gitlab#226). In particular,
      -- these use crucible-llvm's various operations for performing memory
      -- loads (`doLoad`, `loadRaw`, `loadString`, etc.). We have reimplemented
      -- a subset of these functions as machine code overrides (see `customOvs`
      -- above).
      Set.fromList
        [ "__assert_fail"
        , "__assert_rtn"
        , "__printf_chk"
        , "printf"
        , "puts"
        ]

-- | LLVM overrides that are unconditionally made available to all LLVM programs
-- (S-expression or bitcode module), including overrides.
--
-- This does not include \"polymorphic\" overrides, see 'builtinLLVMOverrides'
-- for those.
basicLLVMOverrides ::
  forall p sym ext w.
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (Mem.SomeLLVMOverride p sym ext)
basicLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = Mem.defaultIntrinsicsOptions in
  Seq.fromList $
    List.concat @[]
    [ libcOverrides fs
    , LLVM.basic_llvm_overrides
    ]

-- | Helper, not exported
--
-- LLVM overrides corresponding to functions defined in @libc@.
libcOverrides ::
  forall p sym ext w.
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , ?intrinsicsOpts :: Mem.IntrinsicsOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  [Mem.SomeLLVMOverride p sym ext]
libcOverrides fs =
  List.concat @[]
    [ Libc.libc_overrides
    , symioLlvmOverrides
    ]
  where
    symioLlvmOverrides :: [Mem.SomeLLVMOverride p sym ext]
    symioLlvmOverrides =
      [ Mem.SomeLLVMOverride $ SymIO.openFile fs
      , Mem.SomeLLVMOverride $ SymIO.closeFile fs
      , Mem.SomeLLVMOverride $ SymIO.readFileHandle fs
      , Mem.SomeLLVMOverride $ SymIO.writeFileHandle fs
      ]

-- | All of the @crucible-llvm@ overrides that work across all supported
-- configurations.
--
-- c.f. 'builtinStubsOverrides', which includes a subset of the functions here
-- (i.e., the ones from libc, but not the LLVM intrinsics).
builtinLLVMOverrides ::
  ( C.IsSymInterface sym
  , ?lc :: TCtx.TypeContext
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth w
  ) =>
  SymIO.LLVMFileSystem w ->
  Seq.Seq (Mem.OverrideTemplate p sym ext arch)
builtinLLVMOverrides fs =
  -- We never need to make use of any non-standard IntrinsicsOptions.
  let ?intrinsicsOpts = Mem.defaultIntrinsicsOptions in
  fmap (\(Mem.SomeLLVMOverride ov) -> Mem.basic_llvm_override ov) (basicLLVMOverrides fs)
    <> Seq.fromList (List.map (\(pfx, LLVM.Poly1LLVMOverride ov) -> Mem.polymorphic1_llvm_override pfx ov) LLVM.poly1_llvm_overrides)

-----
-- Turn LLVM overrides into Stubs FunctionOverrides
-----

-- | Helper to turn bitvector types into LLVM pointer types.
--
-- See module comment on "Lang.Crucible.LLVM.Intrinsics.Cast".
bvToPointer ::
  C.TypeRepr t ->
  C.Some C.TypeRepr
bvToPointer =
  \case
    C.BVRepr w -> C.Some (Mem.LLVMPointerRepr w)
    t -> C.Some t

-- | Transform an 'Mem.LLVMOverride' into a 'Stubs.SomeFunctionOverride'.
--
-- Does the necessary pipe-fitting of bitvectors to pointers, see module comment
-- on "Lang.Crucible.LLVM.Intrinsics.Cast".
--
-- This uses null/default values for several fields of 'Stubs.FunctionOverride'
-- that are more appropriate for overrides written in the S-expression syntax,
-- e.g., 'Stubs.functionGlobals'.
--
-- The override will not havoc any registers.
--
-- This will return 'Nothing' if we cannot convert the 'Mem.LLVMOverride'.
-- Currently, this only happens if the 'Mem.LLVMOverride' is for a variadic
-- function, as the approach that 'Mem.LLVMOverride' uses to represent varargs
-- is not easily convertible to the approach that @stubs@ uses.
llvmToStubsOverride ::
  C.IsSymBackend sym bak =>
  Mem.HasLLVMAnn sym =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Mem.LLVMOverride p sym (Symbolic.MacawExt arch) args ret ->
  Maybe (Stubs.SomeFunctionOverride p sym arch)
llvmToStubsOverride bak mvar llvmOv
  | isVariadic
  = Nothing

  | otherwise
  = Just $ runIdentity $ do
      C.Some args <- Identity $ Ctx.fromList (toListFC bvToPointer args0)
      C.Some ret <- Identity $ bvToPointer ret0

      let nm = llvmOverrideName llvmOv
      Identity $
        Stubs.SomeFunctionOverride $
          Stubs.FunctionOverride
          { Stubs.functionName = nm
          , Stubs.functionGlobals = Map.empty
          , Stubs.functionExterns = Map.empty
          , Stubs.functionArgTypes = args
          , Stubs.functionReturnType = ret
          , Stubs.functionAuxiliaryFnBindings = []
          , Stubs.functionForwardDeclarations = Map.empty
          , Stubs.functionOverride =
              \_bak argVals _getVarArg _parents -> do
                -- This won't panic because we're only using bvToPointer, and those casts
                -- are supported by Cast.
                let panic = Panic.panic "llvmToStubsOverride"
                let fargs =
                      case Cast.castLLVMArgs nm bak args0 args of
                        Left err -> panic (Cast.printValCastError err)
                        Right f -> f
                let fret =
                      case Cast.castLLVMRet nm bak ret0 ret of
                        Left err -> panic (Cast.printValCastError err)
                        Right f -> f

                argVals' <- Cast.applyArgCast fargs argVals
                retVal <- Mem.llvmOverride_def llvmOv mvar argVals'
                retVal' <- Cast.applyValCast fret retVal
                let regChanges = []  -- TODO: havoc registers?
                pure (Stubs.OverrideResult regChanges retVal')
          }
  where
    args0 = Mem.llvmOverride_args llvmOv
    ret0 = Mem.llvmOverride_ret llvmOv

    -- crucible-llvm maintains the convention that any LLVMOverride for a
    -- variadic function (e.g., `printf`) will use `VectorRepr AnyRepr` as the
    -- last argument TypeRepr to represent the varargs' type.
    isVariadic :: Bool
    isVariadic =
      case Ctx.viewAssign args0 of
        Ctx.AssignExtend _ (C.VectorRepr C.AnyRepr) ->
          True
        _ ->
          False

-----
-- Custom stubs overrides
-----

-- | An override for the @__assert_fail@ function. This assumes that the fourth
-- argument points to an entirely concrete string.
--
-- This is defined separately from the
-- built-in LLVM override, as this override loads the string in a way that
-- respects the lazy @macaw-symbolic@ memory model.
buildAssertFailOverride ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    C.UnitType
buildAssertFailOverride mvar mmConf archCtx =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkFunctionOverride "__assert_fail" $ \bak args ->
    Ctx.uncurryAssignment (callAssert bak mvar mmConf archCtx) args

-- | An override for the @__assert_rtn@ function. This assumes that the fourth
-- argument points to an entirely concrete string.
--
-- This is defined separately from the
-- built-in LLVM override, as this override loads the string in a way that
-- respects the lazy @macaw-symbolic@ memory model.
buildAssertRtnOverride ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    C.UnitType
buildAssertRtnOverride mvar mmConf archCtx =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkFunctionOverride "__assert_rtn" $ \bak args ->
    Ctx.uncurryAssignment (callAssert bak mvar mmConf archCtx) args

-- | Call the @__assert_fail@ or @__assert_rtn@ function. These are internal
-- functions that the @assert@ function is liable to compile down to, depending
-- on the operating system.
--
-- This assumes that the fourth argument points to an entirely concrete string.
callAssert ::
  ( OnlineSolverAndBackend solver sym bak t st fm
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) r args ret ()
callAssert bak mvar mmConf archCtx _pfn _pfile _pline ptxt = do
  let sym = C.backendGetSym bak
  st0 <- get
  let endian = archCtx ^. archInfo . to MI.archEndianness
  let maxSize = Nothing
  (txt, st1) <- liftIO $ loadConcreteString bak mvar mmConf endian ptxt maxSize st0
  put st1
  let err = C.AssertFailureSimError "Call to assert()" (BSC.unpack txt)
  _ <- liftIO $ C.addFailedAssertion bak err
  loc <- liftIO $ W4.getCurrentProgramLoc sym
  liftIO $ C.abortExecBecause $ C.EarlyExit loc

-- | An override for the @printf@ function. This assumes that the first argument
-- points to an entirely concrete formatting string.
--
-- This is defined separately from the
-- built-in LLVM override, as this override loads the string in a way that
-- respects the lazy @macaw-symbolic@ memory model.
buildPrintfOverride ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPrintfOverride mvar mmConf archCtx =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkVariadicFunctionOverride "printf" $ \bak args gva ->
    Ctx.uncurryAssignment
      (\formatStrPtr -> callPrintf bak mvar mmConf archCtx formatStrPtr gva)
      args

-- | An override for the @__printf_chk@ function. This assumes that the first
-- argument points to an entirely concrete formatting string.
--
-- This is defined separately from the
-- built-in LLVM override, as this override loads the string in a way that
-- respects the lazy @macaw-symbolic@ memory model.
buildPrintfChkOverride ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
                    Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPrintfChkOverride mvar mmConf archCtx =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkVariadicFunctionOverride "__printf_chk" $ \bak args gva ->
    Ctx.uncurryAssignment
      (\_flg formatStrPtr -> callPrintf bak mvar mmConf archCtx formatStrPtr gva)
      args

-- | Call the @printf@ or @__printf_chk@ function. This assumes that the pointer
-- first argument points to an entirely concrete formatting string.
callPrintf ::
  ( OnlineSolverAndBackend solver sym bak t st fm
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  Stubs.GetVarArg sym ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) r args ret
    (Mem.LLVMPtr sym (MC.ArchAddrWidth arch))
callPrintf bak mvar mmConf archCtx formatStrPtr gva = do
  let sym = C.backendGetSym bak
  st0 <- get
  let endian = archCtx ^. archInfo . to MI.archEndianness
  let maxSize = Nothing
  -- Read format string
  (formatStr, st1) <- liftIO $
    loadConcreteString bak mvar mmConf endian formatStrPtr maxSize st0
  put st1
  -- Parse format directives
  case Printf.parseDirectives (BS.unpack formatStr) of
    Left err -> C.overrideError $
      C.AssertFailureSimError "Format string parsing failed" err
    Right ds -> do
      -- Get variadic arguments
      valist <- liftIO $ getPrintfVarArgs (Vec.fromList ds) gva
      mem0 <- C.readGlobal mvar
      -- Execute directives
      ((str, n), mem1) <-
        liftIO $
        runStateT
          (Printf.executeDirectives (Libc.printfOps bak valist) ds)
          mem0
      C.writeGlobal mvar mem1
      -- Print formatted output
      h <- C.printHandle <$> C.getContext
      liftIO $ BSC.hPutStrLn h str
      -- Return the number of characters printed
      nBv <- liftIO $ W4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (toInteger n))
      liftIO $ Mem.llvmPointer_bv sym nBv

-- | Given the directives in a @printf@-style format string, retrieve the
-- corresponding variadic arguments.
getPrintfVarArgs ::
  Mem.HasPtrWidth w =>
  Vec.Vector Printf.PrintfDirective ->
  Stubs.GetVarArg sym ->
  IO (Vec.Vector (C.AnyValue sym))
getPrintfVarArgs pds =
  evalStateT (Vec.mapMaybeM (StateT . getPrintfVarArg) pds)

-- | Given a single directive in a @printf@-style format string:
--
-- * If it is a conversion directive (i.e., beginning with a @%@ character),
--   retrieve a variadic argument @arg@ of the corresponding type and return
--   @('Just' arg, gva)@, where @gva@ is the callback for retrieving the next
--   variadic argument.
--
-- * Otherwise, return @('Nothing', gva)@.
getPrintfVarArg ::
  forall sym w.
  Mem.HasPtrWidth w =>
  Printf.PrintfDirective ->
  Stubs.GetVarArg sym ->
  IO (Maybe (C.AnyValue sym), Stubs.GetVarArg sym)
getPrintfVarArg pd gva@(Stubs.GetVarArg getVarArg) =
  case pd of
    Printf.StringDirective{} -> pure (Nothing, gva)
    Printf.ConversionDirective cd ->
      case Printf.printfType cd of
        Printf.Conversion_Integer{}    -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Char{}       -> getArgWithType Mem.PtrRepr
        Printf.Conversion_String{}     -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Pointer{}    -> getArgWithType Mem.PtrRepr
        Printf.Conversion_CountChars{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Floating{}   -> getArgWithType $ C.FloatRepr C.DoubleFloatRepr
  where
    getArgWithType ::
      forall arg.
      C.TypeRepr arg ->
      IO (Maybe (C.AnyValue sym), Stubs.GetVarArg sym)
    getArgWithType tpRepr = do
      (C.RegEntry ty val, gva') <- getVarArg tpRepr
      pure (Just (C.AnyValue ty val), gva')

-- | An override for the @puts@ function. This assumes that the pointer argument
-- points to an entirely concrete string.
--
-- This is defined separately from the
-- built-in LLVM override, as this override loads the string in a way that
-- respects the lazy @macaw-symbolic@ memory model.
buildPutsOverride ::
  ( Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPutsOverride mvar mmConf archCtx =
  W4.withKnownNat ?ptrWidth $
  Stubs.mkFunctionOverride "puts" $ \bak args ->
    Ctx.uncurryAssignment (callPuts bak mvar mmConf archCtx) args

-- | Call the @puts@ function. This assumes that the pointer argument points to
-- an entirely concrete string.
callPuts ::
  ( OnlineSolverAndBackend solver sym bak t st fm
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  ArchContext arch ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) r args ret
    (Mem.LLVMPtr sym (MC.ArchAddrWidth arch))
callPuts bak mvar mmConf archCtx (C.regValue -> strPtr) = do
  let sym = C.backendGetSym bak
  st0 <- get
  let endian = archCtx ^. archInfo . to MI.archEndianness
  let strEntry = C.RegEntry Mem.PtrRepr strPtr
  let maxSize = Nothing
  (str, st1) <- liftIO $ loadConcreteString bak mvar mmConf endian strEntry maxSize st0
  put st1
  h <- C.printHandle <$> C.getContext
  liftIO $ BSC.hPutStrLn h str
  -- return non-negative value on success
  oneBv <- liftIO $ W4.bvOne (C.backendGetSym bak) MM.memWidthNatRepr
  liftIO $ Mem.llvmPointer_bv sym oneBv

-- | An override for @__stack_chk_fail@, which is called by functions that fail
-- stack protection checks. (See @Note [Coping with stack protection]@ in
-- "Grease.Macaw.Arch").
buildStackChkFailOverride ::
  Stubs.FunctionOverride p sym Ctx.EmptyCtx arch C.UnitType
buildStackChkFailOverride =
  let fnName = "__stack_chk_fail" in
  Stubs.mkFunctionOverride fnName $ \_bak Ctx.Empty ->
    callStackChkFail fnName

-- | An override for @__stack_chk_fail_local@. This behaves identically to the
-- @__stack_chk_fail@ function (see 'buildStackChkFailOverride'), but this
-- function might be called instead of @__stack_chk_fail@ in certain
-- circumstances. (See @Note [Coping with stack protection]@ in
-- "Grease.Macaw.Arch").
buildStackChkFailLocalOverride ::
  Stubs.FunctionOverride p sym Ctx.EmptyCtx arch C.UnitType
buildStackChkFailLocalOverride =
  let fnName = "__stack_chk_fail_local" in
  Stubs.mkFunctionOverride fnName $ \_bak Ctx.Empty ->
    callStackChkFail fnName

-- | Call a function in the @__stack_chk_fail@ family.
callStackChkFail :: W4.FunctionName -> C.OverrideSim p sym ext r args ret ()
callStackChkFail fnName =
  C.ovrWithBackend $ \bak -> liftIO $ do
    let sym = C.backendGetSym bak
    let msg = "Call to " List.++ show fnName
    let err = C.AssertFailureSimError msg ""
    C.assert bak (W4.falsePred sym) err
    loc <- W4.getCurrentProgramLoc sym
    C.abortExecBecause $ C.EarlyExit loc
