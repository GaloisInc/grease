{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Custom stubs overrides.
module Grease.Macaw.Overrides.Defs (
  customStubsOverrides,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT)
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Vector qualified as Vec
import Grease.Macaw.Memory (loadConcreteString)
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Intrinsics.Libc qualified as Libc
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Printf qualified as Printf
import Lang.Crucible.Simulator qualified as C
import Stubs.FunctionOverride qualified as Stubs
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4

-- | Custom overrides that are only applicable at the machine code level (and
-- therefore do not belong in crucible-llvm).
customStubsOverrides ::
  ( ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  ) =>
  C.GlobalVar Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  [Stubs.SomeFunctionOverride p sym arch]
customStubsOverrides mvar mmConf =
  [ -- Functions that need to read strings using the macaw-symbolic memory
    -- model
    Stubs.SomeFunctionOverride (buildAssertFailOverride mvar mmConf)
  , Stubs.SomeFunctionOverride (buildAssertRtnOverride mvar mmConf)
  , Stubs.SomeFunctionOverride (buildPutsOverride mvar mmConf)
  , Stubs.SomeFunctionOverride (buildPrintfOverride mvar mmConf)
  , Stubs.SomeFunctionOverride (buildPrintfChkOverride mvar mmConf)
  , -- Functions that do not appear at the LLVM level but do appear at the
    -- machine code level
    Stubs.SomeFunctionOverride buildStackChkFailOverride
  , Stubs.SomeFunctionOverride buildStackChkFailLocalOverride
  ]

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
  Stubs.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
    )
    arch
    C.UnitType
buildAssertFailOverride mvar mmConf =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkFunctionOverride "__assert_fail" $ \bak args ->
      Ctx.uncurryAssignment (callAssert bak mvar mmConf) args

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
  Stubs.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
    )
    arch
    C.UnitType
buildAssertRtnOverride mvar mmConf =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkFunctionOverride "__assert_rtn" $ \bak args ->
      Ctx.uncurryAssignment (callAssert bak mvar mmConf) args

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
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) r args ret ()
callAssert bak mvar mmConf _pfn _pfile _pline ptxt = do
  let sym = C.backendGetSym bak
  st0 <- get
  let maxSize = Nothing
  (txt, st1) <- liftIO $ loadConcreteString bak mvar mmConf ptxt maxSize st0
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
  Stubs.FunctionOverride
    p
    sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPrintfOverride mvar mmConf =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkVariadicFunctionOverride "printf" $ \bak args gva ->
      Ctx.uncurryAssignment
        (\formatStrPtr -> callPrintf bak mvar mmConf formatStrPtr gva)
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
  Stubs.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
        Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch)
    )
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPrintfChkOverride mvar mmConf =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkVariadicFunctionOverride "__printf_chk" $ \bak args gva ->
      Ctx.uncurryAssignment
        (\_flg formatStrPtr -> callPrintf bak mvar mmConf formatStrPtr gva)
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
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  Stubs.GetVarArg sym ->
  C.OverrideSim
    p
    sym
    (Symbolic.MacawExt arch)
    r
    args
    ret
    (Mem.LLVMPtr sym (MC.ArchAddrWidth arch))
callPrintf bak mvar mmConf formatStrPtr gva = do
  let sym = C.backendGetSym bak
  st0 <- get
  let maxSize = Nothing
  -- Read format string
  (formatStr, st1) <-
    liftIO $
      loadConcreteString bak mvar mmConf formatStrPtr maxSize st0
  put st1
  -- Parse format directives
  case Printf.parseDirectives (BS.unpack formatStr) of
    Left err ->
      C.overrideError $
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
        Printf.Conversion_Integer{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Char{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_String{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Pointer{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_CountChars{} -> getArgWithType Mem.PtrRepr
        Printf.Conversion_Floating{} -> getArgWithType $ C.FloatRepr C.DoubleFloatRepr
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
  Stubs.FunctionOverride
    p
    sym
    (Ctx.EmptyCtx Ctx.::> Mem.LLVMPointerType (MC.ArchAddrWidth arch))
    arch
    (Mem.LLVMPointerType (MC.ArchAddrWidth arch))
buildPutsOverride mvar mmConf =
  W4.withKnownNat ?ptrWidth $
    Stubs.mkFunctionOverride "puts" $ \bak args ->
      Ctx.uncurryAssignment (callPuts bak mvar mmConf) args

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
  C.RegEntry sym (Mem.LLVMPointerType (MC.ArchAddrWidth arch)) ->
  C.OverrideSim
    p
    sym
    (Symbolic.MacawExt arch)
    r
    args
    ret
    (Mem.LLVMPtr sym (MC.ArchAddrWidth arch))
callPuts bak mvar mmConf strPtr = do
  let sym = C.backendGetSym bak
  st0 <- get
  let strEntry = C.RegEntry Mem.PtrRepr (C.regValue strPtr)
  let maxSize = Nothing
  (str, st1) <- liftIO $ loadConcreteString bak mvar mmConf strEntry maxSize st0
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
  let fnName = "__stack_chk_fail"
   in Stubs.mkFunctionOverride fnName $ \_bak Ctx.Empty ->
        callStackChkFail fnName

-- | An override for @__stack_chk_fail_local@. This behaves identically to the
-- @__stack_chk_fail@ function (see 'buildStackChkFailOverride'), but this
-- function might be called instead of @__stack_chk_fail@ in certain
-- circumstances. (See @Note [Coping with stack protection]@ in
-- "Grease.Macaw.Arch").
buildStackChkFailLocalOverride ::
  Stubs.FunctionOverride p sym Ctx.EmptyCtx arch C.UnitType
buildStackChkFailLocalOverride =
  let fnName = "__stack_chk_fail_local"
   in Stubs.mkFunctionOverride fnName $ \_bak Ctx.Empty ->
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
