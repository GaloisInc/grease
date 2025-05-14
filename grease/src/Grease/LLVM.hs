{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Grease.LLVM
  ( SetupHook(..)
  , initState
  ) where

import Control.Exception.Safe (MonadThrow)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Grease.Diagnostic (GreaseLogAction)
import Grease.LLVM.SimulatorHooks (greaseLlvmExtImpl)
import Grease.Options (ErrorSymbolicFunCalls)
import Grease.Setup (SetupMem(getSetupMem))
import Grease.Utility (printHandle)
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Extension (ArchWidth, LLVM)
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.Translation qualified as Trans
import Lang.Crucible.LLVM.TypeContext qualified as TCtx
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.GlobalState qualified as C
import What4.Expr qualified as W4

-- | Hook to run before executing a CFG.
--
-- Note that @sym@ is a type parameter so that users can define 'SetupHook's
-- that reference a fixed @sym@ type.
newtype SetupHook sym
  = SetupHook
    (forall arch p bak rtp a r.
      ( C.IsSymBackend sym bak
      , 16 C.<= ArchWidth arch
      , ArchWidth arch ~ 64
      , Mem.HasPtrWidth (ArchWidth arch)
      , Mem.HasLLVMAnn sym
      , ?lc :: TCtx.TypeContext
      , ?memOpts :: Mem.MemOptions
      , ?intrinsicsOpts :: CLLVM.IntrinsicsOptions
      ) =>
      bak ->
      C.HandleAllocator ->
      Trans.LLVMContext arch ->
      SymIO.LLVMFileSystem (ArchWidth arch) ->
      C.OverrideSim p sym LLVM rtp a r ())

initState ::
  forall sym bak arch m t st fs argTys retTy.
  ( MonadIO m
  , MonadThrow m
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st fs
  , 16 C.<= ArchWidth arch
  , ArchWidth arch ~ 64
  , Mem.HasPtrWidth (ArchWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  C.ExtensionImpl () sym LLVM ->
  C.HandleAllocator ->
  ErrorSymbolicFunCalls ->
  SetupMem sym ->
  SymIO.LLVMFileSystem (ArchWidth arch) ->
  C.SymGlobalState sym ->
  SymIO.SomeOverrideSim sym () ->
  Trans.LLVMContext arch ->
  SetupHook sym ->
  -- | The initial arguments to the entrypoint function.
  Ctx.Assignment (C.RegValue' sym) argTys ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG LLVM argTys (C.StructType argTys)) ->
  -- | The CFG of the user-requested entrypoint function.
  C.SomeCFG LLVM argTys retTy ->
  m (C.ExecState () sym LLVM (C.RegEntry sym retTy))
initState bak la llvmExtImpl halloc errorSymbolicFunCalls mem fs globs (SymIO.SomeOverrideSim initFsOv) llvmCtx setupHook initArgs mbStartupOvCfg (C.SomeCFG cfg) = do
  let dl = TCtx.llvmDataLayout (llvmCtx ^. Trans.llvmTypeCtx)
  let extImpl = greaseLlvmExtImpl la halloc dl errorSymbolicFunCalls llvmExtImpl
  let bindings = C.FnBindings
        $ C.insertHandleMap (C.cfgHandle cfg) (C.UseCFG cfg $ C.postdomInfo cfg) C.emptyHandleMap
  let ctx = C.initSimContext
        bak
        (CLLVM.llvmIntrinsicTypes `MapF.union` SymIO.llvmSymIOIntrinsicTypes)
        halloc
        printHandle
        bindings
        extImpl
        ()
  let argTys = C.cfgArgTypes cfg
  let args =
        Ctx.generate
          (Ctx.size argTys)
          (\i -> C.RegEntry (argTys ^. C.ixF' i) (C.unRV (initArgs ^. C.ixF' i)))
  let mvar = Trans.llvmMemVar llvmCtx
  pure $
    C.InitialState ctx
      (C.insertGlobal mvar (getSetupMem mem) globs)
      C.defaultAbortHandler
      (C.cfgReturnType cfg)
      (C.runOverrideSim (C.cfgReturnType cfg) $ do
        let ?lc = llvmCtx ^. Trans.llvmTypeCtx
        let ?intrinsicsOpts = CLLVM.defaultIntrinsicsOptions
        let SetupHook hook = setupHook
        hook bak halloc llvmCtx fs
        initFsOv
        r <-
          case mbStartupOvCfg of
            Nothing ->
              C.callCFG cfg (C.RegMap args)
            Just (C.SomeCFG startupOvCfg) -> do
              args' <- C.callCFG startupOvCfg (C.RegMap args)
              C.StructRepr argTys' <- pure $ C.regType args'
              C.callCFG cfg
                $ C.RegMap
                $ Ctx.zipWith
                    (\argTy (C.RV arg) -> C.RegEntry argTy arg)
                    argTys'
                    (C.regValue args')
        pure $ C.regValue r)
