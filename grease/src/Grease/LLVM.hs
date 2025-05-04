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

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception.Safe (MonadThrow)
import Control.Lens ((^.))

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF

-- what4
import qualified What4.Expr as W4

-- crucible
import qualified Lang.Crucible.Analysis.Postdom as C
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import qualified Lang.Crucible.Simulator.GlobalState as C

-- crucible-llvm
import Lang.Crucible.LLVM.Extension (ArchWidth, LLVM)
import qualified Lang.Crucible.LLVM.MemModel as Mem
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.Translation as Trans
import qualified Lang.Crucible.LLVM.TypeContext as TCtx

import Grease.Diagnostic (GreaseLogAction)
import Grease.LLVM.SimulatorHooks (greaseLlvmExtImpl)
import Grease.Options (ErrorSymbolicFunCalls)
import Grease.Setup (SetupMem(getSetupMem))
import Grease.Utility (printHandle)

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
  -- | Additional Crucible intrinsic types to use.
  C.IntrinsicTypes sym ->
  C.HandleAllocator ->
  ErrorSymbolicFunCalls ->
  SetupMem sym ->
  C.SymGlobalState sym ->
  Trans.LLVMContext arch ->
  SetupHook sym ->
  -- | The initial arguments to the entrypoint function.
  Ctx.Assignment (C.RegValue' sym) argTys ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG LLVM argTys (C.StructType argTys)) ->
  -- | The CFG of the user-requested entrypoint function.
  C.SomeCFG LLVM argTys retTy ->
  m (C.ExecState () sym LLVM (C.RegEntry sym retTy))
initState bak la llvmExtImpl iTypes halloc errorSymbolicFunCalls mem globs llvmCtx setupHook initArgs mbStartupOvCfg (C.SomeCFG cfg) = do
  let dl = TCtx.llvmDataLayout (llvmCtx ^. Trans.llvmTypeCtx)
  let extImpl = greaseLlvmExtImpl la halloc dl errorSymbolicFunCalls llvmExtImpl
  let bindings = C.FnBindings
        $ C.insertHandleMap (C.cfgHandle cfg) (C.UseCFG cfg $ C.postdomInfo cfg) C.emptyHandleMap
  let ctx = C.initSimContext
        bak
        (CLLVM.llvmIntrinsicTypes `MapF.union` iTypes)
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
        hook bak halloc llvmCtx
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
