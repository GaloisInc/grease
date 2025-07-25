{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

-- | c.f. "Grease.Macaw.SetupHook"
module Grease.LLVM.SetupHook (
  SetupHook (..),
  syntaxSetupHook,
  moduleSetupHook,
) where

import Control.Lens ((^.))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Diagnostic (Diagnostic (LLVMSetupHookDiagnostic), GreaseLogAction)
import Grease.Entrypoint qualified as GE
import Grease.LLVM.Overrides (registerLLVMSexpOverrides)
import Grease.LLVM.Overrides qualified as GLO
import Grease.LLVM.Overrides.Builtin (builtinLLVMOverrides)
import Grease.LLVM.SetupHook.Diagnostic qualified as Diag
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM qualified as CLLVM
import Lang.Crucible.LLVM.Extension (ArchWidth, LLVM)
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.Translation qualified as Trans
import Lang.Crucible.LLVM.TypeContext qualified as TCtx
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lumberjack qualified as LJ

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LLVMSetupHookDiagnostic diag)

-- | Hook to run before executing a CFG.
--
-- Note that @sym@ is a type parameter so that users can define 'SetupHook's
-- that reference a fixed @sym@ type. Same with @arch@.
--
-- c.f. 'Grease.Macaw.SetupHook.SetupHook'
newtype SetupHook sym arch
  = SetupHook
      ( forall p bak rtp a r.
        ( C.IsSymBackend sym bak
        , ArchWidth arch ~ 64
        , Mem.HasPtrWidth (ArchWidth arch)
        , Mem.HasLLVMAnn sym
        , HasToConcretize p
        , ?lc :: TCtx.TypeContext
        , ?memOpts :: Mem.MemOptions
        , ?intrinsicsOpts :: CLLVM.IntrinsicsOptions
        ) =>
        bak ->
        C.HandleAllocator ->
        Trans.LLVMContext arch ->
        SymIO.LLVMFileSystem (ArchWidth arch) ->
        C.OverrideSim p sym LLVM rtp a r ()
      )

-- | A 'SetupHook' for LLVM CFGs from S-expression programs.
syntaxSetupHook ::
  GreaseLogAction ->
  -- | Files containing S-expression overrides
  [FilePath] ->
  CSyn.ParsedProgram LLVM ->
  Map GE.Entrypoint (GE.EntrypointCfgs (C.AnyCFG CLLVM.LLVM)) ->
  SetupHook sym arch
syntaxSetupHook la ovs prog cfgs =
  SetupHook $ \bak halloc llvmCtx fs -> do
    let typeCtx = llvmCtx ^. Trans.llvmTypeCtx
    let dl = TCtx.llvmDataLayout typeCtx
    let mvar = Trans.llvmMemVar llvmCtx

    -- Register built-in and user overrides.
    funOvs <-
      registerLLVMSexpOverrides la (builtinLLVMOverrides fs) ovs bak halloc llvmCtx fs prog

    -- In addition to binding function handles for the user overrides,
    -- we must also redirect function handles resulting from parsing
    -- forward declarations (`declare`) to actually call the overrides.
    GLO.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs (CSyn.parsedProgForwardDecs prog)

    -- If a startup override exists and it contains forward declarations,
    -- then we redirect the function handles to actually call the respective
    -- overrides.
    Monad.forM_ (Map.elems cfgs) $ \entrypointCfgs ->
      Monad.forM_ (GE.startupOvForwardDecs <$> GE.entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
        GLO.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs startupOvFwdDecs

    -- Register defined functions. If there is a user override of the same
    -- name, use the override's definition instead so that it takes
    -- precedence.
    Monad.forM_ (CSyn.parsedProgCFGs prog) $ \(C.Reg.AnyCFG defCfg) -> do
      let defHdl = C.Reg.cfgHandle defCfg
      let defName = C.handleName defHdl
      case Map.lookup defName funOvs of
        Nothing -> do
          C.SomeCFG defSsa <- pure $ C.toSSA defCfg
          -- This could probably be a helper defined in Crucible...
          let bindCfg c = C.bindFnHandle (C.cfgHandle c) (C.UseCFG c (C.postdomInfo c))
          bindCfg defSsa
        Just (CLLVM.SomeLLVMOverride llvmOverride) ->
          GLO.bindLLVMOverrideFnHandle mvar defHdl llvmOverride

-- | A 'SetupHook' for LLVM CFGs from IR modules.
moduleSetupHook ::
  GreaseLogAction ->
  -- | Files containing S-expression overrides
  [FilePath] ->
  Trans.ModuleTranslation arch ->
  Map GE.Entrypoint (GE.EntrypointCfgs (C.AnyCFG CLLVM.LLVM)) ->
  SetupHook sym arch
moduleSetupHook la ovs trans cfgs =
  SetupHook $ \bak halloc llvmCtx fs -> do
    let typeCtx = llvmCtx ^. Trans.llvmTypeCtx
    let dl = TCtx.llvmDataLayout typeCtx
    let mvar = Trans.llvmMemVar llvmCtx
    let llvmMod = trans ^. Trans.modTransModule

    -- Register defined functions...
    let handleTranslationWarning warn = doLog la (Diag.LLVMTranslationWarning warn)
    CLLVM.registerLazyModule handleTranslationWarning trans

    -- ...and then register overrides. We register overrides *after*
    -- registering defined functions so that overrides take precedence over
    -- defined functions.
    funOvs <-
      GLO.registerLLVMModuleOverrides la (builtinLLVMOverrides fs) ovs bak halloc llvmCtx fs llvmMod
    -- If a startup override exists and it contains forward declarations,
    -- then we redirect the function handles to actually call the respective
    -- overrides.
    Monad.forM_ (Map.elems cfgs) $ \entrypointCfgs ->
      Monad.forM_ (GE.startupOvForwardDecs <$> GE.entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
        GLO.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs startupOvFwdDecs
