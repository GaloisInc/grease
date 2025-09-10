{-# LANGUAGE ImplicitParams #-}

-- | c.f. "Grease.LLVM.SetupHook"
module Grease.Macaw.SetupHook (
  SetupHook (..),
  registerOverrideCfgs,
  registerOverrideForwardDeclarations,
  registerOverrideHandles,
  registerSyntaxCfgs,
  registerSyntaxForwardDeclarations,
  registerSyntaxHandles,
  syntaxSetupHook,
  binSetupHook,
) where

import Control.Monad qualified as Monad
import Data.Macaw.CFG.Core (ArchAddrWidth)
import Data.Macaw.Symbolic qualified as DMS
import Data.Map.Strict qualified as Map
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Diagnostic (GreaseLogAction)
import Grease.Entrypoint qualified as GE
import Grease.Macaw.Overrides as GMO
import Grease.Macaw.Overrides.Address as GMOA
import Grease.Macaw.SimulatorState (HasGreaseSimulatorState)
import Lang.Crucible.Backend qualified as LCB
import Lang.Crucible.Backend.Online qualified as LCB
import Lang.Crucible.CFG.Core qualified as LCCC
import Lang.Crucible.CFG.Reg qualified as LCCR
import Lang.Crucible.CFG.SSAConversion (toSSA)
import Lang.Crucible.LLVM.DataLayout (DataLayout)
import Lang.Crucible.LLVM.MemModel qualified as LCLM
import Lang.Crucible.Simulator qualified as LCS
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Stubs.FunctionOverride qualified as Stubs
import What4.Expr.Builder qualified as WEB
import What4.FunctionName qualified as WF
import What4.Protocol.Online qualified as WPO

-- | Hook to run before executing a CFG.
--
-- Note that @sym@ is a type parameter so that users can define 'SetupHook's
-- that reference a fixed @sym@ type. Same with @arch@.
--
-- c.f. 'Grease.LLVM.SetupHook.SetupHook'
newtype SetupHook sym arch
  = SetupHook
      ( forall bak rtp a r solver scope st fs p.
        ( LCB.IsSymBackend sym bak
        , sym ~ WEB.ExprBuilder scope st fs
        , bak ~ LCB.OnlineBackend solver scope st fs
        , WPO.OnlineSolver solver
        , LCLM.HasLLVMAnn sym
        , HasGreaseSimulatorState p sym arch
        , HasToConcretize p
        ) =>
        bak ->
        LCS.GlobalVar LCLM.Mem ->
        -- Map of names of overridden functions to their implementations
        Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
        LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
      )

-- | Register overrides, both user-defined ones and ones that are hard-coded
-- into GREASE itself.
registerOverrideCfgs ::
  Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerOverrideCfgs funOvs =
  Monad.forM_ (Map.elems funOvs) $ \mso -> do
    let publicOvHdl = GMO.msoPublicFnHandle mso
        publicOv = GMO.msoPublicOverride mso
    Stubs.SomeFunctionOverride fnOv <- pure $ GMO.msoSomeFunctionOverride mso
    LCS.bindFnHandle publicOvHdl (LCS.UseOverride publicOv)
    let auxFns = Stubs.functionAuxiliaryFnBindings fnOv
    Monad.forM_ auxFns $ \(LCS.FnBinding auxHdl auxSt) -> LCS.bindFnHandle auxHdl auxSt

-- | Redirect function handles from forward declarations appearing in
-- 'MacawSExpOverride's to their implementations.
registerOverrideForwardDeclarations ::
  ( LCLM.HasPtrWidth (ArchAddrWidth arch)
  , LCB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCB.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerOverrideForwardDeclarations bak funOvs =
  Monad.forM_ (Map.elems funOvs) $ \mso ->
    case GMO.msoSomeFunctionOverride mso of
      Stubs.SomeFunctionOverride fnOv ->
        GMO.registerMacawOvForwardDeclarations bak funOvs (Stubs.functionForwardDeclarations fnOv)

-- | Register all handles from a 'MacawSExpOverride'.
--
-- Calls 'registerOverrides' and 'registerOverrideForwardDeclarations'.
registerOverrideHandles ::
  ( LCLM.HasPtrWidth (ArchAddrWidth arch)
  , LCB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCB.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerOverrideHandles bak funOvs = do
  registerOverrideCfgs funOvs
  registerOverrideForwardDeclarations bak funOvs

-- | Register defined functions from an S-expression program
-- ('CSyn.ParsedProgram').
registerSyntaxCfgs ::
  DMS.SymArchConstraints arch =>
  CSyn.ParsedProgram (DMS.MacawExt arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerSyntaxCfgs prog =
  Monad.forM_ (CSyn.parsedProgCFGs prog) $ \(LCCR.AnyCFG defCfg) -> do
    LCCC.SomeCFG defSsa <- pure $ toSSA defCfg
    LCS.bindCFG defSsa

-- | Redirect function handles from forward declarations appearing in an
-- S-expression program ('CSyn.ParsedProgram') to their implementations.
registerSyntaxForwardDeclarations ::
  ( LCLM.HasPtrWidth (ArchAddrWidth arch)
  , DMS.SymArchConstraints arch
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCB.OnlineBackend solver scope st fs
  , LCLM.HasLLVMAnn sym
  , LCB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , HasToConcretize p
  , ?memOpts :: LCLM.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  DataLayout ->
  LCS.GlobalVar LCLM.Mem ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
  CSyn.ParsedProgram (DMS.MacawExt arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerSyntaxForwardDeclarations bak la dl mvar funOvs prog =
  GMO.registerMacawSexpProgForwardDeclarations bak la dl mvar funOvs (CSyn.parsedProgForwardDecs prog)

-- | Register all handles from a an S-expression program ('CSyn.ParsedProgram').
--
-- Calls 'registerSyntaxCfgs' and 'registerSyntaxForwardDeclarations'.
registerSyntaxHandles ::
  ( LCLM.HasPtrWidth (ArchAddrWidth arch)
  , DMS.SymArchConstraints arch
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCB.OnlineBackend solver scope st fs
  , LCLM.HasLLVMAnn sym
  , LCB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , HasToConcretize p
  , ?memOpts :: LCLM.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  DataLayout ->
  LCS.GlobalVar LCLM.Mem ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WF.FunctionName (MacawSExpOverride p sym arch) ->
  CSyn.ParsedProgram (DMS.MacawExt arch) ->
  LCS.OverrideSim p sym (DMS.MacawExt arch) rtp a r ()
registerSyntaxHandles bak la dl mvar funOvs prog = do
  registerSyntaxCfgs prog
  registerSyntaxForwardDeclarations bak la dl mvar funOvs prog

-- | A 'SetupHook' for Macaw CFGs from S-expression programs.
syntaxSetupHook ::
  ( LCLM.HasPtrWidth (ArchAddrWidth arch)
  , DMS.SymArchConstraints arch
  , ?memOpts :: LCLM.MemOptions
  ) =>
  GreaseLogAction ->
  DataLayout ->
  Map.Map GE.Entrypoint (GE.EntrypointCfgs (LCCR.AnyCFG (DMS.MacawExt arch))) ->
  CSyn.ParsedProgram (DMS.MacawExt arch) ->
  SetupHook sym arch
syntaxSetupHook la dl cfgs prog =
  SetupHook $ \bak mvar funOvs -> do
    registerOverrideHandles bak funOvs
    registerSyntaxHandles bak la dl mvar funOvs prog

    -- Redirect function handles resulting from parsing forward declarations
    -- (`declare`s) to their implementations.
    Monad.forM_ (Map.elems cfgs) $ \entrypointCfgs ->
      Monad.forM_ (GE.startupOvForwardDecs <$> GE.entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
        GMO.registerMacawOvForwardDeclarations bak funOvs startupOvFwdDecs

-- | A 'SetupHook' for Macaw CFGs from binaries.
--
-- This setup hook does much less than 'syntaxSetupHook'. We don't need
-- to register most functions here because that happens incrementally in
-- 'Grease.Macaw.ResolveCall.lookupFunctionHandle'. S-expression programs, on
-- the other hand, looks up functions in a different way, so 'syntaxSetupHook'
-- must eagerly register all functions it might call ahead of time.
--
-- The exceptions to this rule are (1) startup overrides and (2) address
-- overrides.
--
-- If a startup override exists and it contains forward declarations, then we
-- redirect the function handles to actually call the respective overrides.
-- (Alternatively, we could plumb the startup overrides' forward declarations
-- into `lookupFunctionHandle` and register them incrementally, but that is more
-- work. Given that startup overrides can't invoke anything defined in the main
-- program itself, it's much less work to register them ahead of time here.)
--
-- We do the same thing for forward declarations in address overrides.
binSetupHook ::
  ( DMS.SymArchConstraints arch
  , LCLM.HasPtrWidth (ArchAddrWidth arch)
  ) =>
  GMOA.AddressOverrides arch ->
  Map.Map GE.Entrypoint (GE.MacawEntrypointCfgs arch) ->
  SetupHook sym arch
binSetupHook addrOvs cfgs =
  SetupHook $ \bak _mvar funOvs -> do
    GMOA.registerAddressOverrideHandles bak funOvs addrOvs
    Monad.forM_ (Map.elems cfgs) $ \(GE.MacawEntrypointCfgs entrypointCfgs _) ->
      Monad.forM_ (GE.startupOvForwardDecs <$> GE.entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
        GMO.registerMacawOvForwardDeclarations bak funOvs startupOvFwdDecs
