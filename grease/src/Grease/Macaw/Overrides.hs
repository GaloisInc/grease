{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Registering Macaw overrides
module Grease.Macaw.Overrides (
  CantResolveOverrideCallback (..),
  MacawSExpOverride (..),
  MacawFnHandle,
  MacawOverride,
  macawOverride,
  mkMacawOverrideMap,
  mkMacawOverrideMapWithBuiltins,
  registerMacawSexpProgForwardDeclarations,
  registerMacawOvForwardDeclarations,
  lookupMacawForwardDeclarationOverride,
) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NE
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (fmapFC)
import Data.Sequence qualified as Seq
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw.Arch
import Grease.Macaw.Overrides.Builtin (builtinStubsOverrides)
import Grease.Macaw.Overrides.SExp (MacawSExpOverride (..), loadOverrides)
import Grease.Macaw.SimulatorState (HasGreaseSimulatorState, MacawFnHandle, MacawOverride)
import Grease.Overrides (CantResolveOverrideCallback (..))
import Grease.Skip (registerSkipOverride)
import Grease.Syntax (ParseProgramError)
import Grease.Syntax.Overrides as SExp
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO (LLVMFileSystem)
import Lang.Crucible.LLVM.TypeContext (TypeContext)
import Lang.Crucible.Simulator qualified as CS
import Stubs.FunctionOverride qualified as Stubs
import Stubs.FunctionOverride.ForwardDeclarations qualified as Stubs
import What4.Expr qualified as W4
import What4.FunctionName qualified as WFN
import What4.Protocol.Online qualified as W4

-- | Convert a 'Stubs.FunctionOverride' to a 'MacawOverride'. Really, this
-- functionality ought to be exposed from @stubs-common@. See
-- <https://github.com/GaloisInc/stubs/issues/16>.
macawOverride ::
  forall sym bak p arch args ret solver scope st fs.
  ( CB.IsSymInterface sym
  , CLM.HasLLVMAnn sym
  , -- For silly reasons, `stubs` requires the use of an online SMT solver
    -- connection in order to call `functionOverride`. See
    -- https://github.com/GaloisInc/stubs/issues/28.
    W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  C.GlobalVar CLM.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym args arch ret ->
  MacawOverride p sym arch
macawOverride bak mvar archCtx fnOv =
  CS.mkOverride' (Stubs.functionName fnOv) regsRepr ov
 where
  genArchVals :: Symbolic.GenArchVals Symbolic.LLVMMemory arch
  genArchVals = archCtx ^. archVals

  regsRepr :: C.TypeRepr (Symbolic.ArchRegStruct arch)
  regsRepr =
    C.StructRepr $
      Symbolic.crucArchRegTypes $
        Symbolic.archFunctions genArchVals

  ov ::
    forall r.
    CS.OverrideSim
      p
      sym
      (Symbolic.MacawExt arch)
      r
      (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
      (Symbolic.ArchRegStruct arch)
      (CS.RegValue sym (Symbolic.ArchRegStruct arch))
  ov = do
    mem <- CS.readGlobal mvar

    -- Construct the arguments
    argMap <- CS.getOverrideArgs
    let argReg = massageRegAssignment $ CS.regMap argMap
    (args, getVarArg) <-
      liftIO $
        (archCtx ^. archIntegerArguments)
          bak
          (Stubs.functionArgTypes fnOv)
          argReg
          mem

    -- Invoke the override
    retVal <-
      Stubs.functionOverride
        fnOv
        bak
        args
        getVarArg
        [] -- We don't currently make use of parent overrides

    -- Put the return value(s) into the appropriate register(s)
    (archCtx ^. archIntegerReturnRegisters)
      bak
      genArchVals
      (Stubs.functionReturnType fnOv)
      retVal
      argReg

-- | Massage the 'C.RegEntry' 'Ctx.Assignment' that 'C.getOverrideArgs'
-- provides into the form that 'archIntegerArguments' expects.
massageRegAssignment ::
  Ctx.Assignment (CS.RegEntry sym) (Ctx.EmptyCtx Ctx.::> C.StructType ctx) ->
  Ctx.Assignment (CS.RegValue' sym) ctx
massageRegAssignment = CS.unRV . Ctx.last . fmapFC (CS.RV . CS.regValue)

-- | Construct a 'Map.Map' of names of function overrides to their corresponding
-- 'MacawSExpOverride's, suitable for use within @macaw-symbolic@. The
-- overrides are taken from the following sources:
--
-- * Generic function overrides (e.g., from 'builtinStubsOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
mkMacawOverrideMap ::
  forall sym bak arch solver scope st fs p.
  ( CB.IsSymInterface sym
  , W4.OnlineSolver solver
  , ?memOpts :: CLM.MemOptions
  , ?lc :: TypeContext
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  -- | The built-in overrides.
  Seq.Seq (Stubs.SomeFunctionOverride p sym arch) ->
  -- | The paths of each user-supplied override file.
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar CLM.Mem ->
  ArchContext arch ->
  IO (Either ParseProgramError (Map.Map WFN.FunctionName (MacawSExpOverride p sym arch)))
mkMacawOverrideMap bak builtinOvs userOvPaths halloc mvar archCtx = do
  userOvsResult <- loadOverrides userOvPaths halloc
  case userOvsResult of
    Left err -> pure $ Left err
    Right userOvs ->
      Right <$> do
        -- Note the order here: due to how Map.fromList works, user-specified
        -- overrides will take precedence over builtin overrides, since
        -- Map.fromList favors things that appear later in the list in the event
        -- that there are duplicate names.
        let allOvs = builtinOvs <> userOvs
        Map.fromList . Foldable.toList
          <$> traverse
            ( \someFnOv@(Stubs.SomeFunctionOverride fnOv) -> do
                let macawPublicOv = macawOverride bak mvar archCtx fnOv
                macawPublicHdl <-
                  liftIO $
                    C.mkHandle'
                      halloc
                      (CS.overrideName macawPublicOv)
                      (Ctx.singleton regsRepr)
                      regsRepr
                let macawFnOv =
                      MacawSExpOverride
                        { msoPublicFnHandle = macawPublicHdl
                        , msoPublicOverride = macawPublicOv
                        , msoSomeFunctionOverride = someFnOv
                        }
                pure (Stubs.functionName fnOv, macawFnOv)
            )
            allOvs
 where
  regsRepr :: C.TypeRepr (Symbolic.ArchRegStruct arch)
  regsRepr =
    C.StructRepr $
      Symbolic.crucArchRegTypes $
        Symbolic.archFunctions $
          archCtx ^. archVals

-- | Like 'mkMacawOverrideMap', with 'builtinStubsOverrides'.
mkMacawOverrideMapWithBuiltins ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , ?memOpts :: CLM.MemOptions
  , ?lc :: TypeContext
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  -- | The paths of each user-supplied override file.
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar CLM.Mem ->
  ArchContext arch ->
  Symbolic.MemModelConfig p sym arch CLM.Mem ->
  LLVMFileSystem (MC.ArchAddrWidth arch) ->
  IO (Either ParseProgramError (Map.Map WFN.FunctionName (MacawSExpOverride p sym arch)))
mkMacawOverrideMapWithBuiltins bak userOvPaths halloc mvar archCtx memCfg fs = do
  let builtinOvs = builtinStubsOverrides bak mvar memCfg fs
  mkMacawOverrideMap bak builtinOvs userOvPaths halloc mvar archCtx

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. Treat any calls to unresolved
-- forward declarations as though the functions were skipped.
registerMacawSexpProgForwardDeclarations ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  , ToConc.HasToConcretize p
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  CLLVM.DataLayout ->
  C.GlobalVar CLM.Mem ->
  -- | What to do when a forward declaration cannot be resolved.
  CantResolveOverrideCallback sym (Symbolic.MacawExt arch) ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | The map of forward declaration names to their handles.
  Map.Map WFN.FunctionName C.SomeHandle ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawSexpProgForwardDeclarations bak la dl mvar errCb funOvs =
  registerMacawForwardDeclarations bak funOvs $
    CantResolveOverrideCallback $
      registerSkipOverride la dl mvar errCb

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. Attempting to call an
-- unresolved forward declaration will raise an error.
registerMacawOvForwardDeclarations ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , ToConc.HasToConcretize p
  ) =>
  bak ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  CantResolveOverrideCallback sym (Symbolic.MacawExt arch) ->
  -- | The map of forward declaration names to their handles.
  Map.Map WFN.FunctionName C.SomeHandle ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawOvForwardDeclarations bak funOvs errCb =
  registerMacawForwardDeclarations bak funOvs errCb

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. If a forward declaration
-- name cannot be resolved to an override, then perform the supplied action.
registerMacawForwardDeclarations ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , ToConc.HasToConcretize p
  ) =>
  bak ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  CantResolveOverrideCallback sym (Symbolic.MacawExt arch) ->
  -- | The map of forward declaration names to their handles.
  Map.Map WFN.FunctionName C.SomeHandle ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawForwardDeclarations bak funOvs errCb fwdDecs = do
  let CantResolveOverrideCallback cannotResolve = errCb
  Foldable.forM_ (Map.toList fwdDecs) $ \(decName, C.SomeHandle hdl) ->
    registerMacawForwardDeclaration bak funOvs cannotResolve decName hdl

-- | Redirect a handle for a forward declaration in an S-expression file to
-- actually call the corresponding Macaw override. If the forward declaration
-- name cannot be resolved to an override, then perform the supplied action.
registerMacawForwardDeclaration ::
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , ToConc.HasToConcretize p
  ) =>
  bak ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  ( forall args ret.
    WFN.FunctionName ->
    C.FnHandle args ret ->
    CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
  ) ->
  -- | Name of the forward declaration
  WFN.FunctionName ->
  -- | Handle to bind
  C.FnHandle args' ret' ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawForwardDeclaration bak funOvs cannotResolve decName hdl =
  case lookupMacawForwardDeclarationOverride bak funOvs decName hdl of
    Nothing -> cannotResolve decName hdl
    Just ov -> CS.bindFnHandle hdl (CS.UseOverride ov)

-- | Lookup an override for a function handle from a forward declaration.
lookupMacawForwardDeclarationOverride ::
  forall p sym bak arch scope st fs solver args ret.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , ToConc.HasToConcretize p
  ) =>
  bak ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | Name of the forward declaration
  WFN.FunctionName ->
  -- | Handle to bind
  C.FnHandle args ret ->
  Maybe (CS.Override p sym (Symbolic.MacawExt arch) args ret)
lookupMacawForwardDeclarationOverride bak funOvs decName hdl =
  case Map.lookup decName funOvs of
    Nothing -> do
      -- These overrides are *only* callable from S-expression files with
      -- forward-declarations of them.
      case decName of
        "fresh-bytes" -> do
          let ov = SExp.freshBytesOverride @_ @p @sym @(Symbolic.MacawExt arch) ?ptrWidth
          (C.Refl, C.Refl) <- SExp.checkTypedOverrideHandleCompat hdl ov
          Just (CS.runTypedOverride (C.handleName hdl) ov)
        "conc-bv-8" -> tryConcBvOverride bak (C.knownNat @8) hdl
        "conc-bv-16" -> tryConcBvOverride bak (C.knownNat @16) hdl
        "conc-bv-32" -> tryConcBvOverride bak (C.knownNat @32) hdl
        "conc-bv-64" -> tryConcBvOverride bak (C.knownNat @64) hdl
        _ -> Nothing
    Just mso -> do
      let someForwardedOv = msoSomeFunctionOverride mso
          forwardedOv =
            Stubs.mkForwardDeclarationOverride
              bak
              -- We don't use parent overrides, hence the []
              (someForwardedOv NE.:| [])
              decName
              hdl
      Just forwardedOv
