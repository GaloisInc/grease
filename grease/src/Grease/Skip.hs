{-# LANGUAGE ImplicitParams #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Skip (
  createSkipOverride,
  declSkipOverride,
  registerSkipOverride,
) where

import Control.Lens (to, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as Text
import Grease.Cursor qualified as Cursor
import Grease.Cursor.Pointer qualified as PtrCursor
import Grease.Diagnostic (Diagnostic (SkipDiagnostic), GreaseLogAction)
import Grease.Overrides qualified as GO
import Grease.Setup qualified as Setup
import Grease.Shape (ExtShape, Shape, minimalShapeWithPtrs)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Grease.Shape.Selector
import Grease.Skip.Diagnostic qualified as Diag
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout as Mem
import Lang.Crucible.LLVM.Functions as CLLVM
import Lang.Crucible.LLVM.Intrinsics as CLLVM
import Lang.Crucible.LLVM.Intrinsics.Declare as Decl
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.Translation (LLVMContext)
import Lang.Crucible.LLVM.Translation qualified as CLLVM
import Lang.Crucible.LLVM.TypeContext qualified as CLLVM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Types qualified as C
import Lumberjack qualified as LJ
import Text.LLVM.AST qualified as L
import What4.FunctionName qualified as WFN

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (SkipDiagnostic diag)

skipOverride ::
  ( C.IsSyntaxExtension ext
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  Mem.DataLayout ->
  CS.GlobalVar CLM.Mem ->
  WFN.FunctionName ->
  C.TypeRepr ret ->
  Shape ext tag ret ->
  CS.OverrideSim p sym ext r args' ret' (CS.RegValue sym ret)
skipOverride la dl memVar funcName valTy shape = do
  doLog la (Diag.FunctionCall funcName)
  CS.ovrWithBackend $ \bak -> do
    mem <- CS.readGlobal memVar
    -- TODO(#15): Preserve LLVM memory and annotations from setup monad state
    liftIO $ Setup.runSetup (Setup.InitialMem mem) $ do
      let funcNameStr = Text.unpack (WFN.functionName funcName)
      let valName = Setup.ValueName (funcNameStr ++ "Return")
      let sel = SelectRet (RetSelector funcName (Cursor.Here valTy))
      shape' <- Setup.setupShape la bak dl valName valTy sel shape
      pure (CS.unRV (Shape.getTag PtrShape.getPtrTag shape'))

-- | Try to create an override for a declared (but not defined) function.
--
-- This function inspects the return type of the declaration. The override
-- produces a \"minimal\" symbolic value of the corresponding type, e.g.,
--
-- * for booleans, it creates a fresh, symbolic boolean
-- * for structs, it recurses on the fields
-- * for pointers, it uses 'Grease.Shape.Pointer.minimalPtrShape'
--
-- Note that the override does *not* clobber any argument or global pointers,
-- making it unsound to use for verification.
createSkipOverride ::
  ( C.IsSyntaxExtension ext
  , CB.IsSymInterface sym
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  Mem.DataLayout ->
  CS.GlobalVar CLM.Mem ->
  WFN.FunctionName ->
  C.TypeRepr ret ->
  Either Shape.MinimalShapeError (CS.Override p sym ext args ret)
createSkipOverride la dl memVar funcName retTy = do
  shape <- minimalShapeWithPtrs (const NoTag) retTy
  let override =
        CS.mkOverride' funcName retTy $
          skipOverride la dl memVar funcName retTy shape
  Right override

-- | Try to create a skip override from a 'Decl.Declare' and an 'LLVMContext'.
--
-- See 'createSkipOverride' for details.
declSkipOverride ::
  ( C.IsSyntaxExtension ext
  , CB.IsSymInterface sym
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  LLVMContext arch ->
  Decl.SomeDeclare ->
  Maybe (CLLVM.SomeLLVMOverride p sym ext)
declSkipOverride la llvmCtx someDecl = do
  Decl.SomeDeclare
    decl@(Decl.Declare{Decl.decArgs = argTys, Decl.decRet = retTy}) <-
    pure someDecl
  shape <-
    case minimalShapeWithPtrs (const NoTag) retTy of
      Left _err -> Nothing
      Right shape -> Just shape
  let dl = llvmCtx ^. CLLVM.llvmTypeCtx . to CLLVM.llvmDataLayout
  let symb@(L.Symbol name) = Decl.decName decl
  let fnName = WFN.functionNameFromText (Text.pack name)
  Just $
    CLLVM.SomeLLVMOverride $
      CLLVM.LLVMOverride
        { llvmOverride_name = symb
        , llvmOverride_args = argTys
        , llvmOverride_ret = retTy
        , llvmOverride_def = \mvar _args ->
            skipOverride la dl mvar fnName retTy shape
        }

-- | Try to create and register an override for a declared function.
--
-- See 'createSkipOverride' for details.
registerSkipOverride ::
  ( C.IsSyntaxExtension ext
  , CB.IsSymInterface sym
  , CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ExtShape ext ~ PtrShape ext w
  , Cursor.CursorExt ext ~ PtrCursor.Dereference ext w
  ) =>
  GreaseLogAction ->
  Mem.DataLayout ->
  CS.GlobalVar CLM.Mem ->
  GO.CantResolveOverrideCallback sym ext ->
  WFN.FunctionName ->
  C.FnHandle args ret ->
  CS.OverrideSim p sym ext r args' ret' ()
registerSkipOverride la dl memVar errCb funcName hdl =
  case createSkipOverride la dl memVar funcName (C.handleReturnType hdl) of
    Left{} -> GO.runCantResolveOverrideCallback errCb funcName hdl
    Right ov ->
      let symbol = L.Symbol (Text.unpack (WFN.functionName funcName))
       in CLLVM.bindLLVMHandle memVar symbol hdl (CS.UseOverride ov)
