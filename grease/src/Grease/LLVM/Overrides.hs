{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides (
  bindLLVMOverrideFnHandle,
  registerLLVMOverrides,
  registerLLVMSexpOverrides,
  registerLLVMModuleOverrides,
  registerLLVMSexpProgForwardDeclarations,
  registerLLVMOvForwardDeclarations,
) where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (Diagnostic (LLVMOverridesDiagnostic), GreaseLogAction)
import Grease.LLVM.Overrides.Builtin (basicLLVMOverrides)
import Grease.LLVM.Overrides.Declare (mkDeclare)
import Grease.LLVM.Overrides.Diagnostic as Diag
import Grease.LLVM.Overrides.SExp (LLVMSExpOverride (..))
import Grease.LLVM.Overrides.SExp qualified as GLOS
import Grease.Skip (declSkipOverride, registerSkipOverride)
import Grease.Syntax.Overrides (freshBytesOverride, tryBindTypedOverride)
import Grease.Utility (declaredFunNotFound, llvmOverrideName)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout as CLLVM
import Lang.Crucible.LLVM.Functions qualified as CLLVM
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.Syntax.Overrides.String qualified as StrOv
import Lang.Crucible.LLVM.Translation (LLVMContext)
import Lang.Crucible.LLVM.Translation qualified as CLLVM
import Lang.Crucible.LLVM.TypeContext (TypeContext (..))
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lumberjack qualified as LJ
import Text.LLVM.AST qualified as L
import What4.FunctionName qualified as W4
import What4.Expr qualified as W4
import Lang.Crucible.Backend.Online qualified as C
import What4.Protocol.Online qualified as W4

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LLVMOverridesDiagnostic diag)

-- | Bind a 'C.FnHandle' to an 'CLLVM.LLVMOverride'. Note that the argument and
-- result types of the 'C.FnHandle' and the 'CLLVM.LLVMOverride' do not need to
-- be exactly the same, as 'CLLVM.build_llvm_override' handles any necessary
-- type conversions (e.g., converting from bitvectors to pointers).
bindLLVMOverrideFnHandle ::
  Mem.HasLLVMAnn sym =>
  C.GlobalVar Mem.Mem ->
  C.FnHandle fnArgs fnRet ->
  CLLVM.LLVMOverride p sym CLLVM.LLVM ovrArgs ovrRet ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
bindLLVMOverrideFnHandle mvar hdl llvmOverride = do
  let name = C.handleName hdl
  let overrideArgs = CLLVM.llvmOverride_args llvmOverride
  let overrideRet = CLLVM.llvmOverride_ret llvmOverride
  o <-
    CLLVM.build_llvm_override
      name
      overrideArgs
      overrideRet
      (C.handleArgTypes hdl)
      (C.handleReturnType hdl)
      (\asgn -> CLLVM.llvmOverride_def llvmOverride mvar asgn)
  C.bindFnHandle hdl (C.UseOverride o)

-- Private helper, not exported
mapRights :: (a -> Either l r) -> [a] -> [r]
mapRights f =
  \case
    [] -> []
    (x : xs) ->
      case f x of
        Left _ -> mapRights f xs
        Right x' -> x' : mapRights f xs

-- | Construct LLVM declarations corresponding to the forward declarations in
-- an S-expression file. Such declarations are used by the Crucible-LLVM override
-- matching machinery ('CLLVM.register_llvm_overrides_').
forwardDeclDecls ::
  Mem.HasPtrWidth 64 =>
  Map.Map W4.FunctionName C.SomeHandle ->
  [L.Declare]
forwardDeclDecls m =
  mapRights
    (\(fnm, C.SomeHandle hdl) -> mkDeclare (Text.unpack (W4.functionName fnm)) (C.handleArgTypes hdl) (C.handleReturnType hdl))
    (Map.toList m)

-- | Register function overrides and return a 'Map.Map' of override names to
-- their corresponding 'CLLVM.SomeLLVMOverride's, suitable for use within
-- @crucible-llvm@. The overrides are taken from the following sources:
--
-- * Overrides that simply skip function calls (from 'declSkipOverride')
--
-- * Generic function overrides (e.g., from 'builtinLLVMOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
registerLLVMOverrides ::
  forall sym bak arch p rtp as r solver scope st fs.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ToConc.HasToConcretize p
  , ?lc :: TypeContext
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  Seq.Seq (W4.FunctionName, LLVMSExpOverride) ->
  bak ->
  LLVMContext arch ->
  SymIO.LLVMFileSystem 64 ->
  -- | @declare@s in the target program
  [L.Declare] ->
  -- | Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
registerLLVMOverrides la builtinOvs userOvs bak llvmCtx fs decls = do
  let mvar = CLLVM.llvmMemVar llvmCtx

  -- For convenience, we treat all programs and overrides as if they `declare`d
  -- all of the libc and "basic" LLVM functions.
  let basicDecls =
        List.map
          (\(CLLVM.SomeLLVMOverride ov) -> CLLVM.llvmOverride_declare ov)
          (Foldable.toList (basicLLVMOverrides fs))
  let fwdDeclLLVMDecls =
        List.concatMap (\(_, lso) -> forwardDeclDecls (lsoForwardDeclarations lso)) userOvs
  let allDecls = decls List.++ basicDecls List.++ fwdDeclLLVMDecls
  Foldable.forM_ allDecls $ \decl -> do
    doLog la (Diag.FoundDeclare decl)
    let L.Symbol name = L.decName decl
    let aliases = []
    -- See the module comment on "Lang.Crucible.LLVM.Functions" for why this
    -- part is necessary.
    C.modifyGlobal mvar $ \mem ->
      liftIO (CLLVM.registerFunPtr bak mem name (L.decName decl) aliases)

  -- Note the order here: we register "skip" overrides *first*, so that built-in
  -- overrides and user overrides will take precedence over them.
  Foldable.forM_ decls $ \decl -> do
    case declSkipOverride la llvmCtx decl of
      Nothing -> pure ()
      Just ov -> registerOv ov

  -- Note the order again: user overrides (registered later) will take
  -- precedence over built-in overrides (registered here).
  let ovs = Foldable.toList builtinOvs
  builtinOvs' <- CLLVM.register_llvm_overrides_ llvmCtx ovs allDecls
  builtinOvs'' <-
    forM builtinOvs' $ \sov@(CLLVM.SomeLLVMOverride ov) -> do
      let nm = llvmOverrideName ov
      doLog la (Diag.RegisteredOverride sov)
      pure (nm, sov)

  userOvs' <-
    forM userOvs $ \(nm, lso) -> do
      let GLOS.AnyLLVMOverride publicOv = lsoPublicOverride lso
          auxOvs = lsoAuxiliaryOverrides lso
      registerOv publicOv
      Foldable.traverse_ (\(GLOS.AnyLLVMOverride ov) -> registerOv ov) auxOvs
      pure (nm, publicOv)
  -- Similarly, we put the user overrides after the built-in overrides here
  -- (Map.fromList will favor later entries over earlier ones).
  let allOvs = Map.fromList $ Foldable.toList $ Seq.fromList builtinOvs'' <> userOvs'
  -- Next, register the handles for forward declarations in user-defined
  -- overrides. We only do this after registering all of the public functions
  -- so as to ensure that we get the dependencies correct.
  Foldable.for_ userOvs $ \(_, lso) ->
    registerLLVMOvForwardDeclarations bak mvar allOvs $
      lsoForwardDeclarations lso
  pure allOvs
 where
  registerOv ::
    CLLVM.SomeLLVMOverride p sym CLLVM.LLVM ->
    C.OverrideSim p sym CLLVM.LLVM rtp as r ()
  registerOv (CLLVM.SomeLLVMOverride ov) =
    CLLVM.alloc_and_register_override bak llvmCtx ov []

-- | For an S-expression program, register function overrides and return a
-- 'Map.Map' of override names to their corresponding 'CLLVM.SomeLLVMOverride's,
-- suitable for use within @crucible-llvm@. The overrides are taken from the
-- following sources:
--
-- * Generic function overrides (e.g., from 'builtinLLVMOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
registerLLVMSexpOverrides ::
  forall sym bak arch p rtp as r solver scope st fs.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ToConc.HasToConcretize p
  , ?lc :: TypeContext
  , ?memOpts :: Mem.MemOptions
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  Seq.Seq (W4.FunctionName, LLVMSExpOverride) ->
  bak ->
  LLVMContext arch ->
  SymIO.LLVMFileSystem 64 ->
  CSyn.ParsedProgram CLLVM.LLVM ->
  -- | Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
registerLLVMSexpOverrides la builtinOvs sexpOvs bak llvmCtx fs prog = do
  let decls = forwardDeclDecls (CSyn.parsedProgForwardDecs prog)
  registerLLVMOverrides la builtinOvs sexpOvs bak llvmCtx fs decls

-- | For an LLVM module, register function overrides and return a 'Map.Map' of
-- override names to their corresponding 'CLLVM.SomeLLVMOverride's, suitable
-- for use within @crucible-llvm@. The overrides are taken from the following
-- sources:
--
-- * Generic function overrides (from 'builtinLLVMOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
registerLLVMModuleOverrides ::
  forall sym bak arch p rtp as r solver scope st fs.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ToConc.HasToConcretize p
  , ?lc :: TypeContext
  , ?memOpts :: Mem.MemOptions
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  Seq.Seq (W4.FunctionName, LLVMSExpOverride) ->
  bak ->
  LLVMContext arch ->
  SymIO.LLVMFileSystem 64 ->
  L.Module ->
  -- | Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
registerLLVMModuleOverrides la builtinOvs sexpOvs bak llvmCtx fs llMod = do
  let decls = L.modDeclares llMod
  registerLLVMOverrides la builtinOvs sexpOvs bak llvmCtx fs decls

-- | Redirect handles for forward declarations in an LLVM S-expression program
-- to call the corresponding LLVM overrides. Treat any calls to unresolved
-- forward declarations as though the functions were skipped.
registerLLVMSexpProgForwardDeclarations ::
  ( C.IsSymInterface sym
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  bak ->
  GreaseLogAction ->
  CLLVM.DataLayout ->
  C.GlobalVar Mem.Mem ->
  -- | The map of public function names to their overrides.
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM) ->
  -- | The map of forward declaration names to their handles.
  Map.Map W4.FunctionName C.SomeHandle ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMSexpProgForwardDeclarations bak la dl mvar funOvs =
  registerLLVMForwardDeclarations bak mvar funOvs $
    registerSkipOverride la dl mvar

-- | Redirect handles for forward declarations in an LLVM S-expression override
-- to call the corresponding LLVM overrides. Attempting to call an unresolved
-- forward declaration will raise an error.
registerLLVMOvForwardDeclarations ::
  ( C.IsSymInterface sym
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  -- | The map of public function names to their overrides.
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM) ->
  -- | The map of forward declaration names to their handles.
  Map.Map W4.FunctionName C.SomeHandle ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMOvForwardDeclarations bak mvar funOvs =
  registerLLVMForwardDeclarations  bak mvar funOvs $ \fwdDecName _ ->
    declaredFunNotFound fwdDecName

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding LLVM overrides. If a forward declaration
-- name cannot be resolved to an override, then perform the supplied action.
registerLLVMForwardDeclarations ::
  ( C.IsSymInterface sym
  , Mem.HasPtrWidth w
  , Mem.HasLLVMAnn sym
  , ToConc.HasToConcretize p
  , ?memOpts :: Mem.MemOptions
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  -- | The map of public function names to their overrides.
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM) ->
  -- | What to do when a forward declaration cannot be resolved.
  ( forall args ret.
    W4.FunctionName ->
    C.FnHandle args ret ->
    C.OverrideSim p sym CLLVM.LLVM rtp as r ()
  ) ->
  -- | The map of forward declaration names to their handles.
  Map.Map W4.FunctionName C.SomeHandle ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMForwardDeclarations bak mvar funOvs cannotResolve fwdDecs =
  Foldable.for_ (Map.toList fwdDecs) $ \(fwdDecName, C.SomeHandle hdl) ->
    case Map.lookup fwdDecName funOvs of
      Just (CLLVM.SomeLLVMOverride llvmOverride) ->
        bindLLVMOverrideFnHandle mvar hdl llvmOverride
      Nothing ->
        -- These string-manipulating overrides are *only* callable from
        -- S-expression files with forward-declarations of them.
        case fwdDecName of
          "fresh-bytes" -> do
            ok <- tryBindTypedOverride hdl (freshBytesOverride bak ?ptrWidth)
            unless ok (cannotResolve fwdDecName hdl)
          "read-bytes" -> do
            ok <- tryBindTypedOverride hdl (StrOv.readBytesOverride mvar)
            unless ok (cannotResolve fwdDecName hdl)
          "read-c-string" -> do
            ok <- tryBindTypedOverride hdl (StrOv.readCStringOverride mvar)
            unless ok (cannotResolve fwdDecName hdl)
          "write-bytes" -> do
            ok <- tryBindTypedOverride hdl (StrOv.writeBytesOverride mvar)
            unless ok (cannotResolve fwdDecName hdl)
          "write-c-string" -> do
            ok <- tryBindTypedOverride hdl (StrOv.writeCStringOverride mvar)
            unless ok (cannotResolve fwdDecName hdl)
          _ -> cannotResolve fwdDecName hdl
