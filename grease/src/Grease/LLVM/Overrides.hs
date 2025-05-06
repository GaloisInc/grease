{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.LLVM.Overrides
  ( bindLLVMOverrideFnHandle
  , registerLLVMOverrides
  , registerLLVMSexpOverrides
  , registerLLVMModuleOverrides
  , registerLLVMSexpProgForwardDeclarations
  , registerLLVMOvForwardDeclarations
  , LLVMFunctionOverride(..)
  ) where

import Prelude (fromIntegral)

import Control.Applicative (pure)
import Control.Exception.Safe (throw)
import Control.Monad (forM, sequence)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool(..))
import Data.Either (Either(..))
import Data.Eq ((==))
import qualified Data.Foldable as Foldable
import Data.Function (($))
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (traverse)
import Data.String (String)
import System.IO (FilePath, IO)
import System.FilePath (dropExtensions, takeBaseName)

import qualified Lumberjack as LJ

import qualified Text.LLVM.AST as L

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.NatRepr as NatRepr
import qualified Data.Parameterized.Some as Some
import           Data.Parameterized.TraversableFC (toListFC)

-- what4
import qualified What4.FunctionName as W4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Reg as C.Reg
import qualified Lang.Crucible.CFG.SSAConversion as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import Lang.Crucible.LLVM.DataLayout as CLLVM
import qualified Lang.Crucible.LLVM.Functions as CLLVM
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as Mem
import Lang.Crucible.LLVM.Translation (LLVMContext)
import qualified Lang.Crucible.LLVM.Translation as CLLVM
import Lang.Crucible.LLVM.TypeContext (TypeContext(..))

-- crucible-syntax
import qualified Lang.Crucible.Syntax.Concrete as CSyn
import qualified Lang.Crucible.Syntax.Prog as CSyn

-- crucible-llvm-syntax
import Lang.Crucible.LLVM.Syntax (llvmParserHooks, emptyParserHooks)

import Grease.Diagnostic (GreaseLogAction, Diagnostic(LLVMOverridesDiagnostic))
import Grease.FunctionOverride (basicLLVMOverrides)
import Grease.LLVM.Overrides.Diagnostic as Diag
import Grease.Skip (declSkipOverride, registerSkipOverride)
import Grease.Syntax (parseProgram)
import Grease.Utility (GreaseException(..), declaredFunNotFound, tshow, llvmOverrideName)

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (LLVMOverridesDiagnostic diag)

-- | Lift a Crucible type to an LLVM type.
--
-- This function has several missing cases that can be filled in as necessary.
llvmType :: Mem.HasPtrWidth w => C.TypeRepr t -> Maybe L.Type
llvmType =
  \case
    C.AnyRepr {} -> Nothing
    C.BoolRepr -> Just (L.PrimType (L.Integer 1))
    C.CharRepr {} -> Nothing
    C.BVRepr w -> Just (intType w)
    C.ComplexRealRepr {} -> Nothing
    C.FloatRepr {} -> Nothing  -- TODO?
    C.FunctionHandleRepr {} -> Nothing
    C.IEEEFloatRepr {} -> Nothing  -- TODO?
    C.IntegerRepr {} -> Nothing
    C.MaybeRepr {} -> Nothing
    C.NatRepr {} -> Nothing
    C.RealValRepr {} -> Nothing
    C.RecursiveRepr {} -> Nothing
    C.ReferenceRepr {} -> Nothing
    C.SequenceRepr {} -> Nothing
    C.StringRepr {} -> Nothing
    C.StringMapRepr {} -> Nothing
    C.StructRepr fieldAssn -> do
      fieldTys <-
        traverse (Some.viewSome llvmType) $
        toListFC Some.Some fieldAssn
      Just $ L.Struct fieldTys
    C.SymbolicArrayRepr {} -> Nothing
    C.SymbolicStructRepr {} -> Nothing
    C.UnitRepr -> Just (L.PrimType L.Void)
    C.VariantRepr {} -> Nothing
    C.VectorRepr {} -> Nothing
    C.WordMapRepr {} -> Nothing

    Mem.LLVMPointerRepr w ->
      case C.testEquality w ?ptrWidth of
        Just C.Refl -> Just L.PtrOpaque
        Nothing -> Just (intType w)
    C.IntrinsicRepr {} -> Nothing
  where
    -- TODO(lb): Avoid 'fromIntegral', handle overflow gracefully
    intType :: NatRepr.NatRepr n -> L.Type
    intType w = L.PrimType (L.Integer (fromIntegral (NatRepr.natValue w)))

-- | Create an LLVM declaration from Crucible types.
--
-- See https://github.com/GaloisInc/crucible/issues/1138 for progress on
-- obviating this code.
mkDeclare ::
  Mem.HasPtrWidth w =>
  String ->
  Ctx.Assignment C.TypeRepr args ->
  C.TypeRepr ret ->
  Either Text L.Declare
mkDeclare name args ret = do
  let getType :: forall t. C.TypeRepr t -> Either Text L.Type
      getType t =
        case llvmType t of
          Nothing -> Left ("Can't make LLVM type from Crucible type " <> tshow t)
          Just llTy -> Right llTy
  llvmArgs <- sequence (toListFC getType args)
  llvmRet <- getType ret
  pure $
    L.Declare
    { L.decArgs = llvmArgs
    , L.decAttrs = []
    , L.decComdat = Nothing
    , L.decLinkage = Nothing
    , L.decName = L.Symbol name
    , L.decRetType = llvmRet
    , L.decVarArgs = False
    , L.decVisibility = Nothing
    }

-- | Parse overrides in the Crucible-LLVM S-expression syntax.
loadOverrides ::
  Mem.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Seq.Seq (W4.FunctionName, LLVMFunctionOverride p sym))
loadOverrides paths halloc mvar =
  traverse (\path -> loadOverride path halloc mvar) (Seq.fromList paths)

-- | Parse an override in the Crucible-LLVM S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  Mem.HasPtrWidth w =>
  FilePath ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (W4.FunctionName, LLVMFunctionOverride p sym)
loadOverride path halloc mvar = do
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  let fnNameText = Text.pack $ dropExtensions $ takeBaseName path
  let fnName = W4.functionNameFromText fnNameText
  let (publicCfgs, auxCfgs) =
        List.partition (isPublicCblFun fnName) (CSyn.parsedProgCFGs prog)
  case publicCfgs of
    [publicCfg] -> do
      publicOv <- acfgToSomeLLVMOverride path publicCfg
      auxOvs <- traverse (acfgToSomeLLVMOverride path) auxCfgs
      let fwdDecs = CSyn.parsedProgForwardDecs prog
      pure
        ( fnName
        , LLVMFunctionOverride
            { lfoPublicOverride = publicOv
            , lfoAuxiliaryOverrides = auxOvs
            , lfoForwardDeclarations = fwdDecs
            }
        )
    [] ->
      throw $ GreaseException $
        "Expected to find a function named `" <> fnNameText <> "` in `" <>
        Text.pack path <> "`"
    _:_ ->
      throw $ GreaseException $
        "Override `" <> Text.pack path <> "` contains multiple `" <>
        fnNameText <> "` functions"

-- | Convert an 'C.Reg.AnyCFG' for a function defined in a Crucible-LLVM
-- S-expression program to a 'CLLVM.SomeLLVMOverride' value.
acfgToSomeLLVMOverride ::
  Mem.HasPtrWidth w =>
  FilePath {- The file which defines the CFG's function.
              This is only used for error messages. -} ->
  C.Reg.AnyCFG CLLVM.LLVM ->
  IO (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)
acfgToSomeLLVMOverride path (C.Reg.AnyCFG cfg) = do
  let argTys = C.Reg.cfgArgTypes cfg
  let retTy = C.Reg.cfgReturnType cfg
  C.SomeCFG ssa <- pure $ C.toSSA cfg
  let fnName = C.handleName (C.cfgHandle ssa)
  let name = Text.unpack (W4.functionName fnName)
  case mkDeclare name argTys retTy of
    Right decl ->
      pure $
      CLLVM.SomeLLVMOverride $
      CLLVM.LLVMOverride
        { CLLVM.llvmOverride_declare = decl
        , CLLVM.llvmOverride_args = argTys
        , CLLVM.llvmOverride_ret = retTy
        , CLLVM.llvmOverride_def =
            \_mvar args ->
              C.regValue <$> C.callCFG ssa (C.RegMap args)
        }
    Left err ->
      throw (GreaseException ("Bad override in file " <> Text.pack path <> ": " <> err))

-- | Does a function have the same name as the @.cbl@ file in which it is
-- defined? That is, is a function publicly visible from the @.cbl@ file?
isPublicCblFun :: W4.FunctionName -> C.Reg.AnyCFG ext -> Bool
isPublicCblFun fnName (C.Reg.AnyCFG cfg) =
  C.handleName (C.Reg.cfgHandle cfg) == fnName

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
  let overrideRet  = CLLVM.llvmOverride_ret llvmOverride
  o <- CLLVM.build_llvm_override name overrideArgs overrideRet (C.handleArgTypes hdl) (C.handleReturnType hdl)
         (\asgn -> CLLVM.llvmOverride_def llvmOverride mvar asgn)
  C.bindFnHandle hdl (C.UseOverride o)

-- Private helper, not exported
mapRights :: (a -> Either l r) -> [a] -> [r]
mapRights f =
  \case
    [] -> []
    (x:xs) ->
      case f x of
        Left _ -> mapRights f xs
        Right x' -> x':mapRights f xs

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
  forall sym bak arch p rtp as r.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ?lc :: TypeContext
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  [FilePath] ->
  bak ->
  C.HandleAllocator ->
  LLVMContext arch ->
  -- | @declare@s in the target program
  [L.Declare] ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
  -- ^ Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
registerLLVMOverrides la builtinOvs paths bak halloc llvmCtx decls = do
  let mvar = CLLVM.llvmMemVar llvmCtx
  userOvs <- liftIO (loadOverrides paths halloc mvar)

  -- For convenience, we treat all programs and overrides as if they `declare`d
  -- all of the libc and "basic" LLVM functions.
  let basicDecls =
        List.map
          (\(CLLVM.SomeLLVMOverride ov) -> CLLVM.llvmOverride_declare ov)
          (Foldable.toList basicLLVMOverrides)
  let fwdDeclLLVMDecls =
        List.concatMap (\(_, lfo) -> forwardDeclDecls (lfoForwardDeclarations lfo)) userOvs
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
    forM userOvs $ \(nm, lfo) -> do
     let publicOv = lfoPublicOverride lfo
         auxOvs = lfoAuxiliaryOverrides lfo
     registerOv publicOv
     Foldable.traverse_ registerOv auxOvs
     pure (nm, publicOv)
  -- Similarly, we put the user overrides after the built-in overrides here
  -- (Map.fromList will favor later entries over earlier ones).
  let allOvs = Map.fromList $ Foldable.toList $ Seq.fromList builtinOvs'' <> userOvs'
  -- Next, register the handles for forward declarations in user-defined
  -- overrides. We only do this after registering all of the public functions
  -- so as to ensure that we get the dependencies correct.
  Foldable.for_ userOvs $ \(_, lfo) ->
    registerLLVMOvForwardDeclarations mvar allOvs $
    lfoForwardDeclarations lfo
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
  forall sym bak arch p rtp as r.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ?lc :: TypeContext
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  [FilePath] ->
  bak ->
  C.HandleAllocator ->
  LLVMContext arch ->
  CSyn.ParsedProgram CLLVM.LLVM ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
  -- ^ Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
registerLLVMSexpOverrides la builtinOvs paths bak halloc llvmCtx prog = do
  let decls = forwardDeclDecls (CSyn.parsedProgForwardDecs prog)
  registerLLVMOverrides la builtinOvs paths bak halloc llvmCtx decls

-- | For an LLVM module, register function overrides and return a 'Map.Map' of
-- override names to their corresponding 'CLLVM.SomeLLVMOverride's, suitable
-- for use within @crucible-llvm@. The overrides are taken from the following
-- sources:
--
-- * Generic function overrides (from 'builtinLLVMOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
registerLLVMModuleOverrides ::
  forall sym bak arch p rtp as r.
  ( C.IsSymBackend sym bak
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ?lc :: TypeContext
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  Seq.Seq (CLLVM.OverrideTemplate p sym CLLVM.LLVM arch) ->
  [FilePath] ->
  bak ->
  C.HandleAllocator ->
  LLVMContext arch ->
  L.Module ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r (Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM))
  -- ^ Return a map of public function names to their overrides. This map does
  -- not include names of auxiliary functions, as they are intentionally hidden
  -- from other overrides.
registerLLVMModuleOverrides la builtinOvs paths bak halloc llvmCtx mod = do
  let decls = L.modDeclares mod
  registerLLVMOverrides la builtinOvs paths bak halloc llvmCtx decls

-- | Redirect handles for forward declarations in an LLVM S-expression program
-- to call the corresponding LLVM overrides. Treat any calls to unresolved
-- forward declarations as though the functions were skipped.
registerLLVMSexpProgForwardDeclarations ::
  ( C.IsSymInterface sym
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth 64
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  CLLVM.DataLayout ->
  C.GlobalVar Mem.Mem ->
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)
    {- ^ The map of public function names to their overrides. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMSexpProgForwardDeclarations la dl mvar funOvs =
  registerLLVMForwardDeclarations mvar funOvs $
    registerSkipOverride la dl mvar

-- | Redirect handles for forward declarations in an LLVM S-expression override
-- to call the corresponding LLVM overrides. Attempting to call an unresolved
-- forward declaration will raise an error.
registerLLVMOvForwardDeclarations ::
  Mem.HasLLVMAnn sym =>
  C.GlobalVar Mem.Mem ->
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)
    {- ^ The map of public function names to their overrides. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMOvForwardDeclarations mvar funOvs =
  registerLLVMForwardDeclarations mvar funOvs $ \fwdDecName _ ->
    declaredFunNotFound fwdDecName

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding LLVM overrides. If a forward declaration
-- name cannot be resolved to an override, then perform the supplied action.
registerLLVMForwardDeclarations ::
  Mem.HasLLVMAnn sym =>
  C.GlobalVar Mem.Mem ->
  Map.Map W4.FunctionName (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)
    {- ^ The map of public function names to their overrides. -} ->
  (forall args ret.
    W4.FunctionName ->
    C.FnHandle args ret ->
    C.OverrideSim p sym CLLVM.LLVM rtp as r ())
    {- ^ What to do when a forward declaration cannot be resolved. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym CLLVM.LLVM rtp as r ()
registerLLVMForwardDeclarations mvar funOvs cannotResolve fwdDecs =
  Foldable.for_ (Map.toList fwdDecs) $ \(fwdDecName, C.SomeHandle hdl) ->
    case Map.lookup fwdDecName funOvs of
      Nothing -> cannotResolve fwdDecName hdl
      Just (CLLVM.SomeLLVMOverride llvmOverride) ->
        bindLLVMOverrideFnHandle mvar hdl llvmOverride

-- | An LLVM function override, corresponding to a single S-expression file.
data LLVMFunctionOverride p sym =
  LLVMFunctionOverride
    { lfoPublicOverride :: CLLVM.SomeLLVMOverride p sym CLLVM.LLVM
      -- ^ The override for the public function, whose name matches that of the
      -- S-expression file.
    , lfoAuxiliaryOverrides :: [CLLVM.SomeLLVMOverride p sym CLLVM.LLVM]
      -- ^ Overrides for the auxiliary functions in the S-expression file.
    , lfoForwardDeclarations :: Map.Map W4.FunctionName C.SomeHandle
      -- ^ The map of names of forward declarations in the S-expression file to
      -- their handles.
    }
