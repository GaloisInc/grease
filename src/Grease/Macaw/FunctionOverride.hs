{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImplicitParams #-}

-- | Functionality for converting 'Stubs.FunctionOverride's into functions
-- that can be simulated within @macaw-symbolic@.
module Grease.Macaw.FunctionOverride
  ( MacawFunctionOverride(..)
  , MacawFnHandle
  , MacawOverride
  , macawOverride
  , mkMacawOverrideMap
  , registerMacawSexpProgForwardDeclarations
  , registerMacawOvForwardDeclarations
  ) where

import Prelude ((<$>), ($), (.), IO, pure)

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import qualified Data.Sequence as Seq
import Data.Traversable (traverse)
import Data.Type.Equality (type (~))
import System.IO (FilePath)

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.TraversableFC (fmapFC)

-- what4
import qualified What4.Expr as W4
import qualified What4.FunctionName as W4
import qualified What4.Protocol.Online as W4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Backend.Online as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as Mem
import Lang.Crucible.LLVM.TypeContext (TypeContext)

-- crucible-llvm-syntax
import Lang.Crucible.LLVM.Syntax (emptyParserHooks)

-- crucible-syntax
import qualified Lang.Crucible.Syntax.Concrete as CSyn
import qualified Lang.Crucible.Syntax.Prog as CSyn

-- macaw
import qualified Data.Macaw.CFG as MC

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- macaw-symbolic-syntax
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)

-- stubs-common
import qualified Stubs.FunctionOverride as Stubs
import qualified Stubs.FunctionOverride.ForwardDeclarations as Stubs

-- stubs-wrapper
import qualified Stubs.Wrapper as Stubs

import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw.Arch
import Grease.Macaw.SimulatorState
import Grease.Skip (registerSkipOverride)
import Grease.Syntax (parseProgram)
import Grease.Utility (declaredFunNotFound)

-- | A Macaw function override, corresponding to a single S-expression file.
data MacawFunctionOverride p sym arch =
  MacawFunctionOverride
    { mfoPublicFnHandle :: MacawFnHandle arch
      -- ^ The handle for the public function, whose name matches that of the
      -- S-expression file.
    , mfoPublicOverride :: MacawOverride p sym arch
      -- ^ The override for the public function, whose name matches that of the
      -- S-expression file.
    , mfoSomeFunctionOverride :: Stubs.SomeFunctionOverride p sym arch
      -- ^ The 'SomeFunctionOverride' value for S-expression file. This is
      -- primarily needed to compute the 'MacawOverride' above, but it is still
      -- convenient to keep the 'SomeFunctionOverride' value around to access
      -- the argument types, result type, auxiliary function bindings, forward
      -- declarations, etc.
    }

-- | Convert a 'Stubs.FunctionOverride' to a 'MacawOverride'. Really, this
-- functionality ought to be exposed from @stubs-common@. See
-- <https://github.com/GaloisInc/stubs/issues/16>.
macawOverride ::
  forall sym bak p arch args ret solver scope st fs.
  ( C.IsSymInterface sym
  , Mem.HasLLVMAnn sym
    -- For silly reasons, `stubs` requires the use of an online SMT solver
    -- connection in order to call `functionOverride`. See
    -- https://github.com/GaloisInc/stubs/issues/28.
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  C.GlobalVar Mem.Mem ->
  ArchContext arch ->
  Stubs.FunctionOverride p sym args arch ret ->
  MacawOverride p sym arch
macawOverride bak mvar archCtx fnOv =
    C.mkOverride' (Stubs.functionName fnOv) regsRepr ov
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
      C.OverrideSim
        p sym (Symbolic.MacawExt arch) r
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        (Symbolic.ArchRegStruct arch)
        (C.RegValue sym (Symbolic.ArchRegStruct arch))
    ov = do
      mem <- C.readGlobal mvar

      -- Construct the arguments
      argMap <- C.getOverrideArgs
      let argReg = massageRegAssignment $ C.regMap argMap
      (args, getVarArg) <- liftIO $
        (archCtx ^. archIntegerArguments)
          bak (Stubs.functionArgTypes fnOv)
          argReg mem

      -- Invoke the override
      retVal <-
        Stubs.functionOverride
          fnOv bak args getVarArg
          [] -- We don't currently make use of parent overrides

      -- Put the return value(s) into the appropriate register(s)
      (archCtx ^. archIntegerReturnRegisters)
        bak genArchVals
        (Stubs.functionReturnType fnOv)
        retVal argReg

-- | Massage the 'C.RegEntry' 'Ctx.Assignment' that 'C.getOverrideArgs'
-- provides into the form that 'archIntegerArguments' expects.
massageRegAssignment ::
     Ctx.Assignment (C.RegEntry sym) (Ctx.EmptyCtx Ctx.::> C.StructType ctx)
  -> Ctx.Assignment (C.RegValue' sym) ctx
massageRegAssignment = C.unRV . Ctx.last . fmapFC (C.RV . C.regValue)

-- | Construct a 'Map.Map' of names of function overrides to their corresponding
-- 'MacawFunctionOverride's, suitable for use within @macaw-symbolic@. The
-- overrides are taken from the following sources:
--
-- * Generic function overrides (e.g., from 'builtinStubsOverrides')
--
-- * User-defined overrides from S-expressions (see 'loadOverrides')
mkMacawOverrideMap ::
  forall sym bak arch solver scope st fs p.
  ( C.IsSymInterface sym
  , W4.OnlineSolver solver
  , ?memOpts :: Mem.MemOptions
  , ?lc :: TypeContext
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
    {- ^ The built-in overrides. -} ->
  [FilePath] {- ^ The paths of each user-supplied override file. -} ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  ArchContext arch ->
  IO (Map.Map W4.FunctionName (MacawFunctionOverride p sym arch))
mkMacawOverrideMap bak builtinOvs userOvPaths halloc mvar archCtx = do
  userOvs <- loadOverrides userOvPaths halloc
  -- Note the order here: due to how Map.fromList works, user-specified
  -- overrides will take precedence over builtin overrides, since
  -- Map.fromList favors things that appear later in the list in the event
  -- that there are duplicate names.
  let allOvs = builtinOvs <> userOvs
  Map.fromList . Foldable.toList <$>
    traverse
      (\someFnOv@(Stubs.SomeFunctionOverride fnOv) -> do
        let macawPublicOv = macawOverride bak mvar archCtx fnOv
        macawPublicHdl <- liftIO $
          C.mkHandle'
            halloc (C.overrideName macawPublicOv)
            (Ctx.singleton regsRepr) regsRepr
        let macawFnOv =
              MacawFunctionOverride
                { mfoPublicFnHandle = macawPublicHdl
                , mfoPublicOverride = macawPublicOv
                , mfoSomeFunctionOverride = someFnOv
                }
        pure (Stubs.functionName fnOv, macawFnOv))
      allOvs
  where
    regsRepr :: C.TypeRepr (Symbolic.ArchRegStruct arch)
    regsRepr =
      C.StructRepr $
      Symbolic.crucArchRegTypes $
      Symbolic.archFunctions $
      archCtx ^. archVals

-- | Parse overrides in the Macaw S-expression syntax.
loadOverrides ::
  Symbolic.SymArchConstraints arch =>
  [FilePath] ->
  C.HandleAllocator ->
  IO (Seq.Seq (Stubs.SomeFunctionOverride p sym arch))
loadOverrides paths halloc =
  traverse
    (\path -> loadOverride path halloc)
    (Seq.fromList paths)

-- | Parse an override in the Macaw S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  forall sym arch p.
  Symbolic.SymArchConstraints arch =>
  FilePath ->
  C.HandleAllocator ->
  IO (Stubs.SomeFunctionOverride p sym arch)
loadOverride path halloc = do
  let ?parserHooks = machineCodeParserHooks Proxy emptyParserHooks
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  Stubs.parsedProgToFunctionOverride path prog

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. Treat any calls to unresolved
-- forward declarations as though the functions were skipped.
registerMacawSexpProgForwardDeclarations ::
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  , Mem.HasLLVMAnn sym
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  , ?memOpts :: Mem.MemOptions
  ) =>
  bak ->
  GreaseLogAction ->
  CLLVM.DataLayout ->
  C.GlobalVar Mem.Mem ->
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch)
    {- ^ The map of public function names to their overrides. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawSexpProgForwardDeclarations bak la dl mvar funOvs =
  registerMacawForwardDeclarations bak funOvs $
    registerSkipOverride la dl mvar

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. Attempting to call an
-- unresolved forward declaration will raise an error.
registerMacawOvForwardDeclarations ::
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  bak ->
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch)
    {- ^ The map of public function names to their overrides. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawOvForwardDeclarations bak funOvs =
  registerMacawForwardDeclarations bak funOvs $ \fwdDecName _ ->
    declaredFunNotFound fwdDecName

-- | Redirect handles for forward declarations in an S-expression file to
-- actually call the corresponding Macaw overrides. If a forward declaration
-- name cannot be resolved to an override, then perform the supplied action.
registerMacawForwardDeclarations ::
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  ) =>
  bak ->
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch)
    {- ^ The map of public function names to their overrides. -} ->
  (forall args ret.
    W4.FunctionName ->
    C.FnHandle args ret ->
    C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ())
    {- ^ What to do when a forward declaration cannot be resolved. -} ->
  Map.Map W4.FunctionName C.SomeHandle
    {- ^ The map of forward declaration names to their handles. -} ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerMacawForwardDeclarations bak funOvs cannotResolve fwdDecs =
  Foldable.forM_ (Map.toList fwdDecs) $ \(decName, C.SomeHandle hdl) ->
    case Map.lookup decName funOvs of
      Nothing -> cannotResolve decName hdl
      Just mfo -> do
        let someForwardedOv = mfoSomeFunctionOverride mfo
            forwardedOv =
              Stubs.mkForwardDeclarationOverride
                bak
                -- We don't use parent overrides, hence the []
                (someForwardedOv NE.:| [])
                decName hdl
        C.bindFnHandle hdl (C.UseOverride forwardedOv)
