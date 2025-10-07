{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Overrides.Builtin (
  builtinStubsOverrides,
) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (toListFC)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Grease.LLVM.Overrides.Builtin (libcOverrides)
import Grease.Macaw.Overrides.Defs (customStubsOverrides)
import Grease.Macaw.Overrides.Networking (networkOverrides)
import Grease.Macaw.SimulatorState (HasGreaseSimulatorState)
import Grease.Panic qualified as Panic
import Grease.Utility (llvmOverrideName)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.Intrinsics.Cast qualified as Cast
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.LLVM.TypeContext qualified as TCtx
import Stubs.FunctionOverride qualified as Stubs
import Text.LLVM.AST qualified as L

-- | All of the @stubs@ overrides that work across all supported configurations.
--
-- c.f. 'builtinLLVMOverrides', which includes all of the functions here (i.e.,
-- from libc) and then some (i.e., LLVM intrinsics).
builtinStubsOverrides ::
  forall sym bak p arch.
  ( CB.IsSymBackend sym bak
  , ?memOpts :: CLM.MemOptions
  , ?lc :: TCtx.TypeContext
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , HasGreaseSimulatorState p sym arch
  ) =>
  bak ->
  C.GlobalVar CLM.Mem ->
  Symbolic.MemModelConfig p sym arch CLM.Mem ->
  SymIO.LLVMFileSystem (MC.ArchAddrWidth arch) ->
  Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
builtinStubsOverrides bak mvar mmConf fs =
  customOvs <> fromLlvmOvs <> networkingOvs
 where
  -- Custom overrides that are only applicable at the machine code level (and
  -- therefore do not belong in crucible-llvm).
  customOvs :: Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
  customOvs = Seq.fromList (customStubsOverrides mvar mmConf)

  -- Overrides that arise from crucible-llvm.
  fromLlvmOvs :: Seq.Seq (Stubs.SomeFunctionOverride p sym arch)
  fromLlvmOvs =
    -- We never need to make use of any non-standard IntrinsicsOptions.
    let ?intrinsicsOpts = CLI.defaultIntrinsicsOptions
     in Seq.fromList
          $ mapMaybe
            (\(CLI.SomeLLVMOverride ov) -> llvmToStubsOverride bak mvar ov)
          $ List.filter
            ( \(CLI.SomeLLVMOverride ov) ->
                L.decName (CLI.llvmOverride_declare ov)
                  `Set.notMember` excludedLibcOverrides
            )
            (libcOverrides fs)

  networkingOvs = networkOverrides fs mvar mmConf

  -- Overrides that we do not want to use at the binary level. If you add an
  -- override to this list, make sure to include a comment with the reason why
  -- the override is excluded.
  excludedLibcOverrides :: Set L.Symbol
  excludedLibcOverrides =
    -- These overrides read strings from memory in a way that is at odds with
    -- macaw-symbolic's lazy memory model (see gitlab#226). In particular,
    -- these use crucible-llvm's various operations for performing memory
    -- loads (`doLoad`, `loadRaw`, `loadString`, etc.). We have reimplemented
    -- a subset of these functions as machine code overrides (see `customOvs`
    -- above).
    Set.fromList
      [ "__assert_fail"
      , "__assert_rtn"
      , "__printf_chk"
      , "printf"
      , "puts"
      ]

-----
-- Turn LLVM overrides into Stubs FunctionOverrides
-----

-- | Helper to turn bitvector types into LLVM pointer types.
--
-- See module comment on "Lang.Crucible.LLVM.Intrinsics.Cast".
bvToPointer ::
  C.TypeRepr t ->
  C.Some C.TypeRepr
bvToPointer =
  \case
    C.BVRepr w -> C.Some (CLM.LLVMPointerRepr w)
    t -> C.Some t

-- | Transform an 'Mem.LLVMOverride' into a 'Stubs.SomeFunctionOverride'.
--
-- Does the necessary pipe-fitting of bitvectors to pointers, see module comment
-- on "Lang.Crucible.LLVM.Intrinsics.Cast".
--
-- This uses null/default values for several fields of 'Stubs.FunctionOverride'
-- that are more appropriate for overrides written in the S-expression syntax,
-- e.g., 'Stubs.functionGlobals'.
--
-- The override will not havoc any registers.
--
-- This will return 'Nothing' if we cannot convert the 'Mem.LLVMOverride'.
-- Currently, this only happens if the 'Mem.LLVMOverride' is for a variadic
-- function, as the approach that 'Mem.LLVMOverride' uses to represent varargs
-- is not easily convertible to the approach that @stubs@ uses.
llvmToStubsOverride ::
  CB.IsSymBackend sym bak =>
  CLM.HasLLVMAnn sym =>
  bak ->
  C.GlobalVar CLM.Mem ->
  CLI.LLVMOverride p sym (Symbolic.MacawExt arch) args ret ->
  Maybe (Stubs.SomeFunctionOverride p sym arch)
llvmToStubsOverride bak mvar llvmOv
  | isVariadic =
      Nothing
  | otherwise =
      Just $ runIdentity $ do
        C.Some args <- Identity $ Ctx.fromList (toListFC bvToPointer args0)
        C.Some ret <- Identity $ bvToPointer ret0

        let nm = llvmOverrideName llvmOv
        Identity $
          Stubs.SomeFunctionOverride $
            Stubs.FunctionOverride
              { Stubs.functionName = nm
              , Stubs.functionGlobals = Map.empty
              , Stubs.functionExterns = Map.empty
              , Stubs.functionArgTypes = args
              , Stubs.functionReturnType = ret
              , Stubs.functionAuxiliaryFnBindings = []
              , Stubs.functionForwardDeclarations = Map.empty
              , Stubs.functionOverride =
                  \_bak argVals _getVarArg _parents -> do
                    -- This won't panic because we're only using bvToPointer, and those casts
                    -- are supported by Cast.
                    let panic = Panic.panic "llvmToStubsOverride"
                    let fargs =
                          case Cast.castLLVMArgs nm bak args0 args of
                            Left err -> panic (Cast.printValCastError err)
                            Right f -> f
                    let fret =
                          case Cast.castLLVMRet nm bak ret0 ret of
                            Left err -> panic (Cast.printValCastError err)
                            Right f -> f

                    argVals' <- Cast.applyArgCast fargs argVals
                    retVal <- CLI.llvmOverride_def llvmOv mvar argVals'
                    retVal' <- Cast.applyValCast fret retVal
                    let regChanges = [] -- TODO: havoc registers?
                    pure (Stubs.OverrideResult regChanges retVal')
              }
 where
  args0 = CLI.llvmOverride_args llvmOv
  ret0 = CLI.llvmOverride_ret llvmOv

  -- crucible-llvm maintains the convention that any LLVMOverride for a
  -- variadic function (e.g., `printf`) will use `VectorRepr AnyRepr` as the
  -- last argument TypeRepr to represent the varargs' type.
  isVariadic :: Bool
  isVariadic =
    case Ctx.viewAssign args0 of
      Ctx.AssignExtend _ (C.VectorRepr C.AnyRepr) ->
        True
      _ ->
        False
