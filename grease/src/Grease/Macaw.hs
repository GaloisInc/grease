{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw (
  SetupHook (..),
  emptyMacawMem,
  minimalArgShapes,
  regStructRepr,
  memConfigInitial,
  memConfigWithHandles,
  initState,
) where

import Control.Exception.Safe (MonadThrow)
import Control.Lens (to, (^.))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.BitVector.Sized qualified as BV
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.ElfLoader qualified as EL
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Backend qualified as Symbolic
import Data.Macaw.Symbolic.Memory qualified as MSM
import Data.Macaw.Symbolic.Memory.Lazy qualified as Symbolic
import Data.Macaw.Types qualified as MT
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Parameterized.Classes (TestEquality (..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.List qualified as P.List
import Data.Parameterized.Map qualified as MapF
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import Data.Word (Word64, Word8)
import GHC.Stack (HasCallStack, callStack)
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Diagnostic
import Grease.Macaw.Arch
import Grease.Macaw.Load.Relocation (RelocType (..))
import Grease.Macaw.Overrides.Address (AddressOverrides)
import Grease.Macaw.Overrides.SExp (MacawSExpOverride)
import Grease.Macaw.ResolveCall qualified as ResolveCall
import Grease.Macaw.SetupHook (SetupHook (SetupHook))
import Grease.Macaw.SimulatorHooks
import Grease.Macaw.SimulatorState (HasGreaseSimulatorState)
import Grease.Options qualified as Opts
import Grease.Panic (panic)
import Grease.Setup
import Grease.Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer
import Grease.Syntax (ResolvedOverridesYaml)
import Grease.Utility
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.GlobalState qualified as CS
import Stubs.Common qualified as Stubs
import Stubs.Syscall qualified as Stubs
import What4.Expr qualified as W4
import What4.FunctionName qualified as WFN
import What4.Interface qualified as WI
import What4.ProgramLoc as W4PL
import What4.Protocol.Online qualified as W4

emptyMacawMem ::
  forall (arch :: Type) (sym :: Type) (bak :: Type) (m :: Type -> Type) t st fs.
  ( MonadIO m
  , MonadThrow m
  , CB.IsSymBackend sym bak
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , sym ~ W4.ExprBuilder t st fs
  ) =>
  bak ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  Symbolic.MemoryModelContents ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  m (InitialMem sym, Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch))
emptyMacawMem bak arch macawMem mutGlobs relocs = do
  (initMem, ptrTable) <-
    Symbolic.newGlobalMemoryWith
      (globalMemoryHooks arch relocs)
      (Proxy @arch)
      bak
      (arch ^. archInfo . to (Symbolic.toCrucibleEndian . MI.archEndianness))
      mutGlobs
      macawMem
  pure (InitialMem initMem, ptrTable)

globalMemoryHooks ::
  forall arch.
  MM.MemWidth (MC.ArchAddrWidth arch) =>
  ArchContext arch ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  Symbolic.GlobalMemoryHooks (MC.ArchAddrWidth arch)
globalMemoryHooks arch relocs =
  Symbolic.GlobalMemoryHooks
    { Symbolic.populateRelocation = \bak relocMem relocSeg relocBaseAddr reloc -> do
        -- This function populates relocation types we support as appropriate.
        --
        -- If grease encounters a relocation type that it doesn't support, it
        -- fills it in with symbolic bytes. Most of the time, these bytes will
        -- never be read, because `assertRelocSupported` will throw an exception
        -- before we ever have a chance to simulate the code reading from the
        -- relocation. However, if `--skip-unsupported-relocs` is used, then the
        -- program can in fact read these bytes. We choose to use symbolic bytes
        -- instead of a constant value so that if we ever read from the middle
        -- of a relocation by mistake, we are more likely to trigger assertion
        -- failures elsewhere.
        let sym = CB.backendGetSym bak
        let relocAbsBaseAddr = relocAddrToAbsAddr relocMem relocSeg relocBaseAddr reloc
        case Map.lookup relocAbsBaseAddr relocs of
          Just relocType
            | Just supportedRelocType <-
                (arch ^. archRelocSupported) relocType ->
                case supportedRelocType of
                  RelativeReloc ->
                    relativeRelocHook sym reloc relocAbsBaseAddr
                  SymbolReloc ->
                    symbolRelocHook sym reloc relocAbsBaseAddr
          _ ->
            symbolicRelocation sym reloc Nothing
    }
 where
  -- Build a symbolic relocation value for `reloc`.  We use this for
  -- relocation types we don't yet support.
  symbolicRelocation ::
    forall sym.
    CB.IsSymInterface sym =>
    sym ->
    MM.Relocation (MC.ArchAddrWidth arch) ->
    Maybe String ->
    IO [WI.SymBV sym 8]
  symbolicRelocation sym reloc mName = do
    let name = fromMaybe "unknown" mName ++ "-reloc"
    traverse (symbolicByte sym name) [0 .. MM.relocationSize reloc - 1]

  -- Construct a symbolic byte.
  symbolicByte ::
    forall sym.
    CB.IsSymInterface sym =>
    sym ->
    -- The prefix to use in the symbolic byte's name.
    String ->
    -- The index to use as a suffix symbolic byte's name (zero-indexed).
    Int ->
    IO (WI.SymBV sym 8)
  symbolicByte sym name idx = do
    let symbol = WI.safeSymbol $ name ++ "-byte" ++ show idx
    WI.freshConstant sym symbol WI.knownRepr

  -- Convert a MemAddr to an absolute address.
  relocAddrToAbsAddr ::
    MM.Memory (MC.ArchAddrWidth arch) ->
    MM.MemSegment (MC.ArchAddrWidth arch) ->
    MC.ArchMemAddr arch ->
    MM.Relocation (MC.ArchAddrWidth arch) ->
    MM.MemWord (MC.ArchAddrWidth arch)
  relocAddrToAbsAddr mem seg addr reloc =
    case MM.resolveRegionOff mem (MM.addrBase addr) (MM.addrOffset addr) of
      Just addrOff -> MM.segmentOffset seg + MM.segoffOffset addrOff
      Nothing ->
        panic
          "relocAddrToAbsAddr"
          [ "Failed to resolve relocation"
          , "Relocation: " ++ show reloc
          , "Address:    " ++ show addr
          ]

  -- Compute the address that a relocation references and convert it to a
  -- list of bytes.
  relocAddrBV ::
    forall sym.
    CB.IsSymInterface sym =>
    sym ->
    MM.Relocation (MC.ArchAddrWidth arch) ->
    MM.MemWord (MC.ArchAddrWidth arch) ->
    IO [WI.SymBV sym 8]
  relocAddrBV sym reloc relocAbsBaseAddr = do
    -- First, compute the address by adding the offset...
    let relocAddr = relocAbsBaseAddr + MM.relocationOffset reloc

    -- ...next, chunk up the address into bytes...
    let archAddrWidth = MM.memWidthNatRepr @(MC.ArchAddrWidth arch)
    let bv = BV.mkBV archAddrWidth (fromIntegral relocAddr)
    let bytesLE =
          fromMaybe
            ( panic
                "relocAddrBV"
                ["Failed to split bitvector into bytes - word size not a multiple of 8?"]
            )
            (BV.asBytesLE archAddrWidth bv)

    --- ...finally, convert each byte to a SymBV.
    traverse (WI.bvLit sym (WI.knownNat @8) . BV.word8) bytesLE

  -- Handle a RelativeReloc relocation. This is perhaps the simplest type of
  -- relocation to handle, as there are no symbol names to cross-reference.
  -- All we have to do is compute the address that the relocation references.
  relativeRelocHook ::
    forall sym.
    CB.IsSymInterface sym =>
    sym ->
    MM.Relocation (MC.ArchAddrWidth arch) ->
    MM.MemWord (MC.ArchAddrWidth arch) ->
    IO [WI.SymBV sym 8]
  relativeRelocHook = relocAddrBV

  -- Handle a SymbolReloc relocation (e.g., GLOB_DAT). We populate
  -- SymbolRelocs with the address of the relocation itself. This is a hack,
  -- but it is a useful one: the relocation address is a reasonably unique
  -- value, so if the program tries to call a function with this same address,
  -- then we can look up the address to determine if it is an external
  -- function and simulate it appropriately. See Note [Subtleties of resolving
  -- PLT stubs] (Wrinkle 1: .plt.got) in Grease.Macaw.PLT.
  symbolRelocHook ::
    forall sym.
    CB.IsSymInterface sym =>
    sym ->
    MM.Relocation (MC.ArchAddrWidth arch) ->
    MM.MemWord (MC.ArchAddrWidth arch) ->
    IO [WI.SymBV sym 8]
  symbolRelocHook sym reloc relocAbsBaseAddr =
    let
      -- First, convert the relocation address to a Word64. Note that the
      -- size of the address itself may be less than 64 bits (e.g., on
      -- 32-bit architectures), but we will truncate the Word64 to the
      -- appropriate size later.
      relocAbsBaseAddrWord64 :: Word64
      relocAbsBaseAddrWord64 = MM.memWordValue relocAbsBaseAddr

      relocSizeInt64 :: Int64
      relocSizeInt64 =
        fromIntegral @Int @Int64 (MM.relocationSize reloc)

      -- Next, convert the relocation address to a ByteString of the
      -- appropriate size and endianness.
      relocAbsBaseAddrBs :: BSL.ByteString
      relocAbsBaseAddrBs =
        case arch ^. archInfo . to MI.archEndianness of
          MM.LittleEndian ->
            BSL.take relocSizeInt64 $
              Builder.toLazyByteString $
                Builder.word64LE relocAbsBaseAddrWord64
          MM.BigEndian ->
            BSL.takeEnd relocSizeInt64 $
              Builder.toLazyByteString $
                Builder.word64BE relocAbsBaseAddrWord64

      -- Finally, create an individual list of bytes corresponding to the
      -- relocation address. This is what we will write to the memory that
      -- the relocation address points to.
      relocAbsBaseAddrWord8s :: [Word8]
      relocAbsBaseAddrWord8s = BSL.unpack relocAbsBaseAddrBs
     in
      traverse (WI.bvLit sym WI.knownRepr . BV.word8) relocAbsBaseAddrWord8s

minimalArgShapes ::
  forall arch sym bak m wptr.
  ( MonadIO m
  , MonadThrow m
  , CB.IsSymBackend sym bak
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , CLM.HasPtrWidth wptr
  , Symbolic.SymArchConstraints arch
  ) =>
  bak ->
  ArchContext arch ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.MemWord (MC.ArchAddrWidth arch)) ->
  m (ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.MacawCrucibleRegTypes arch))
minimalArgShapes _bak arch mbEntryAddr = do
  regShapes <- Symbolic.macawAssignToCrucM mkRegShape $ Symbolic.crucGenRegAssignment symFns
  pure $ ArgShapes regShapes
 where
  symFns = arch ^. archVals . to Symbolic.archFunctions

  mkRegShape ::
    forall tp.
    MC.ArchReg arch tp ->
    m (Shape (Symbolic.MacawExt arch) NoTag (Symbolic.ToCrucibleType tp))
  mkRegShape r =
    if
      | Just Refl <- testEquality r MC.sp_reg ->
          case MT.typeRepr r of
            MT.BVTypeRepr w ->
              case testEquality w $ MT.knownNat @(MC.ArchAddrWidth arch) of
                Just C.Refl -> pure (ShapeExt (arch ^. archStackPtrShape))
                Nothing -> panic "minimalArgShapes" ["Bad stack pointer width"]
      | Just Refl <- testEquality r MC.ip_reg
      , Just ipVal <- mbEntryAddr ->
          case MT.typeRepr r of
            MT.BVTypeRepr w ->
              case testEquality w $ MT.knownNat @(MC.ArchAddrWidth arch) of
                Just C.Refl ->
                  let bv = BV.mkBV w (fromIntegral (EL.memWordValue ipVal))
                   in pure (ShapeExt (ShapePtrBVLit NoTag w bv))
                Nothing -> panic "minimalArgShapes" ["Bad stack pointer width"]
      | otherwise -> do
          shape <- case MT.typeRepr r of
            MT.BoolTypeRepr -> pure (ShapeBool NoTag)
            MT.BVTypeRepr w -> pure $ ShapeExt (ShapePtrBV NoTag w)
            MT.TupleTypeRepr P.List.Nil -> pure $ ShapeStruct NoTag Ctx.empty
            _ ->
              -- None of our supported architectures have registers with types
              -- other than those listed above.
              panic
                "minimalArgShapes"
                [ "Could not determine minimal shape for register"
                , show (MC.prettyF r)
                , show (MT.typeRepr r)
                ]
          pure shape

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch = C.StructRepr . Symbolic.crucArchRegTypes $ arch ^. archVals . to Symbolic.archFunctions

-- | Produce an initial 'Symbolic.MemModelConfig' value that can do everything
-- @grease@'s Macaw backend needs, with the exception of looking up function or
-- syscall handles. ('memConfigWithHandles' does that part—see its Haddocks for
-- why it needs to exist as a separate function.) The 'Symbolic.MemModelConfig'
-- value that this function returns is suitable for overrides that do not need
-- to look up any handles.
memConfigInitial ::
  forall arch sym bak solver scope st fs p.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , CB.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ CB.OnlineBackend solver scope st fs
  , 16 C.<= MC.ArchAddrWidth arch
  , Show (ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , CLM.HasLLVMAnn sym
  , MSM.MacawProcessAssertion sym
  , Symbolic.HasMacawLazySimulatorState p sym (MC.ArchAddrWidth arch)
  , HasCallStack
  ) =>
  bak ->
  ArchContext arch ->
  Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch) ->
  Opts.SkipUnsupportedRelocs ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  Symbolic.MemModelConfig p sym arch CLM.Mem
memConfigInitial bak arch ptrTable skipUnsupportedRelocs relocs =
  lazyMemModelConfig
    { -- We deviate from the lazy macaw-symbolic memory model's settings and opt
      -- \*not* to concretize pointers before reads or writes. See gitlab#143 for
      -- further discussion on this point.
      Symbolic.resolvePointer = pure
    , -- Upon each read, we assert that we are not reading from an unsupported
      -- relocation type, throwing a helpful error message otherwise. The
      -- concreteImmutableGlobalRead field of MemModelConfig gives us a convenient
      -- way to hook each read without needing to re-implement all of the logic
      -- for MacawReadMem/MacawCondReadMem, which is quite involved.
      Symbolic.concreteImmutableGlobalRead = \memRep ptr -> do
        Monad.unless (Opts.getSkipUnsupportedRelocs skipUnsupportedRelocs) $ do
          loc <- WI.getCurrentProgramLoc (CB.backendGetSym bak)
          assertRelocSupported arch loc ptr relocs
        Symbolic.concreteImmutableGlobalRead lazyMemModelConfig memRep ptr
    }
 where
  lazyMemModelConfig = Symbolic.memModelConfig bak ptrTable

-- | Take a 'Symbolic.MemModelConfig' and augment it with the ability to look up
-- function and syscall handles. This needs to be defined separately from
-- 'memConfigInitial' in order to break a dependency cycle:
--
-- * 'memConfigWithHandles' needs the list of overridden functions (for
--   'lookupFunctionHandle'), which is produced by...
--
-- * 'mkMacawOverrideMap', which needs a 'Symbolic.MemModelConfig' that is
--   produced by...
--
-- * 'memConfigInitial'. (This is why we cannot combine 'memConfigInitial' and
--   'memConfigWithHandles' into a single function: it would need the list of
--   overridden functions produced by 'mkMacawOverrideMap', but
--   'mkMacawOverrideMap' would in turn need the 'Symbolic.MemModelConfig' that
--   the combined function would produce, leading to a cycle.)
memConfigWithHandles ::
  forall arch sym bak solver scope st fs p cExt.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , CB.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ CB.OnlineBackend solver scope st fs
  , 16 C.<= MC.ArchAddrWidth arch
  , Show (ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , CLM.HasLLVMAnn sym
  , HasGreaseSimulatorState p cExt sym arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , HasToConcretize p
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  -- | Map of dynamic function names to their addresses
  Map.Map WFN.FunctionName (MC.ArchSegmentOff arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  ResolvedOverridesYaml (MC.ArchAddrWidth arch) ->
  -- | Map of names of overridden syscalls to their implementations
  Map.Map WFN.FunctionName (Stubs.SomeSyscall p sym (Symbolic.MacawExt arch)) ->
  Opts.ErrorSymbolicFunCalls ->
  Opts.ErrorSymbolicSyscalls ->
  Opts.SkipInvalidCallAddrs ->
  -- | What to do when a forward declaration cannot be resolved (during symex).
  (WFN.FunctionName -> IO ()) ->
  Symbolic.MemModelConfig p sym arch CLM.Mem ->
  Symbolic.MemModelConfig p sym arch CLM.Mem
memConfigWithHandles bak logAction halloc arch memory symMap pltStubs dynFunMap funOvs funAddrOvs syscallOvs errorSymbolicFunCalls errorSymbolicSyscalls skipInvalidCallAddress errCb' memCfg =
  memCfg
    { Symbolic.lookupFunctionHandle = ResolveCall.lookupFunctionHandle bak logAction halloc arch memory symMap pltStubs dynFunMap funOvs funAddrOvs errorSymbolicFunCalls skipInvalidCallAddress lfhd
    , Symbolic.lookupSyscallHandle = ResolveCall.lookupSyscallHandle bak arch syscallOvs errorSymbolicSyscalls lsd
    }
 where
  lfhd = ResolveCall.defaultLookupFunctionHandleDispatch bak logAction halloc arch memory funOvs errCb'
  lsd = ResolveCall.defaultLookupSyscallDispatch bak logAction halloc arch

-- | Check whether a pointer points to a relocation address, and if so, assert
-- that the underlying relocation type is supported. If not, throw an exception.
-- This is a best effort attempt: if the read is symbolic, the check is skipped.
assertRelocSupported ::
  ( CB.IsSymInterface sym
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Show (ArchReloc arch)
  , HasCallStack
  ) =>
  ArchContext arch ->
  W4PL.ProgramLoc ->
  CLM.LLVMPtr sym (MC.ArchAddrWidth arch) ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  IO ()
assertRelocSupported arch loc (CLM.LLVMPointer _base offset) relocs =
  case WI.asBV offset of
    Nothing ->
      -- Symbolic read. Cannot check whether this is an unsupported relocation.
      return ()
    Just bv -> do
      -- Check whether this read is from an unsupported relocation type.
      let addr = MM.memWord (fromInteger (BV.asUnsigned bv))
      case Map.lookup addr relocs of
        Just relocType
          | Nothing <- (arch ^. archRelocSupported) relocType -> do
              let msg =
                    unlines
                      [ "Attempted to read from an unsupported relocation type ("
                          <> show relocType
                          <> ") at address "
                          <> show addr
                          <> "."
                      , "This may be due to a PLT stub that grease did not detect."
                      , "If so, try passing --plt-stub <ADDR>:<NAME>, where"
                      , "<ADDR> and <NAME> can be obtained by disassembling the"
                      , "relevant PLT section of the binary"
                      , "(.plt, .plt.got, .plt.sec, etc.)."
                      ]
              let reason = CS.Unsupported callStack msg
              let simErr = CS.SimError loc reason
              CB.abortExecBecause (CB.AssertionFailure simErr)
        _ ->
          pure ()

initState ::
  forall arch sym bak m t solver scope st fs p cExt.
  ( CB.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ CB.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , CLM.HasLLVMAnn sym
  , MSM.MacawProcessAssertion sym
  , ?memOpts :: CLM.MemOptions
  , HasGreaseSimulatorState p cExt sym arch
  , HasToConcretize p
  ) =>
  bak ->
  GreaseLogAction ->
  CS.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  C.HandleAllocator ->
  CS.GlobalVar CLM.Mem ->
  SetupMem sym ->
  CS.SymGlobalState sym ->
  SymIO.SomeOverrideSim sym () ->
  ArchContext arch ->
  SetupHook sym arch ->
  AddressOverrides arch ->
  -- | The initial personality, see
  -- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'
  p ->
  -- | The initial register state.
  ArchRegs sym arch ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  -- | The 'C.CFG' of the user-requested entrypoint function.
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) ->
  IO (CS.ExecState p sym (Symbolic.MacawExt arch) (CS.RegEntry sym (C.StructType (Symbolic.MacawCrucibleRegTypes arch))))
initState bak la macawExtImpl halloc mvar mem0 globs0 (SymIO.SomeOverrideSim initFsOv) arch setupHook tgtOvs initialPersonality initialRegs funOvs mbStartupOvCfg (C.SomeCFG cfg) = do
  let sym = CB.backendGetSym bak
  archStruct <- C.freshGlobalVar halloc "grease:archRegs" (Symbolic.crucGenRegStructType $ Symbolic.archFunctions (arch ^. archVals))
  (mem1, globs1) <- liftIO $ (arch ^. archInitGlobals) (Stubs.Sym sym bak) (getSetupMem mem0) globs0
  let globs2 = CS.insertGlobal mvar mem1 globs1
  let globs3 = CS.insertGlobal archStruct initialRegs globs2
  let extImpl = greaseMacawExtImpl arch bak la tgtOvs mvar archStruct macawExtImpl
  let cfgHdl = C.cfgHandle cfg
  let bindings =
        CS.FnBindings $
          C.insertHandleMap cfgHdl (CS.UseCFG cfg $ C.postdomInfo cfg) C.emptyHandleMap
  let ctx =
        CS.initSimContext
          bak
          (Mem.llvmIntrinsicTypes `MapF.union` SymIO.llvmSymIOIntrinsicTypes)
          halloc
          printHandle
          bindings
          extImpl
          initialPersonality
  let sRepr = regStructRepr arch
  pure
    $ CS.InitialState
      ctx
      globs3
      CS.defaultAbortHandler
      sRepr
    $ CS.runOverrideSim sRepr
    $ Symbolic.crucGenArchConstraints (arch ^. archVals . to Symbolic.archFunctions)
    $ do
      let SetupHook hook = setupHook
      hook bak mvar funOvs
      initFsOv
      let args = Ctx.singleton $ CS.RegEntry sRepr initialRegs
      r <-
        case mbStartupOvCfg of
          Nothing ->
            CS.callCFG cfg (CS.RegMap args)
          Just (C.SomeCFG startupOvCfg) -> do
            args' <- CS.callCFG startupOvCfg (CS.RegMap args)
            CS.callCFG cfg (CS.RegMap (Ctx.singleton args'))
      pure $ CS.regValue r
