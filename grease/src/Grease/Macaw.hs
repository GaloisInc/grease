{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw
  ( SetupHook(..)
  , emptyMacawMem
  , minimalArgShapes
  , regStructRepr
  , memConfigInitial
  , memConfigWithHandles
  , initState
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens ((^.), to)
import Control.Monad.IO.Class (MonadIO(..))
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
import Data.Macaw.Symbolic.Memory.Lazy qualified as Symbolic
import Data.Macaw.Types qualified as MT
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Parameterized.Classes (TestEquality(..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.List qualified as P.List
import Data.Parameterized.Map qualified as MapF
import Data.Proxy (Proxy(Proxy))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Type.Equality ((:~:)(Refl))
import Data.Word (Word8, Word64)
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Diagnostic
import Grease.Macaw.Arch
import Grease.Macaw.FunctionOverride
import Grease.Macaw.Load.Relocation (RelocType(..))
import Grease.Macaw.ResolveCall qualified as ResolveCall
import Grease.Macaw.SimulatorHooks
import Grease.Macaw.SimulatorState
import Grease.Options qualified as Opts
import Grease.Panic (panic)
import Grease.Setup
import Grease.Shape
import Grease.Shape.NoTag (NoTag(NoTag))
import Grease.Shape.Pointer
import Grease.Utility
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.SymIO qualified as SymIO
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.GlobalState qualified as C
import Stubs.Common qualified as Stubs
import Stubs.Syscall qualified as Stubs
import What4.Expr qualified as W4
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4
import What4.Protocol.Online qualified as W4

-- | Hook to run before executing a CFG.
--
-- Note that @sym@ is a type parameter so that users can define 'SetupHook's
-- that reference a fixed @sym@ type.
newtype SetupHook sym arch
  = SetupHook
    (forall bak rtp a r solver scope st fs p.
      ( C.IsSymBackend sym bak
      , sym ~ W4.ExprBuilder scope st fs
      , bak ~ C.OnlineBackend solver scope st fs
      , W4.OnlineSolver solver
      , Mem.HasLLVMAnn sym
      , HasGreaseSimulatorState p sym arch
      , HasToConcretize p
      ) =>
      bak ->
      C.GlobalVar Mem.Mem ->
      -- Map of names of overridden functions to their implementations
      Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
      C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ())

emptyMacawMem ::
  forall (arch :: Type) (sym :: Type) (bak :: Type) (m :: Type -> Type) t st fs.
  ( MonadIO m
  , MonadThrow m
  , C.IsSymBackend sym bak
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  , sym ~ W4.ExprBuilder t st fs
  ) =>
  bak ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  Opts.MutableGlobalState ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  m (InitialMem sym, Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch))
emptyMacawMem bak arch macawMem mutGlobs relocs = do
  globs <-
    case mutGlobs of
      Opts.Initialized -> pure Symbolic.ConcreteMutable
      Opts.Symbolic -> pure Symbolic.SymbolicMutable
      Opts.Uninitialized -> throw (GreaseException "Macaw does not support uninitialized globals (macaw#372)")
  (initMem, ptrTable) <-
    Symbolic.newGlobalMemoryWith
      (globalMemoryHooks arch relocs)
      (Proxy @arch)
      bak
      (arch ^. archInfo . to (Symbolic.toCrucibleEndian . MI.archEndianness))
      globs
      macawMem
  pure (InitialMem initMem, ptrTable)

globalMemoryHooks ::
  forall arch.
  MM.MemWidth (MC.ArchAddrWidth arch) =>
  ArchContext arch ->
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch)
    {- ^ Map of relocation addresses and types -} ->
  Symbolic.GlobalMemoryHooks (MC.ArchAddrWidth arch)
globalMemoryHooks arch relocs = Symbolic.GlobalMemoryHooks {
    Symbolic.populateRelocation = \bak relocMem relocSeg relocBaseAddr reloc -> do
      -- This function populates relocation types we support as appropriate.
      --
      -- If grease encounters a relocation type that it doesn't support, then
      -- the `assertRelocSupported` function should throw an exception before
      -- we ever have a chance to simulate it. Still, we need to fill in
      -- _something_ here for the address spaces of unsupported relocations,
      -- so we fill them in with symbolic bytes. We choose to use symbolic bytes
      -- instead of a constant value so that if we ever read from the middle of
      -- a relocation by mistake, we are more likely to trigger assertion
      -- failures elsewhere.
      let sym = C.backendGetSym bak
      let relocAbsBaseAddr = relocAddrToAbsAddr relocMem relocSeg relocBaseAddr reloc
      case Map.lookup relocAbsBaseAddr relocs of
        Just relocType
          |  Just supportedRelocType <-
               (arch ^. archRelocSupported) relocType
          -> case supportedRelocType of
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
      C.IsSymInterface sym =>
      sym ->
      MM.Relocation (MC.ArchAddrWidth arch) ->
      Maybe String ->
      IO [W4.SymBV sym 8]
    symbolicRelocation sym reloc mName = do
      let name = fromMaybe "unknown" mName ++ "-reloc"
      traverse (symbolicByte sym name) [0 .. MM.relocationSize reloc-1]

    -- Construct a symbolic byte.
    symbolicByte ::
      forall sym.
      C.IsSymInterface sym =>
      sym ->
      -- | The prefix to use in the symbolic byte's name.
      String ->
      -- | The index to use as a suffix symbolic byte's name (zero-indexed).
      Int ->
      IO (W4.SymBV sym 8)
    symbolicByte sym name idx = do
      let symbol = W4.safeSymbol $ name ++ "-byte" ++ show idx
      W4.freshConstant sym symbol W4.knownRepr

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
        Nothing -> panic
                     "relocAddrToAbsAddr"
                     [ "Failed to resolve relocation"
                     , "Relocation: " ++ show reloc
                     , "Address:    " ++ show addr
                     ]

    -- Compute the address that a relocation references and convert it to a
    -- list of bytes.
    relocAddrBV ::
      forall sym.
      C.IsSymInterface sym =>
      sym ->
      MM.Relocation (MC.ArchAddrWidth arch) ->
      MM.MemWord (MC.ArchAddrWidth arch) ->
      IO [W4.SymBV sym 8]
    relocAddrBV sym reloc relocAbsBaseAddr = do
      -- First, compute the address by adding the offset...
      let relocAddr = relocAbsBaseAddr + MM.relocationOffset reloc

      -- ...next, chunk up the address into bytes...
      let archAddrWidth = MM.memWidthNatRepr @(MC.ArchAddrWidth arch)
      let bv = BV.mkBV archAddrWidth (fromIntegral relocAddr)
      let bytesLE = fromMaybe
            (panic
              "relocAddrBV"
              ["Failed to split bitvector into bytes - word size not a multiple of 8?"])
            (BV.asBytesLE archAddrWidth bv)

      --- ...finally, convert each byte to a SymBV.
      traverse (W4.bvLit sym (W4.knownNat @8) . BV.word8) bytesLE

    -- Handle a RelativeReloc relocation. This is perhaps the simplest type of
    -- relocation to handle, as there are no symbol names to cross-reference.
    -- All we have to do is compute the address that the relocation references.
    relativeRelocHook ::
      forall sym.
      C.IsSymInterface sym =>
      sym ->
      MM.Relocation (MC.ArchAddrWidth arch) ->
      MM.MemWord (MC.ArchAddrWidth arch) ->
      IO [W4.SymBV sym 8]
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
      C.IsSymInterface sym =>
      sym ->
      MM.Relocation (MC.ArchAddrWidth arch) ->
      MM.MemWord (MC.ArchAddrWidth arch) ->
      IO [W4.SymBV sym 8]
    symbolRelocHook sym reloc relocAbsBaseAddr =
      let -- First, convert the relocation address to a Word64. Note that the
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
          relocAbsBaseAddrWord8s = BSL.unpack relocAbsBaseAddrBs in

      traverse (W4.bvLit sym W4.knownRepr . BV.word8) relocAbsBaseAddrWord8s

minimalArgShapes ::
  forall arch sym bak m wptr.
  ( MonadIO m
  , MonadThrow m
  , C.IsSymBackend sym bak
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Mem.HasPtrWidth wptr
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

    mkRegShape :: forall tp. MC.ArchReg arch tp -> m (Shape (Symbolic.MacawExt arch) NoTag (Symbolic.ToCrucibleType tp))
    mkRegShape r =
      if | Just Refl <- testEquality r MC.sp_reg ->
           case MT.typeRepr r of
             MT.BVTypeRepr w ->
               case testEquality w $ MT.knownNat @(MC.ArchAddrWidth arch) of
                 Just C.Refl -> pure (ShapeExt (arch ^. archStackPtrShape))
                 Nothing -> throw $ GreaseException "Bad stack pointer width"
         | Just Refl <- testEquality r MC.ip_reg
         , Just ipVal <- mbEntryAddr ->
           case MT.typeRepr r of
             MT.BVTypeRepr w ->
               case testEquality w $ MT.knownNat @(MC.ArchAddrWidth arch) of
                 Just C.Refl ->
                   let bv = BV.mkBV w (fromIntegral (EL.memWordValue ipVal)) in
                   pure (ShapeExt (ShapePtrBVLit NoTag w bv))
                 Nothing -> throw $ GreaseException "Bad instruction pointer width"
         | otherwise -> do
           shape <- case MT.typeRepr r of
             MT.BoolTypeRepr -> pure (ShapeBool NoTag)
             MT.BVTypeRepr w ->
               case testEquality w $ MT.knownNat @(MC.ArchAddrWidth arch) of
                 Just C.Refl -> pure $ ShapeExt (ShapePtr NoTag (Offset 0) (ptrTarget Seq.Empty))
                 Nothing -> pure $ ShapeExt (ShapePtrBV NoTag w)
             MT.TupleTypeRepr P.List.Nil -> pure $ ShapeStruct NoTag Ctx.empty
             _ -> throw $ GreaseException "Could not determine minimal shape for register"
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
  , C.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , 16 C.<= MC.ArchAddrWidth arch
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , Symbolic.HasMacawLazySimulatorState p sym (MC.ArchAddrWidth arch)
  ) =>
  bak ->
  ArchContext arch ->
  Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch) ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  Symbolic.MemModelConfig p sym arch Mem.Mem
memConfigInitial bak arch ptrTable relocs =
  lazyMemModelConfig
  { -- We deviate from the lazy macaw-symbolic memory model's settings and opt
    -- *not* to concretize pointers before reads or writes. See gitlab#143 for
    -- further discussion on this point.
    Symbolic.resolvePointer = pure
    -- Upon each read, we assert that we are not reading from an unsupported
    -- relocation type, throwing a helpful error message otherwise. The
    -- concreteImmutableGlobalRead field of MemModelConfig gives us a convenient
    -- way to hook each read without needing to re-implement all of the logic
    -- for MacawReadMem/MacawCondReadMem, which is quite involved.
  , Symbolic.concreteImmutableGlobalRead = \memRep ptr -> do
      assertRelocSupported arch ptr relocs
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
  forall arch sym bak solver scope st fs p.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , C.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , 16 C.<= MC.ArchAddrWidth arch
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  , Mem.HasLLVMAnn sym
  , HasGreaseSimulatorState p sym arch
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
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
  Map.Map (MC.ArchSegmentOff arch) W4.FunctionName ->
  -- | Map of dynamic function names to their addresses
  Map.Map W4.FunctionName (MC.ArchSegmentOff arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
  -- | Map of names of overridden syscalls to their implementations
  Map.Map W4.FunctionName (Stubs.SomeSyscall p sym (Symbolic.MacawExt arch)) ->
  Opts.ErrorSymbolicFunCalls ->
  Opts.ErrorSymbolicSyscalls ->
  Symbolic.MemModelConfig p sym arch Mem.Mem ->
  Symbolic.MemModelConfig p sym arch Mem.Mem
memConfigWithHandles bak logAction halloc arch memory symMap pltStubs dynFunMap funOvs syscallOvs errorSymbolicFunCalls errorSymbolicSyscalls memCfg =
  memCfg
  { Symbolic.lookupFunctionHandle = ResolveCall.lookupFunctionHandle bak logAction halloc arch memory symMap pltStubs dynFunMap funOvs errorSymbolicFunCalls lfhd
  , Symbolic.lookupSyscallHandle = ResolveCall.lookupSyscallHandle bak arch syscallOvs errorSymbolicSyscalls lsd
  }
  where
    lfhd = ResolveCall.defaultLookupFunctionHandleDispatch bak logAction halloc arch memory funOvs
    lsd = ResolveCall.defaultLookupSyscallDispatch bak logAction halloc arch

-- | Check whether a pointer points to a relocation address, and if so, assert
-- that the underlying relocation type is supported. If not, throw an exception.
-- This is a best effort attempt: if the read is symbolic, the check is skipped.
assertRelocSupported ::
  ( C.IsSymInterface sym
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Show (ArchReloc arch)
  ) =>
  ArchContext arch ->
  Mem.LLVMPtr sym (MC.ArchAddrWidth arch) ->
  -- | Map of relocation addresses and types
  Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch) ->
  IO ()
assertRelocSupported arch (Mem.LLVMPointer _base offset) relocs =
  case W4.asBV offset of
    Nothing ->
      -- Symbolic read. Cannot check whether this is an unsupported relocation.
      return ()
    Just bv -> do
      -- Check whether this read is from an unsupported relocation type.
      let addr = MM.memWord (fromInteger (BV.asUnsigned bv))
      case Map.lookup addr relocs of
        Just relocType
          |  Nothing <- (arch ^. archRelocSupported) relocType
          -> throw $ GreaseException $ Text.unlines
               [ "Attempted to read from an unsupported relocation type (" <>
                 tshow relocType <>
                 ") at address " <> tshow addr <> "."
               , "This may be due to a PLT stub that grease did not detect."
               , "If so, try passing --plt-stub <ADDR>:<NAME>, where"
               , "<ADDR> and <NAME> can be obtained by disassembling the"
               , "relevant PLT section of the binary"
               , "(.plt, .plt.got, .plt.sec, etc.)."
               ]
        _ ->
          pure ()

initState ::
  forall arch sym bak m t solver scope st fs p.
  ( MonadIO m
  , MonadThrow m
  , C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , W4.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , Mem.HasLLVMAnn sym
  , ?memOpts :: Mem.MemOptions
  , HasGreaseSimulatorState p sym arch
  , HasToConcretize p
  ) =>
  bak ->
  GreaseLogAction ->
  C.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  SetupMem sym ->
  C.SymGlobalState sym ->
  SymIO.SomeOverrideSim sym () ->
  ArchContext arch ->
  Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch) ->
  SetupHook sym arch ->
  -- | The initial personality, see
  -- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'
  p ->
  -- | The initial register state.
  ArchRegs sym arch ->
  -- | Map of names of overridden functions to their implementations
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  -- | The 'C.CFG' of the user-requested entrypoint function.
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) ->
  m (C.ExecState p sym (Symbolic.MacawExt arch) (C.RegEntry sym (C.StructType (Symbolic.MacawCrucibleRegTypes arch))))
initState bak la macawExtImpl halloc mvar mem0 globs0 (SymIO.SomeOverrideSim initFsOv) arch ptrTable setupHook initialPersonality initialRegs funOvs mbStartupOvCfg (C.SomeCFG cfg) = do
  let sym = C.backendGetSym bak
  (mem1, globs1) <- liftIO $ (arch ^. archInitGlobals) (Stubs.Sym sym bak) (getSetupMem mem0) globs0
  let globs2 = C.insertGlobal mvar mem1 globs1
  let globMap = Symbolic.mapRegionPointers ptrTable
  let extImpl = greaseMacawExtImpl bak la globMap macawExtImpl
  let cfgHdl = C.cfgHandle cfg
  let bindings = C.FnBindings
        $ C.insertHandleMap cfgHdl (C.UseCFG cfg $ C.postdomInfo cfg) C.emptyHandleMap
  let ctx = C.initSimContext
        bak
        (Mem.llvmIntrinsicTypes `MapF.union` SymIO.llvmSymIOIntrinsicTypes)
        halloc
        printHandle
        bindings
        extImpl
        initialPersonality
  let sRepr = regStructRepr arch
  pure $
    C.InitialState
      ctx
      globs2
      C.defaultAbortHandler
      sRepr
      $ C.runOverrideSim sRepr
      $ Symbolic.crucGenArchConstraints (arch ^. archVals . to Symbolic.archFunctions)
      $ do
        let SetupHook hook = setupHook
        hook bak mvar funOvs
        initFsOv
        let args = Ctx.singleton $ C.RegEntry sRepr initialRegs
        r <-
          case mbStartupOvCfg of
            Nothing ->
              C.callCFG cfg (C.RegMap args)
            Just (C.SomeCFG startupOvCfg) -> do
              args' <- C.callCFG startupOvCfg (C.RegMap args)
              C.callCFG cfg (C.RegMap (Ctx.singleton args'))
        pure $ C.regValue r
