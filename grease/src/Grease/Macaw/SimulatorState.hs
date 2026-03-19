{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.SimulatorState (
  -- * @GreaseSimulatorState@
  GreaseSimulatorState (..),
  mkGreaseSimulatorState,
  HasGreaseSimulatorState (..),
  HasDiscoveryState (..),

  -- * Lenses for @GreaseSimulatorState@
  gssPersonality,
  discoveredFnHandles,
  discoveryStateRef,
  syscallHandles,
  macawLazySimulatorState,
  gssRecordState,
  gssReplayState,
  stateDiscoveredFnHandles,
  stateSyscallHandles,
  stateMacawLazySimulatorState,

  -- * Auxiliary definitions
  MacawFnHandle,
  MacawOverride,
) where

import Control.Lens (Lens')
import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import GHC.TypeLits (type Natural)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Personality qualified as GP
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as CR
import Lang.Crucible.Types qualified as CT
import Stubs.Syscall qualified as Stubs

-- | The Crucible state extension for holding @grease@-specific state.
type GreaseSimulatorState ::
  -- @sym@: the symbolic backend expression type
  Type ->
  -- @bak@: the symbolic backend type
  Type ->
  -- @t@: the nonce generator scope
  Type ->
  -- @cExt@: debugger command extension type
  Type ->
  -- @arch@: the machine code architecture
  Type ->
  -- @ret@: the Crucible return type (usually @'Symbolic.ArchRegStruct' arch@)
  CT.CrucibleType ->
  -- @argTys@: Crucible argument types for the target function
  CT.Ctx CT.CrucibleType ->
  -- @wptr@: pointer width
  Natural ->
  Type
data GreaseSimulatorState sym bak t cExt arch ret argTys wptr = GreaseSimulatorState
  { _gssPersonality :: GP.Personality sym bak t cExt (Symbolic.MacawExt arch) ret argTys wptr
  -- ^ The shared personality core. See 'Grease.Personality.Personality'.
  , _discoveryStateRef :: IORef (Discovery.DiscoveryState arch)
  -- ^ A shared, mutable 'Discovery.DiscoveryState' used for incremental code
  -- discovery. Each call to 'Grease.Macaw.Discovery.discoverFunctionIncremental'
  -- reads and updates this state, so successive discoveries benefit from
  -- knowledge accumulated by previous ones (e.g., known function boundaries).
  , _discoveredFnHandles :: Map.Map (MC.ArchSegmentOff arch) (MacawFnHandle arch)
  -- ^ A map of discovered function addresses to their handles. Any time a new
  -- function is discovered (see @Note [Incremental code discovery]@), it will
  -- be added to this map so that it can be looked up in future invocations of
  -- that function.
  , _syscallHandles :: MapF.MapF Stubs.SyscallNumRepr Stubs.SyscallFnHandle
  -- ^ A map of syscall numbers (that have been invoked thus far during
  -- simulation) to their handles. Any time a new syscall is simulated, it
  -- will be added to this map so that it can be looked up in future
  -- invocations of that syscall.
  --
  -- We track syscall handles here for a different reason than why we track
  -- '_discoveredFnHandles'. The reason we track '_syscallHandles' is a
  -- practical one: it is difficult to ascertain the types of syscall
  -- function handles until simulation enters a
  -- 'Symbolic.LookupSyscallHandle', which quantifies the types of the
  -- syscall's argument and return registers in a rank-n type. As such, we
  -- cannot easily create a map of all syscall handles ahead of time.
  -- Tracking syscall handles in the 'GreaseSimulatorState' avoids this
  -- problem, as it allows us to wait until we are within a
  -- 'Symbolic.LookupSyscallHandle', where the syscall register types are
  -- within reach.
  --
  -- Strictly speaking, we don't /have/ to track the syscall handles here, as
  -- we could simply allocate a new handle every time we invoke any syscall.
  -- The downside is that if we repeatedly invoked the same syscall, then we
  -- would constantly overwrite the previous handle that we allocated before
  -- with a new one.
  , _macawLazySimulatorState :: Symbolic.MacawLazySimulatorState sym (MC.ArchAddrWidth arch)
  -- ^ The state used in the lazy @macaw-symbolic@ memory model, on top of
  -- which @grease@ is built.
  , _gssRecordState ::
      CR.RecordState
        (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
        sym
        (Symbolic.MacawExt arch)
        (CS.RegEntry sym ret)
  , _gssReplayState ::
      CR.ReplayState
        (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
        sym
        (Symbolic.MacawExt arch)
        (CS.RegEntry sym ret)
  }

-- | Create a 'GreaseSimulatorState' with the given configuration.
mkGreaseSimulatorState ::
  forall sym bak t cExt arch ret argTys wptr p.
  (p ~ GreaseSimulatorState sym bak t cExt arch ret argTys wptr) =>
  GP.Personality sym bak t cExt (Symbolic.MacawExt arch) ret argTys wptr ->
  IORef (Discovery.DiscoveryState arch) ->
  CR.RecordState p sym (Symbolic.MacawExt arch) (CS.RegEntry sym ret) ->
  CR.ReplayState p sym (Symbolic.MacawExt arch) (CS.RegEntry sym ret) ->
  p
mkGreaseSimulatorState pers discStateRef recState repState =
  GreaseSimulatorState
    { _gssPersonality = pers
    , _discoveryStateRef = discStateRef
    , _discoveredFnHandles = Map.empty
    , _syscallHandles = MapF.empty
    , _macawLazySimulatorState = Symbolic.emptyMacawLazySimulatorState
    , _gssRecordState = recState
    , _gssReplayState = repState
    }

-- | A class for Crucible personality types @p@ which contain a
-- 'GreaseSimulatorState'. @grease@'s code is polymorphic over
-- 'HasGreaseSimulatorState' instances so that downstream @grease@ users can
-- supply their own personality types that extend 'GreaseSimulatorState'
-- further.
class
  ( Symbolic.HasMacawLazySimulatorState p sym (MC.ArchAddrWidth arch)
  , GSN.HasServerSocketFds p
  , GP.HasPersonality p sym bak t cExt (Symbolic.MacawExt arch) ret argTys wptr
  ) =>
  HasGreaseSimulatorState p sym bak t cExt arch ret argTys wptr
    | p -> sym bak t cExt arch ret argTys wptr
  where
  greaseSimulatorState :: Lens' p (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)

-- | A class for personality types that provide access to a shared, mutable
-- 'Discovery.DiscoveryState'. This enables incremental code discovery where
-- each successive function discovery benefits from knowledge accumulated by
-- previous ones.
--
-- The 'IORef' is intentional: when the scheduler snapshots a 'SimState' into
-- a 'WorkItem', the 'IORef' ensures that all suspended paths share the same
-- discovery knowledge, avoiding redundant rediscovery.
class HasDiscoveryState p arch | p -> arch where
  getDiscoveryStateRef :: p -> IORef (Discovery.DiscoveryState arch)

-----
-- These types should probably be defined in Grease.Macaw.FunctionOverride, but
-- they are instead defined here to avoid import cycles.
-----

-- | A 'C.FnHandle' suitable for use within @macaw-symbolic@.
type MacawFnHandle arch =
  C.FnHandle
    (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
    (Symbolic.ArchRegStruct arch)

-- | An 'C.Override' suitable for use within @macaw-symbolic@.
type MacawOverride p sym arch =
  CS.Override
    p
    sym
    (Symbolic.MacawExt arch)
    (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
    (Symbolic.ArchRegStruct arch)

-----
-- Lenses (generated at the very bottom of the file to avoid Template Haskell
-- staging restrictions)
-----

makeLenses ''GreaseSimulatorState

instance
  (ret ~ Symbolic.ArchRegStruct arch) =>
  Dbg.HasContext (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) cExt sym (Symbolic.MacawExt arch) ret
  where
  context = gssPersonality . GP.pDbgContext
  {-# INLINE context #-}

instance GP.HasMemVar (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) where
  getMemVar = Lens.view (gssPersonality . GP.pMemVar)

instance ToConc.HasToConcretize (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) where
  toConcretize = Lens.view (gssPersonality . GP.pToConcretize)

instance GP.HasPersonality (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) sym bak t cExt (Symbolic.MacawExt arch) ret argTys wptr where
  personality = gssPersonality
  {-# INLINE personality #-}

instance
  (MC.ArchAddrWidth arch ~ ww) =>
  Symbolic.HasMacawLazySimulatorState
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    sym
    ww
  where
  macawLazySimulatorState = macawLazySimulatorState

instance
  HasGreaseSimulatorState
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    sym
    bak
    t
    cExt
    arch
    ret
    argTys
    wptr
  where
  greaseSimulatorState = id

instance HasDiscoveryState (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) arch where
  getDiscoveryStateRef = Lens.view discoveryStateRef

instance GSN.HasServerSocketFds (GreaseSimulatorState sym bak t cExt arch ret argTys wptr) where
  serverSocketFdsL = gssPersonality . GP.pServerSocketFds

instance
  CR.HasRecordState
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    sym
    (Symbolic.MacawExt arch)
    (CS.RegEntry sym ret)
  where
  recordState = gssRecordState
  {-# INLINE recordState #-}

instance
  CR.HasReplayState
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    (GreaseSimulatorState sym bak t cExt arch ret argTys wptr)
    sym
    (Symbolic.MacawExt arch)
    (CS.RegEntry sym ret)
  where
  replayState = gssReplayState
  {-# INLINE replayState #-}

stateDiscoveredFnHandles ::
  HasGreaseSimulatorState p sym bak t cExt arch ret argTys wptr =>
  Lens'
    (CS.SimState p sym ext r f a)
    (Map.Map (MC.ArchSegmentOff arch) (MacawFnHandle arch))
stateDiscoveredFnHandles =
  CS.stateContext
    . CS.cruciblePersonality
    . greaseSimulatorState
    . discoveredFnHandles

stateSyscallHandles ::
  HasGreaseSimulatorState p sym bak t cExt arch ret argTys wptr =>
  Lens'
    (CS.SimState p sym ext r f a)
    (MapF.MapF Stubs.SyscallNumRepr Stubs.SyscallFnHandle)
stateSyscallHandles =
  CS.stateContext
    . CS.cruciblePersonality
    . greaseSimulatorState
    . syscallHandles

stateMacawLazySimulatorState ::
  HasGreaseSimulatorState p sym bak t cExt arch ret argTys wptr =>
  Lens'
    (CS.SimState p sym ext r f a)
    (Symbolic.MacawLazySimulatorState sym (MC.ArchAddrWidth arch))
stateMacawLazySimulatorState =
  CS.stateContext
    . CS.cruciblePersonality
    . greaseSimulatorState
    . macawLazySimulatorState

{-
Note [Incremental code discovery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grease will not perform code discovery unless it needs to, as:

1. Code discovery is fairly expensive, and
2. In large binaries, one typically only needs to discover a portion of the
   functions available in the binary.

Because of this, grease will only discover one function at a time, and only if
grease needs to find an address that has not previously been discovered before.
Here is an outline of how incremental code discovery works:

1. At the start of simulation, the discoveredFnHandles in the
   GreaseSimulatorState will only contain the user-requested entrypoint
   function.

2. During simulation, if grease encounters a non-overridden function that is not
   in the discoveredFnHandles map, then perform code discovery on it, add it to
   the map, and then continue by invoking the CFG for that function.

3. Later, if a previously discovered function is invoked once more, then we can
   simply look it up in the discoveredFnHandles map and call the handle for that
   function. This means that we will never need to perform code discovery on a
   function more than once.
-}
