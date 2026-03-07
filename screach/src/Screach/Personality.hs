{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Screach.Personality (
  ScreachSimulatorState (..),
  greaseSimulatorState,
  recordState,
  replayState,
  mkScreachSimulatorState,
) where

import Control.Lens (makeLenses)
import Control.Lens qualified as Lens
import Data.Kind (Type)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as MS
import Data.Macaw.Symbolic.Debug qualified as MDebug
import Data.Parameterized.Ctx qualified as C
import GHC.TypeLits (type Natural)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Macaw.SimulatorState qualified as GMSS
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.FunctionHandle (HandleAllocator)
import Lang.Crucible.Simulator.RecordAndReplay qualified as CSR
import Lang.Crucible.Types qualified as CT
import Screach.Distance qualified as Dist
import Screach.RefineFeature qualified as RFT
import Screach.SchedulerFeature qualified as Sched
import Screach.ShortestDistanceScheduler qualified as ShortDistSched

-- | The Screach \"personality\".
--
-- See 'Lang.Crucible.Simulator.cruciblePersonality'.
-- The simulator state holds on to the aty and rtp for the function under analysis.
-- These parameters encode the type of the crucible function that is being summarized so that the personality itself
-- can hold the 'RefinementData' (summary) for GREASE.
-- * @aty@: Crucible argument types for the target function
-- * @t@ the scope for annotation nonces
-- * @rtp@: Crucible simulator return type
-- * @w@: pointer width (Natural)
type ScreachSimulatorState ::
  Type -> Type -> Type -> Type -> Type -> Type -> Type -> C.Ctx CT.CrucibleType -> Natural -> Type
data ScreachSimulatorState p sym bak ext rtp arch t aty w = ScreachSimulatorState
  { _greaseSimulatorState :: GMSS.GreaseSimulatorState MDebug.MacawCommand sym arch
  , _schedulerState ::
      Sched.SchedulerState (ScreachSimulatorState p sym bak ext rtp arch t aty w) sym ext rtp
  , _distState :: Dist.DijkstraCaches
  , _recordState ::
      CSR.RecordState (ScreachSimulatorState p sym bak ext rtp arch t aty w) sym ext rtp
  , _replayState ::
      CSR.ReplayState (ScreachSimulatorState p sym bak ext rtp arch t aty w) sym ext rtp
  , _refineState :: RFT.SrchRefineData sym bak t ext aty w
  }

makeLenses ''ScreachSimulatorState

instance ShortDistSched.HasDistancesState (ScreachSimulatorState p sym bak ext rtp arch t aty w) where
  distanceState = distState

instance
  Sched.HasSchedulerState
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    sym
    ext
    rtp
  where
  schedulerState = schedulerState

instance
  ( ext ~ MS.MacawExt arch
  , ret ~ MS.ArchRegStruct arch
  ) =>
  Dbg.HasContext (ScreachSimulatorState p sym bak ext rtp arch t aty w) MDebug.MacawCommand sym ext ret
  where
  context = greaseSimulatorState . Dbg.context
  {-# INLINE context #-}

instance
  CSR.HasRecordState
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    sym
    ext
    rtp
  where
  recordState = recordState
  {-# INLINE recordState #-}

instance
  CSR.HasReplayState
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    sym
    ext
    rtp
  where
  replayState = replayState
  {-# INLINE replayState #-}

instance
  (MC.ArchAddrWidth arch ~ w) =>
  MS.HasMacawLazySimulatorState (ScreachSimulatorState p sym bak ext rtp arch t aty w) sym w
  where
  macawLazySimulatorState = greaseSimulatorState . GMSS.macawLazySimulatorState
  {-# INLINEABLE macawLazySimulatorState #-}

instance GSN.HasServerSocketFds (ScreachSimulatorState p sym bak ext rtp arch t aty w) where
  serverSocketFdsL = greaseSimulatorState . GSN.serverSocketFdsL
  {-# INLINE serverSocketFdsL #-}

instance
  (MC.ArchAddrWidth arch ~ w) =>
  GMSS.HasGreaseSimulatorState
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    MDebug.MacawCommand
    sym
    arch
  where
  greaseSimulatorState = greaseSimulatorState
  {-# INLINE greaseSimulatorState #-}

instance ToConc.HasToConcretize (ScreachSimulatorState p sym bak ext rtp arch t aty w) where
  toConcretize = Lens.view (greaseSimulatorState . Lens.to ToConc.toConcretize)

instance
  RFT.HasRefinmentState
    (ScreachSimulatorState p sym bak ext rtp arch t aty w)
    sym
    bak
    t
    ext
    aty
    w
  where
  refinementState = refineState

mkScreachSimulatorState ::
  forall sym bak t ext aty arch p rtp w.
  sym ->
  HandleAllocator ->
  GMSS.GreaseSimulatorState MDebug.MacawCommand sym arch ->
  RFT.SrchRefineData sym bak t ext aty w ->
  IO (ScreachSimulatorState p sym bak ext rtp arch t aty w)
mkScreachSimulatorState sym halloc gss schrRefineData = do
  recState <- CSR.mkRecordState halloc
  empTrace <- CSR.emptyRecordedTrace sym
  repState <- CSR.mkReplayState halloc empTrace
  pure
    ScreachSimulatorState
      { _greaseSimulatorState = gss
      , _schedulerState = Sched.emptySchedulerState
      , _distState = Dist.emptyDijkstraCaches
      , _recordState = recState
      , _replayState = repState
      , _refineState = schrRefineData
      }
{-# INLINEABLE mkScreachSimulatorState #-}
