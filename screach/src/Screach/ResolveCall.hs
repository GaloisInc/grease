module Screach.ResolveCall (
  ecfsLookupFunctionHandleDispatch,
) where

import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw.Arch
import Grease.Macaw.ResolveCall qualified as ResolveCall
import Grease.Macaw.SimulatorState
import Grease.Macaw.SkippedCall qualified as SkippedCall
import Lang.Crucible.FunctionHandle qualified as CFH
import What4.FunctionName qualified as WFN

-- | A 'ResolveCall.LookupFunctionHandleDispatch' specifically geared towards
-- ECFS files. ECFS files include shared libraries in a giant address space, so
-- if we look up a PLT stub without an override, we do /not/ skip it, but
-- instead we perform code discovery on the PLT stub's definition and simulate
-- it directly. This works because the PLT stub will jump to an entry in the GOT
-- (global offset table), and that GOT entry will have been filled in by the
-- dynamic loader by the time the ECFS file is created.
ecfsLookupFunctionHandleDispatch ::
  ( Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p cExt sym arch ret
  ) =>
  GreaseLogAction ->
  CFH.HandleAllocator ->
  ArchContext arch ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  ResolveCall.LookupFunctionHandleDispatch p sym arch ->
  ResolveCall.LookupFunctionHandleDispatch p sym arch
ecfsLookupFunctionHandleDispatch la halloc arch memory symMap pltStubs lfhd =
  let ResolveCall.LookupFunctionHandleDispatch defaultDispatch = lfhd
   in ResolveCall.LookupFunctionHandleDispatch $ \st mem regs lfhr -> do
        let dispatch' st' = defaultDispatch st' mem regs
        case lfhr of
          ResolveCall.SkippedFunctionCall (SkippedCall.PltNoOverride pltAddr _pltName) -> do
            (hdl, st') <-
              ResolveCall.discoverFuncAddr la halloc arch memory symMap pltStubs pltAddr st
            dispatch' st' $
              ResolveCall.DiscoveredFnHandle pltAddr hdl Nothing
          _ -> dispatch' st lfhr
