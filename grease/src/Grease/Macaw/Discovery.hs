-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Discovery (
  discoverFunction,
  discoverFunctionWith,
  discoverFunctionIncremental,
  mkInitialDiscoveryState,
) where

import Control.Lens (to, (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.IORef qualified as IORef
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Memory.ElfLoader qualified as EL
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Utils.IncComp qualified as IncComp
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (Some (Some))
import Grease.Diagnostic (Diagnostic (LoadDiagnostic))
import Grease.Macaw.Arch (ArchContext, ArchRegCFG, archInfo, archVals)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Load.Diagnostic qualified as Diag
import Grease.Utility (functionNameFromByteString, tshow)
import Lang.Crucible.FunctionHandle qualified as C
import Lumberjack qualified as LJ
import What4.FunctionName qualified as WFN
import What4.ProgramLoc qualified as WPL

-- | We pass this log function to @macaw@ to wrap discovery events in a custom
-- wrapper that we stream out with the rest of our diagnostics.
logDiscoveryEvent ::
  ( MonadIO m
  , MC.ArchConstraints arch
  , w ~ MC.RegAddrWidth (MC.ArchReg arch)
  ) =>
  LJ.LogAction IO Diagnostic ->
  Discovery.AddrSymMap w ->
  Discovery.DiscoveryEvent arch ->
  m ()
logDiscoveryEvent logAction symMap evt =
  liftIO $ LJ.writeLog logAction (LoadDiagnostic (Diag.DiscoveryEvent symMap evt))

-- | Run code discovery on a single function at the given address, streaming
-- out diagnostics that provide indications of progress.
--
-- Note that this will not explore any functions besides the one at the given
-- address, so this is meant to be used as a way to discover code incrementally.
-- (See @Note [Incremental code discovery]@ in "Grease.Macaw.SimulatorState".)
-- Moreover, this will not do any validity checking of the given address (e.g.,
-- checking if it inhabits an executable segment of memory) before running code
-- discovery, so it is the responsibility of the caller to perform these checks.
discoverFunction ::
  MonadIO m =>
  LJ.LogAction IO Diagnostic ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names. Although this function only
  -- explores a single function, it is still helpful to pass all of the
  -- entrypoint addresses because it can be used to recover the name of the
  -- function when logging.
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names. We must mark these as trusted
  -- function entry points—see @Note [Mark PLT stubs as trusted function
  -- entry points]@ for an explanation.
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  -- | The function address to discover.
  MC.ArchSegmentOff arch ->
  m (ArchRegCFG arch)
discoverFunction logAction halloc arch mem symMap pltStubs addr = do
  let archInf = arch ^. Arch.archInfo
  -- Mark the PLT stubs as trusted function entry points.
  -- See Note [Mark PLT stubs as trusted function entry points].
  let pltEntryPoints = Discovery.MayReturnFun <$ pltStubs
  let s0 =
        Discovery.emptyDiscoveryState mem symMap archInf
          & Discovery.trustedFunctionEntryPoints .~ pltEntryPoints
  MI.withArchConstraints archInf $ do
    (_state, Some funInfo) <-
      IncComp.processIncCompLogs (logDiscoveryEvent logAction symMap) $ IncComp.runIncCompM $ do
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunction addr
        let discoveryOpts = Discovery.defaultDiscoveryOptions
        res@(_, Some funInfo) <-
          IncComp.liftIncComp id $
            Discovery.discoverFunction discoveryOpts addr Discovery.UserRequest s0 []
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunctionDone funInfo
        pure res
    liftIO $
      Symbolic.mkFunRegCFG
        (arch ^. Arch.archVals . to Symbolic.archFunctions)
        halloc
        (functionNameFromByteString $ Discovery.discoveredFunName funInfo)
        (WPL.OtherPos . tshow) -- simply use addresses as source positions for now
        funInfo

-- | Create the initial 'Discovery.DiscoveryState' for a binary, with PLT
-- stubs marked as trusted function entry points. This state can be stored in
-- an 'IORef' and shared across incremental discoveries via
-- 'discoverFunctionIncremental'.
mkInitialDiscoveryState ::
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  Discovery.DiscoveryState arch
mkInitialDiscoveryState arch mem symMap pltStubs =
  let archInf = arch ^. archInfo
      pltEntryPoints = Discovery.MayReturnFun <$ pltStubs
  in Discovery.emptyDiscoveryState mem symMap archInf
       & Discovery.trustedFunctionEntryPoints .~ pltEntryPoints

-- | Discover a single function, returning the updated 'Discovery.DiscoveryState'.
-- Each call benefits from knowledge accumulated by previous discoveries
-- (e.g., known function boundaries). This maintains lazy, on-demand discovery
-- while allowing the caller to thread state however it sees fit.
discoverFunctionWith ::
  MonadIO m =>
  LJ.LogAction IO Diagnostic ->
  C.HandleAllocator ->
  ArchContext arch ->
  Discovery.DiscoveryState arch ->
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  MC.ArchSegmentOff arch ->
  m (ArchRegCFG arch, Discovery.DiscoveryState arch)
discoverFunctionWith logAction halloc arch s0 symMap addr = do
  let archInf = arch ^. archInfo
  MI.withArchConstraints archInf $ do
    (s', Some funInfo) <-
      IncComp.processIncCompLogs (logDiscoveryEvent logAction symMap) $ IncComp.runIncCompM $ do
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunction addr
        let discoveryOpts = Discovery.defaultDiscoveryOptions
        res@(_, Some funInfo) <-
          IncComp.liftIncComp id $
            Discovery.discoverFunction discoveryOpts addr Discovery.UserRequest s0 []
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunctionDone funInfo
        pure res
    cfg <- liftIO $
      Symbolic.mkFunRegCFG
        (arch ^. archVals . to Symbolic.archFunctions)
        halloc
        (functionNameFromByteString $ Discovery.discoveredFunName funInfo)
        (WPL.OtherPos . tshow)
        funInfo
    pure (cfg, s')

-- | Convenience wrapper around 'discoverFunctionWith' that reads and writes
-- a shared 'Discovery.DiscoveryState' via an 'IORef'. Useful for IO callbacks
-- (e.g., in distance computation) that cannot easily thread state.
discoverFunctionIncremental ::
  MonadIO m =>
  LJ.LogAction IO Diagnostic ->
  C.HandleAllocator ->
  ArchContext arch ->
  IORef.IORef (Discovery.DiscoveryState arch) ->
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  MC.ArchSegmentOff arch ->
  m (ArchRegCFG arch)
discoverFunctionIncremental logAction halloc arch stateRef symMap addr = do
  s <- liftIO $ IORef.readIORef stateRef
  (cfg, s') <- discoverFunctionWith logAction halloc arch s symMap addr
  liftIO $ IORef.writeIORef stateRef s'
  pure cfg

{-
Note [Mark PLT stubs as trusted function entry points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because we perform incremental code discovery (see Note [Incremental code
discovery] in Grease.Macaw.SimulatorState), we usually do not need to mark any
functions as trusted entry points, since most of the time it suffices to
perform code discovery on a function right after invoking it in macaw. However,
this only works if macaw is able to accurately infer what parts of the binary
are actually functions. As noted in
<https://github.com/GaloisInc/macaw/issues/285>, macaw fails to realize that
PLT stubs are functions when they are entered via tail calls, which in turn
prevents us from applying overrides to these sorts of PLT stubs.

Our workaround to this problem is to simply add all of the PLT stubs for a
particular binary to the set of `trustedFunctionEntryPoints` before running
code discovery. This is sufficient to make code discovery go the extra mile to
analyze each PLT stub and mark them as functions, ensuring that tail calls into
these PLT stubs will work as expected.

If macaw improved its global fixed-point analysis, we might not need this
workaround. See <https://github.com/GaloisInc/macaw/issues/298> for more
details.
-}
