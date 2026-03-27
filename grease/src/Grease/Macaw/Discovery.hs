-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Discovery (
  discoverFunction,
) where

import Control.Lens (to, (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Utils.IncComp qualified as IncComp
import Data.Parameterized.Some (Some (Some))
import Grease.Diagnostic (Diagnostic (LoadDiagnostic))
import Grease.Macaw.Arch (ArchContext, ArchRegCFG)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Load (BinMd (binPltStubs, binSymMap))
import Grease.Macaw.Load.Diagnostic qualified as Diag
import Grease.Utility (functionNameFromByteString, tshow)
import Lang.Crucible.FunctionHandle qualified as C
import Lumberjack qualified as LJ
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
  -- | Although this function only explores a single function, it is still
  -- helpful to pass all of the entrypoint addresses (via 'binSymMap') because
  -- they can be used to recover the name of the function when logging. The PLT
  -- stubs (via 'binPltStubs') are marked as trusted function entry points—see
  -- @Note [Mark PLT stubs as trusted function entry points]@ for an
  -- explanation.
  BinMd arch ->
  MC.Memory (MC.RegAddrWidth (MC.ArchReg arch)) ->
  -- | The function address to discover.
  MC.ArchSegmentOff arch ->
  m (ArchRegCFG arch)
discoverFunction logAction halloc arch binMd mem addr = do
  let symMap = binSymMap binMd
  let pltStubs = binPltStubs binMd
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
