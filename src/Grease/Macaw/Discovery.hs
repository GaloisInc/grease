{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Macaw.Discovery
  ( discoverFunction
  ) where

import Control.Lens ((^.), (.~), to)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Lumberjack as LJ

-- parameterized-utils
import Data.Parameterized.Some

-- what4
import qualified What4.FunctionName as W4
import qualified What4.ProgramLoc as W4

-- crucible
import qualified Lang.Crucible.FunctionHandle as C

-- macaw-base
import qualified Data.Macaw.Architecture.Info as MI
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as Discovery
import qualified Data.Macaw.Memory.ElfLoader as EL
import qualified Data.Macaw.Utils.IncComp as IncComp

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

import Grease.Diagnostic
import Grease.Macaw.Arch
import qualified Grease.Macaw.Load.Diagnostic as Diag
import Grease.Utility

-- | We pass this log function to @macaw@ to wrap discovery events in a custom
-- wrapper that we stream out with the rest of our diagnostics.
logDiscoveryEvent
  :: ( MonadIO m
     , MC.ArchConstraints arch
     , w ~ MC.RegAddrWidth (MC.ArchReg arch) )
  => LJ.LogAction IO Diagnostic
  -> Discovery.AddrSymMap w
  -> Discovery.DiscoveryEvent arch
  -> m ()
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
  Map.Map (MC.ArchSegmentOff arch) W4.FunctionName ->
  -- | The function address to discover.
  MC.ArchSegmentOff arch ->
  m (ArchRegCFG arch)
discoverFunction logAction halloc arch mem symMap pltStubs addr = do
  let archInf = arch ^. archInfo
  -- Mark the PLT stubs as trusted function entry points.
  -- See Note [Mark PLT stubs as trusted function entry points].
  let pltEntryPoints = Discovery.MayReturnFun <$ pltStubs
  let s0 = Discovery.emptyDiscoveryState mem symMap archInf
             & Discovery.trustedFunctionEntryPoints .~ pltEntryPoints
  MI.withArchConstraints archInf $ do
    (_state, Some funInfo) <-
      IncComp.processIncCompLogs (logDiscoveryEvent logAction symMap) $ IncComp.runIncCompM $ do
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunction addr
        let discoveryOpts = Discovery.defaultDiscoveryOptions
        res@(_, Some funInfo) <- IncComp.liftIncComp id $
          Discovery.discoverFunction discoveryOpts addr Discovery.UserRequest s0 []
        IncComp.incCompLog $ Discovery.ReportAnalyzeFunctionDone funInfo
        pure res
    liftIO $ Symbolic.mkFunRegCFG
      (arch ^. archVals . to Symbolic.archFunctions)
      halloc
      (functionNameFromByteString $ Discovery.discoveredFunName funInfo)
      (W4.OtherPos . tshow) -- simply use addresses as source positions for now
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
