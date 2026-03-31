{-# LANGUAGE ImplicitParams #-}

module Grease.ExecutionFeatures (
  boundedExecFeats,
  pathSatFeat,
  greaseExecFeats,
) where

import Control.Monad (when)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Time.Clock (secondsToNominalDiffTime)
import Grease.BranchTracer (greaseBranchTracerFeature)
import Grease.Diagnostic (GreaseLogAction)
import Grease.Options qualified as GO
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.BoundedExec qualified as C
import Lang.Crucible.Simulator.BoundedRecursion qualified as C
import Lang.Crucible.Simulator.PathSatisfiability qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Utils.Seconds qualified as C
import Prettyprinter qualified as PP
import What4.Config qualified as W4C
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

-- | Execution features related to bounding and timeouts
boundedExecFeats ::
  GO.BoundsOpts ->
  IO [CS.GenericExecutionFeature sym]
boundedExecFeats boundsOpts = do
  let symexTimeout = realToFrac (C.secondsToInt (GO.simTimeout boundsOpts))
  let GO.LoopBound loopBound = GO.simLoopBound boundsOpts
  let boundObligation = GO.simLoopBoundObligation boundsOpts
  boundExecFeat <- C.boundedExecFeature (\_ -> pure (Just loopBound)) boundObligation
  boundRecFeat <- C.boundedRecursionFeature (\_ -> pure (Just loopBound)) boundObligation
  timeoutFeat <- CS.timeoutFeature (secondsToNominalDiffTime symexTimeout)
  pure [boundExecFeat, boundRecFeat, timeoutFeat]

-- | Path satisfiability feature, optionally enables 'C.assertThenAssumeConfigOption'
pathSatFeat ::
  ( CB.IsSymBackend sym bak
  , bak ~ C.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st (WE.Flags fm)
  , WPO.OnlineSolver solver
  ) =>
  Bool ->  -- ^ Enable assert-then-assume
  bak ->
  IO (CS.GenericExecutionFeature sym)
pathSatFeat enableAssertThenAssume bak = do
  let sym = CB.backendGetSym bak
  pathSat <- C.pathSatisfiabilityFeature sym (C.considerSatisfiability bak)
  when enableAssertThenAssume $ do
    let cfg = WI.getConfiguration sym
    assertThenAssume <- W4C.getOptionSetting CB.assertThenAssumeConfigOption cfg
    -- This can technically return warnings/errors, but seems unlikely in this
    -- case...
    warns <- W4C.setOpt assertThenAssume True
    case warns of
      [] -> pure ()
      _ -> panic "configurePathSatFeature" (List.map show warns)
  pure pathSat

-- | Debugger, path satisfiability, and branch tracing features
greaseExecFeats ::
  ( CB.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , sym ~ WE.ExprBuilder scope st (WE.Flags fm)
  , ?parserHooks :: CSyn.ParserHooks ext
  , PP.Pretty cExt
  , PP.Pretty (Dbg.ResponseExt cExt)
  , bak ~ C.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st (WE.Flags fm)
  , WPO.OnlineSolver solver
  , Dbg.HasContext p cExt sym ext ret
  ) =>
  GreaseLogAction ->
  bak ->
  Bool ->  -- ^ Enable path satisfiability checking
  Bool ->  -- ^ Enable assert-then-assume
  -- | Debugger configuration, if desired
  Maybe (Dbg.ExtImpl cExt p sym ext ret) ->
  IO [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)]
greaseExecFeats la bak enablePathSat enableAssertThenAssume dbgOpts = do
  let execFeats_ =
        case dbgOpts of
          Just extImpl -> [Dbg.debugger extImpl]
          Nothing -> []
  mbPathSat <- if enablePathSat
                 then Just <$> pathSatFeat enableAssertThenAssume bak
                 else pure Nothing
  let execFeats =
        Maybe.maybeToList (fmap CS.genericToExecutionFeature mbPathSat)
          ++ greaseBranchTracerFeature la
          : execFeats_
  pure execFeats
