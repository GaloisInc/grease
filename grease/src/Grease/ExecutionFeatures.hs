{-# LANGUAGE ImplicitParams #-}

module Grease.ExecutionFeatures (
  boundedExecFeats,
  pathSatFeat,
  greaseExecFeats,
) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Time.Clock (secondsToNominalDiffTime)
import Grease.BranchTracer (greaseBranchTracerFeature)
import Grease.Diagnostic (GreaseLogAction)
import Grease.Options qualified as GO
import Grease.Panic (panic)
import Grease.Pretty (prettyPtrFnMap)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.BoundedExec qualified as C
import Lang.Crucible.Simulator.BoundedRecursion qualified as C
import Lang.Crucible.Simulator.PathSatisfiability qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Types qualified as CT
import Lang.Crucible.Utils.Seconds qualified as C
import Prettyprinter qualified as PP
import What4.Config qualified as W4C
import What4.Expr qualified as W4
import What4.Interface qualified as W4I
import What4.Protocol.Online qualified as WPO

-- | Execution features related to bounding and timeouts
boundedExecFeats ::
  GO.BoundsOpts ->
  IO [C.GenericExecutionFeature sym]
boundedExecFeats boundsOpts = do
  let symexTimeout = realToFrac (C.secondsToInt (GO.simTimeout boundsOpts))
  let GO.LoopBound loopBound = GO.simLoopBound boundsOpts
  boundExecFeat <- C.boundedExecFeature (\_ -> pure (Just loopBound)) True
  boundRecFeat <- C.boundedRecursionFeature (\_ -> pure (Just loopBound)) True
  timeoutFeat <- C.timeoutFeature (secondsToNominalDiffTime symexTimeout)
  pure [boundExecFeat, boundRecFeat, timeoutFeat]

-- | Path satisfiability feature, plus enables 'C.assertThenAssumeConfigOption'
pathSatFeat ::
  ( C.IsSymBackend sym bak
  , bak ~ C.OnlineBackend solver scope st fs
  , sym ~ W4.ExprBuilder scope st (W4.Flags fm)
  , WPO.OnlineSolver solver
  ) =>
  bak ->
  IO (C.GenericExecutionFeature sym)
pathSatFeat bak = do
  let sym = C.backendGetSym bak
  pathSat <- C.pathSatisfiabilityFeature sym (C.considerSatisfiability bak)
  let cfg = W4I.getConfiguration sym
  assertThenAssume <- W4C.getOptionSetting C.assertThenAssumeConfigOption cfg
  -- This can technically return warnings/errors, but seems unlikely in this
  -- case...
  warns <- W4C.setOpt assertThenAssume True
  case warns of
    [] -> pure ()
    _ -> panic "configurePathSatFeature" (List.map show warns)
  pure pathSat

greaseExecFeats ::
  ( C.IsSymBackend sym bak
  , C.IsSyntaxExtension ext
  , sym ~ W4.ExprBuilder scope st (W4.Flags fm)
  , ?parserHooks :: CSyn.ParserHooks ext
  , PP.Pretty cExt
  , PP.Pretty (Dbg.ResponseExt cExt)
  , bak ~ C.OnlineBackend solver scope st fs
  , sym ~ W4.ExprBuilder scope st (W4.Flags fm)
  , WPO.OnlineSolver solver
  ) =>
  GreaseLogAction ->
  bak ->
  -- | Debugger configuration, if desired
  Maybe (Dbg.CommandExt cExt, Dbg.ExtImpl cExt p sym ext ret, CT.TypeRepr ret) ->
  GO.BoundsOpts ->
  IO [C.ExecutionFeature p sym ext (C.RegEntry sym ret)]
greaseExecFeats la bak dbgOpts boundsOpts = do
  debuggerFeat <-
    case dbgOpts of
      Just (cmdExt, extImpl, retTy) -> do
        dbgInputs <- Dbg.defaultDebuggerInputs cmdExt
        Just
          <$> Dbg.debugger
            cmdExt
            extImpl
            prettyPtrFnMap
            dbgInputs
            Dbg.defaultDebuggerOutputs
            retTy
      Nothing -> pure Nothing
  let execFeats_ = Maybe.catMaybes [debuggerFeat]
  pathSat <- pathSatFeat bak
  boundsFeats <- boundedExecFeats boundsOpts
  let execFeats =
        C.genericToExecutionFeature pathSat
          : greaseBranchTracerFeature la
          : execFeats_
          List.++ List.map C.genericToExecutionFeature boundsFeats
  pure execFeats
