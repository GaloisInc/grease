{-# LANGUAGE GADTs #-}

module Screach.LocationExecutionFeature (loggingFeature, frameLocFromSimState, getFrameLoc) where

import Control.Lens ((^.))
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.EvalStmt qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lumberjack qualified as LJ
import Screach.Diagnostic qualified as Diag
import What4.ProgramLoc qualified as WPL

frameLocFromSimState :: C.SimState p sym ext rtp f args -> Maybe WPL.ProgramLoc
frameLocFromSimState simState =
  let frm = simState ^. C.stateTree . C.actFrame . C.gpValue
   in case frm of
        C.MF callFrame -> Just $ C.frameProgramLoc callFrame
        _ -> Nothing

getFrameLoc :: C.ExecState p sym ext rtp -> Maybe WPL.ProgramLoc
getFrameLoc st = do
  C.SomeSimState simState <- C.execStateSimState st
  let frm = simState ^. C.stateTree . C.actFrame . C.gpValue
  case frm of
    C.MF callFrame -> Just $ C.frameProgramLoc callFrame
    _ -> Nothing

loggingFeature :: Diag.ScreachLogAction -> C.GenericExecutionFeature sym
loggingFeature sla =
  C.GenericExecutionFeature
    { C.runGenericExecutionFeature = \st ->
        let stType = case st of
              C.AbortState reason _ -> "abort " ++ show reason
              C.UnwindCallState{} -> "unwind"
              C.CallState{} -> "call state"
              C.SymbolicBranchState{} -> "branch state"
              C.ResultState{} -> "result state"
              C.ReturnState{} -> "return state"
              C.TailCallState{} -> "tail call state"
              C.RunningState{} -> "running state"
              C.ControlTransferState{} -> "ct transfer"
              C.OverrideState{} -> "override state"
              C.BranchMergeState{} -> "branch merge"
              C.InitialState{} -> "initial state"
         in do
              maybe (pure ()) (LJ.writeLog sla . \x -> Diag.ExecutingFrame x stType) (getFrameLoc st)
              pure C.ExecutionFeatureNoChange
    }
