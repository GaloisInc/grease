{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Screach.ShortestDistanceScheduler (sdsePrioritizationFunction, HasDistancesState (..)) where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad (forM)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadIO (liftIO), MonadTrans (lift), StateT, runStateT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Macaw.CFG qualified as MM
import Data.Macaw.X86 qualified as MA
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Some qualified as Some
import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw.Arch qualified as GRS
import Grease.Macaw.SimulatorState qualified as GMSS
import Grease.Scheduler qualified as Sched
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.FunctionHandle qualified as CFH
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.RecordAndReplay qualified as RR
import Lumberjack qualified as LJ
import Numeric.Natural (Natural)
import Safe (tailMay)
import Screach.CallGraph qualified as CG
import Screach.Diagnostic qualified as ScrchDiagnostic
import Screach.Distance qualified as Dist
import Screach.LoadedELF (LoadedELF (..))
import Screach.Panic (panic)
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL

-- | Class for personality types that provide access to a mutable distance cache.
class HasDistancesState p where
  distancesRef :: p -> IORef Dist.DijkstraCaches

getPausedFrameStatementNode :: C.PausedFrame p sym ext rtp f -> Dist.StatementNode
getPausedFrameStatementNode paused =
  case C.resume paused of
    C.ContinueResumption (C.ResolvedJump bId _) ->
      Dist.StatementNode
        { Dist.blockId = Ctx.indexVal (CCC.blockIDIndex bId)
        , Dist.statId = 0
        }
    _ ->
      panic
        "getPausedFrameStatementNode"
        ["hopefully not possible? we should only invoke this function on paused frames from SymBranch"]

getExplorationEntry ::
  C.SimState p sym ext rtp f ('Just args) ->
  C.PausedFrame p sym ext rtp f ->
  IO (Maybe (Some.Some CCC.AnyCFG, Dist.StatementNode))
getExplorationEntry st pf = case st ^. C.stateTree . C.actFrame . C.gpValue of
  C.MF (C.CallFrame{C._frameCFG = cfg}) -> do
    let snode = getPausedFrameStatementNode pf
    pure $ Just (Some.Some $ CCC.AnyCFG cfg, snode)
  _ -> pure Nothing

calledFunctionToAddrLoc :: CG.CallGraph 64 -> MM.MemWord 64 -> [Dist.AddressLocation]
calledFunctionToAddrLoc cg functionToReturnFrom =
  let cs = CG.cgCallsites functionToReturnFrom cg
   in maybe [] (Maybe.mapMaybe callsiteAddrToAddressLocation) cs
 where
  callsiteAddrToAddressLocation :: MM.MemWord 64 -> Maybe Dist.AddressLocation
  callsiteAddrToAddressLocation callsite = do
    containingFunc <- CG.callsiteGetContainingFunction cg callsite
    pure
      Dist.AddressLocation
        { Dist.addrFunctionEntry = CG.addressToLoc containingFunc
        , Dist.addrInsnLoc = Just $ CG.addressToLoc callsite
        }

resolveReturnsFromCG ::
  CG.CallGraph 64 ->
  Dist.FunctionEntry ->
  Dist.Callsite ->
  [Dist.AddressLocation]
resolveReturnsFromCG cg (Dist.FunctionEntry fentry) _ =
  let addr = (CG.locToAddressMaybe fentry :: Maybe (MM.MemWord 64))
   in maybe [] (calledFunctionToAddrLoc cg) addr

isTarget ::
  forall blocks ext ret ctx' ctx'' a.
  -- | The target address
  MM.MemWord 64 ->
  -- | The address of the function containing the target address
  MM.MemWord 64 ->
  CG.CallGraph 64 ->
  -- | We dont need the 'FunctionEntry' address in our current strategy
  -- for checking instructions
  a ->
  WPL.ProgramLoc ->
  Dist.CrucibleStmt blocks ext ret ctx' ctx'' ->
  Maybe Dist.TargetType
isTarget targetAbsAddr targetFunctionAbsAddr cg _ insnAddr st =
  do
    let addr = (CG.locToAddressMaybe insnAddr :: Maybe (MM.MemWord 64))
    (if addr == Just targetAbsAddr then Just Dist.IntraTarget else Nothing)
      <|> ( let callCouldReach = if CG.couldReachTarget cg targetFunctionAbsAddr insnAddr then Just Dist.InterTarget else Nothing
             in case st of
                  Right CCC.CallHandle{} -> callCouldReach
                  Left CCC.TailCall{} -> callCouldReach
                  Left CCC.Return{} ->
                    -- TODO(internal#114) we need to fix this by checking the callgraph in reverse for wether we could reach the return.
                    -- in practice we now use the return stack so this should not be a big deal.
                    -- but we could still waste time by computing the distance for a return on the stack that can never reach target.
                    -- NOTE: In tests we always explore returns even if this function cannot return
                    -- to a function that reaches target, in practice the SDSE setup will use the return stack instead
                    -- derived from the symbolic state.
                    Just Dist.InterTarget
                  _ -> Nothing
          )

-- TODO maybe switch to execstates
-- this is also unbalanced since we dont have as many addrs as crucible stmts so this is a lower bound
getTraceDistance ::
  WI.IsExpr (WI.SymExpr sym) =>
  WI.IsExprBuilder sym =>
  RR.HasRecordState p p sym ext rtp =>
  RR.HasReplayState p p sym ext rtp =>
  C.SimState p sym ext rtp f ('Just args) ->
  IO Natural
getTraceDistance st = do
  mbLen <- RR.recordTraceLength st
  case mbLen of
    Nothing -> pure 0
    Just symLen ->
      case WI.asNat symLen of
        Just len -> pure len
        Nothing ->
          panic
            "getTraceDistance"
            [ "Expected concrete nat, found:"
            , show (WI.printSymExpr (WI.natToIntegerPure symLen))
            ]

callStackFromSimState ::
  C.SimState p sym ext rtp f ('Just args) -> StateT Dist.DijkstraCaches IO Dist.CallStack
callStackFromSimState st = do
  let frms = C.activeFrames $ st ^. C.stateTree
  bids <- forM frms frmToICFGBlock
  pure $ Dist.CallStack $ Maybe.catMaybes bids
 where
  frmToICFGBlock ::
    C.SomeFrame (C.SimFrame sym ext) ->
    StateT Dist.DijkstraCaches IO (Maybe Dist.InterproceduralBlockID)
  frmToICFGBlock (C.SomeFrame (C.MF cf)) = Just <$> Dist.interBlockIDFromFrame cf
  frmToICFGBlock _ = pure Nothing

-- | Run a 'StateT DijkstraCaches IO' computation using an 'IORef' for mutable state.
runWithCachesRef :: IORef Dist.DijkstraCaches -> StateT Dist.DijkstraCaches IO a -> IO a
runWithCachesRef ref action = do
  caches <- IORef.readIORef ref
  (result, caches') <- runStateT action caches
  IORef.writeIORef ref caches'
  pure result

-- TODO(internal#115) we need a counting feature or something to get the history distance
sdsePrioritizationFunction ::
  forall p sym ext rtp.
  ( HasDistancesState p
  , GMSS.HasDiscoveryState p MA.X86_64
  , RR.HasRecordState p p sym ext rtp
  , RR.HasReplayState p p sym ext rtp
  , WI.IsExprBuilder sym
  ) =>
  -- | Target Address
  MM.MemWord 64 ->
  -- | Target containing function
  MM.MemWord 64 ->
  GRS.ArchContext MA.X86_64 ->
  CG.CallGraph 64 ->
  CG.CFGCache 64 ->
  Dist.DistanceConfig ->
  ScrchDiagnostic.ScreachLogAction ->
  GreaseLogAction ->
  LoadedELF 64 ->
  CFH.HandleAllocator ->
  Sched.PrioritizationFunction p sym ext rtp
sdsePrioritizationFunction tgtAddr tgtFunction archCtx cg cache distConfig sla gla lElf halloc frame state =
  let LoadedELF
        { symMap = symMap
        , mem = mem
        } = lElf
      personality = state ^. C.stateContext . C.cruciblePersonality
      cachesRef = distancesRef personality
      discStateRef = GMSS.getDiscoveryStateRef personality
      maybeRes :: MaybeT (Dist.DistanceMonad Dist.DijkstraCaches) Dist.Distance
      maybeRes =
        do
          (cfg, snode) <- MaybeT $ liftIO $ getExplorationEntry state frame
          let rcall (Dist.FunctionEntry fentry) (Dist.Callsite callsite) = CG.resolveCall cg cache discStateRef sla gla mem halloc archCtx symMap fentry callsite
          Dist.CallStack cs <- MaybeT $ lift $ Just <$> callStackFromSimState state
          let poppedCS = Maybe.fromMaybe [] $ tailMay cs
          let
            isT ::
              forall blocks ext' ret ctx' ctx'' a.
              a ->
              WPL.ProgramLoc ->
              Dist.CrucibleStmt blocks ext' ret ctx' ctx'' ->
              Maybe Dist.TargetType
            isT = isTarget tgtAddr tgtFunction cg
          let flatGetCfg (Dist.FunctionEntry floc) =
                ( let addr = (CG.locToAddressMaybe floc :: Maybe (MM.MemWord 64))
                   in case addr of
                        Nothing -> do
                          LJ.writeLog sla (ScrchDiagnostic.AttemptedToReturnViaCallgraphForNonAddressFunction floc)
                          pure Nothing
                        Just a -> CG.getCFG cache discStateRef mem a sla gla halloc archCtx symMap
                )
          let retHandler =
                Dist.ReturnHandler
                  ( Just
                      Dist.ReturnResolutionInfo
                        { Dist.returnResolver = resolveReturnsFromCG cg
                        , Dist.returnCallstack = Dist.CallStack poppedCS
                        , Dist.returnCfgBuidler = flatGetCfg
                        }
                  )
          let x =
                Dist.computeMinDistanceTargetsFromStatmementExt cfg sla snode (Dist.IsTarget isT) retHandler rcall
          MaybeT x
   in do
        prevDist <- fromIntegral <$> getTraceDistance state
        x <-
          runWithCachesRef cachesRef $
            runReaderT (runMaybeT maybeRes) distConfig
        pure $
          (\(Dist.Distance dist) -> Sched.Priority $ dist + prevDist)
            <$> x
