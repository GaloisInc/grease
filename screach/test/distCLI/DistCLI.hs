{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module DistCLI (cli, DisttestConfig (..), runDistTest) where

import Control.Applicative ((<|>))
import Control.Monad qualified
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State.Lazy (evalStateT)
import Data.ByteString qualified as BS
import Data.IORef qualified as IORef
import Data.List qualified as List
import Data.Macaw.BinaryLoader qualified as Loader
import Data.Macaw.CFG qualified as MM
import Data.Macaw.Symbolic.Syntax qualified as MSS
import Data.Macaw.X86 qualified as MX86
import Data.Macaw.X86.Symbolic.Syntax qualified as MX86Syn
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some qualified as Some
import Data.Proxy
import Grease.Diagnostic qualified as GD
import Grease.Macaw.Arch.X86 (x86Ctx)
import Grease.Macaw.Discovery qualified as GMD
import Grease.Macaw.Load qualified as GL
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.CFG.Extension qualified as CCE
import Lang.Crucible.CFG.Reg qualified as CCR
import Lang.Crucible.CFG.SSAConversion qualified as CCS
import Lang.Crucible.FunctionHandle qualified as CFH
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lumberjack qualified as LJ
import Options.Applicative qualified as Opts
import Screach qualified as Scrch
import Screach.CallGraph qualified as CG
import Screach.Cli qualified as ScrCLI
import Screach.Config qualified as Conf
import Screach.Diagnostic qualified as SD
import Screach.Diagnostic qualified as ScrchDiag
import Screach.Distance qualified as Dist
import Screach.Distance.Diagnostic qualified as Diagnostic
import Screach.LoadedELF qualified as Scrch
import Screach.Panic (panic)
import What4.ProgramLoc qualified as WPL

cli :: Opts.ParserInfo DisttestConfig
cli =
  Opts.info (conf Opts.<**> Opts.helper) $
    mconcat
      [ Opts.fullDesc
      , Opts.header "screach distance test - test utility for shortest path heuristic"
      ]

data DisttestConfig = DisttestConfig {progConfig :: Conf.ProgramConfig, callgraph :: FilePath}

conf :: Opts.Parser DisttestConfig
conf =
  DisttestConfig
    <$> ScrCLI.programConfig
    <*> Opts.strOption
      ( Opts.long "callgraph"
          <> Opts.metavar "PATH"
          <> Opts.help "The path to the callgraph csv"
      )

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

-- | Convert a register-based CFG ('CCR.SomeCFG') to an SSA-based CFG
-- ('CCC.SomeCFG').
toSsaSomeCfg ::
  (CCE.IsSyntaxExtension ext) =>
  CCR.SomeCFG ext init ret ->
  CCC.SomeCFG ext init ret
toSsaSomeCfg (CCR.SomeCFG cfg) = CCS.toSSA cfg

type DistTask = (CCC.Some CCC.AnyCFG, Dist.StatementNode)

data TestTask = TestTask {longer :: DistTask, shorter :: DistTask}

getTaskForFunc ::
  BS.ByteString ->
  Map.Map (MM.MemSegmentOff 64) BS.ByteString ->
  (MM.MemSegmentOff 64 -> IO (CCC.Some CCC.AnyCFG)) ->
  IO DistTask
getTaskForFunc tgtName symMap buildCFG = do
  (targetAddr, _) <- case List.find (\(_, v) -> v == tgtName) $ Map.toList symMap of
    Nothing -> fail $ "Could not find target function: " ++ show tgtName
    Just x -> pure x
  (CCC.Some (CCC.AnyCFG cfg)) <- buildCFG targetAddr
  let ent = Dist.cfgEntrySnode cfg
  pure (CCC.Some (CCC.AnyCFG cfg), ent)

getTestTask ::
  Map.Map (MM.MemSegmentOff 64) BS.ByteString ->
  (MM.MemSegmentOff 64 -> IO (CCC.Some CCC.AnyCFG)) ->
  IO TestTask
getTestTask symMap buildCFG = do
  let getTsk nm = getTaskForFunc nm symMap buildCFG
  longTask <- getTsk "long_path"
  shortTask <- getTsk "short_path"
  pure (TestTask{longer = longTask, shorter = shortTask})

runTestTask :: TestTask -> (DistTask -> IO (Maybe Dist.Distance)) -> IO ()
runTestTask tsk runner =
  let getOrFail err x = case x of
        Just p -> pure p
        Nothing -> fail err
   in do
        shortTsk <- getOrFail "should be able to reach short" =<< runner (shorter tsk)
        longTsk <- getOrFail "should be able to reach long" =<< runner (longer tsk)
        Control.Monad.when (shortTsk >= longTsk) $
          fail $
            ("Shorter task was " ++ show shortTsk ++ " and longer task was " ++ show longTsk)

locToAddress :: WPL.ProgramLoc -> MM.MemWord 64
locToAddress loc =
  Maybe.fromMaybe
    (panic "Dist tester does not support distance through overrides" [])
    (CG.locToAddressMaybe loc)

-- | Runs a singular distance test. Distance tests test the 'Screach.Distance' module
-- by comparing the distance from a function named "long_path" to "target", and a function named "short_path" to
-- "target". Both functions are expected to be able to reach target with the distance from "long_path" being greater than "short_path".
-- This setup allows testing distances in isolation from the other functionality of Screach.
runDistTest :: DisttestConfig -> IO ()
runDistTest DisttestConfig{callgraph = callgraph, progConfig = pconf} =
  do
    let ?ptrWidth = NatRepr.knownNat @64
    let ?memOpts = CLM.defaultMemOptions
    let ?parserHooks = MSS.machineCodeParserHooks (Proxy @MX86.X86_64) MX86Syn.x86ParserHooks
    let sla :: SD.ScreachLogAction
        sla = LJ.LogAction $ \diag ->
          LJ.writeLog (Scrch.defaultLogAction pconf) diag
    mbCg <- (CG.loadCallGraph callgraph :: IO (Either CG.CallGraphError (CG.CallGraph 64)))
    cg <- case mbCg of
      Left err -> fail $ "CG error: " ++ show err
      Right x -> pure x
    let gla :: GD.GreaseLogAction
        gla = LJ.LogAction $ \diag ->
          LJ.writeLog sla (SD.GreaseDiagnostic diag)
    halloc <- CFH.newHandleAllocator
    archCtx <-
      ( do
          let retAddr = Nothing -- TODO
          let slots = Conf.stackArgumentSlots pconf
          x86Ctx halloc retAddr slots
      )
    cfgCache <- IORef.newIORef Map.empty
    lElf <- Scrch.loadElfFromConfig pconf sla gla archCtx
    let loadedProg = Scrch.loadedProgram lElf
    let binMd = GL.progBinMd loadedProg
    let mem = Loader.memoryImage (GL.progLoadedBinary loadedProg)
    let symMap = GL.binSymMap binMd
    let pltStubs = GL.binPltStubs binMd
    let discoveryState0 = GMD.mkInitialDiscoveryState archCtx mem symMap pltStubs
    discStateRef <- IORef.newIORef discoveryState0
    let discover addr = do
          cfg <- GMD.discoverFunctionIncremental gla halloc archCtx discStateRef symMap addr
          maybe
            (pure ())
            (\raddr -> LJ.writeLog sla (ScrchDiag.DistanceDiagnostic $ Diagnostic.DiscoveredCFG raddr cfg))
            (MM.segoffAsAbsoluteAddr addr)
          pure cfg
    -- TODO this results in duplicate caches we should commit to the monad or IORef
    let flatGetCfg (Dist.FunctionEntry floc) =
          let addr = (locToAddress floc :: MM.MemWord 64)
           in CG.getCFG (CG.CFGCache cfgCache) discStateRef binMd mem addr sla gla halloc archCtx
    let rcall (Dist.FunctionEntry fentry) (Dist.Callsite callsite) =
          CG.resolveCall cg (CG.CFGCache cfgCache) discStateRef sla gla binMd mem halloc archCtx fentry callsite
    -- We want to find the target entry instruction

    let resolveReturnsFromCG :: Dist.FunctionEntry -> Dist.Callsite -> [Dist.AddressLocation]
        resolveReturnsFromCG (Dist.FunctionEntry fentry) _ =
          let addr = (locToAddress fentry :: MM.MemWord 64)
           in calledFunctionToAddrLoc cg addr
        isTarget ::
          forall blocks ext ret ctx' ctx'' a.
          a -> WPL.ProgramLoc -> Dist.CrucibleStmt blocks ext ret ctx' ctx'' -> Maybe Dist.TargetType
        isTarget _ insnAddr st =
          ( let res =
                  ( do
                      let addr = (locToAddress insnAddr :: MM.MemWord 64)
                      (targetAddr, _) <- List.find (\(_, v) -> v == "target") $ Map.toList symMap
                      targetAbsAddr <- MM.segoffAsAbsoluteAddr targetAddr
                      (if addr == targetAbsAddr then Just Dist.IntraTarget else Nothing)
                        <|> ( let callCouldReach = if CG.couldReachTarget cg targetAbsAddr insnAddr then Just Dist.InterTarget else Nothing
                               in case st of
                                    Right CCC.CallHandle{} -> callCouldReach
                                    Left CCC.TailCall{} -> callCouldReach
                                    Left CCC.Return{} ->
                                      -- NOTE: In tests we always explore returns even if this function cannot return
                                      -- to a function that reaches target, in practice the SDSE setup will use the return stack instead
                                      -- derived from the symbolic state.
                                      Just Dist.InterTarget
                                    _ -> Nothing
                            )
                  )
             in res
          )
    tstTask <-
      getTestTask
        symMap
        ( \x -> do
            discovered <- discover x
            CCC.SomeCFG newCFG <- pure $ toSsaSomeCfg discovered
            pure $ Some.Some (CCC.AnyCFG newCFG)
        )
    let retDef = Dist.DefaultReturnDist 10
    let distConf = Dist.DistanceConfig{Dist.defaultRetDist = retDef, Dist.isInfiniteDistLoc = const False}
    let nullReturnHandler =
          Dist.ReturnHandler
            ( Just
                Dist.ReturnResolutionInfo
                  { Dist.returnResolver = resolveReturnsFromCG
                  , Dist.returnCallstack = Dist.CallStack []
                  , Dist.returnCfgBuidler = flatGetCfg
                  }
            )
    runTestTask
      tstTask
      ( \(cfg, snode) ->
          evalStateT
            ( runReaderT
                ( Dist.computeMinDistanceTargetsFromStatmementExt
                    cfg
                    sla
                    snode
                    (Dist.IsTarget isTarget)
                    nullReturnHandler
                    rcall
                )
                distConf
            )
            Dist.emptyDijkstraCaches
      )
