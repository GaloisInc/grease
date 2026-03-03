{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Screach.CallGraph (
  CallGraph,
  CallGraphError,
  cgCallees,
  cgCallers,
  resolveCall,
  getCFG,
  empty,
  loadCallGraph,
  CFGCache (..),
  locToAddressMaybe,
  couldReachTarget,
  cgCallsites,
  callsiteGetContainingFunction,
  addressToLoc,
) where

import Control.Exception qualified as X
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord)
import Data.Csv qualified as CSV
import Data.Graph.Inductive qualified as G
import Data.Graph.Inductive qualified as Gr
import Data.Graph.Inductive.NodeMap qualified as GNM
import Data.Graph.Inductive.PatriciaTree qualified as GPT
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as MD
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.Some qualified as Some
import Data.Text qualified as Text
import Data.Vector qualified as Vec
import Data.Word (Word64)
import GHC.IORef qualified as IORef
import GHC.TypeNats (type (<=))
import Grease.Diagnostic (GreaseLogAction)
import Grease.Macaw.Arch qualified as GRS
import Grease.Macaw.Discovery qualified as GMD
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.CFG.Reg qualified as CCR
import Lang.Crucible.CFG.SSAConversion qualified as CCS
import Lang.Crucible.FunctionHandle qualified as CFH
import Lumberjack qualified as LJ
import Screach.Diagnostic (ScreachLogAction)
import Screach.Diagnostic qualified as ScrchDiag
import Screach.Distance.Diagnostic qualified as Diagnositc
import Text.Read qualified as Read
import What4.FunctionName qualified as WFN
import What4.ProgramLoc qualified as WPL

data NodeLabel w
  = NodeCallsite (MM.MemWord w)
  | NodeFunctionEntry (MM.MemWord w)
  deriving (Eq, Ord, Show)

data CallGraph w
  = CallGraph
  { nodeMap :: GNM.NodeMap (NodeLabel w)
  , graph :: GPT.Gr (NodeLabel w) ()
  }

data CallGraphError
  = FailedToParseError !FilePath !String

instance Show CallGraphError where
  show (FailedToParseError fp err) = "Failed to parse call graph from " ++ show fp ++ ": " ++ show err

instance X.Exception CallGraphError

empty :: CallGraph w
empty = CallGraph{nodeMap = G.new, graph = G.empty}

newtype Hex = Hex Word64

parseHex :: BS.ByteString -> CSV.Parser Word64
parseHex s = maybe mempty return $ Read.readMaybe $ BSC.unpack s

instance CSV.FromField Hex where
  parseField s = Hex <$> parseHex s

loadCallGraph :: (1 <= w, MM.MemWidth w) => FilePath -> IO (Either CallGraphError (CallGraph w))
loadCallGraph fp = do
  csvData <- BSL.readFile fp
  let decodeOptions = CSV.defaultDecodeOptions{CSV.decDelimiter = fromIntegral (ord '\t')}
  case CSV.decodeWith decodeOptions CSV.NoHeader csvData of
    Left err -> return $ Left $ FailedToParseError fp err
    Right v ->
      let ((), (nm, g)) = G.run G.empty $ do
            Vec.forM_ v $ \(Hex functionFrom, Hex from, Hex to) -> do
              let funcFrom = MM.memWord functionFrom
              let fromAddr = MM.memWord from
              let toAddr = MM.memWord to
              _ <- G.insMapNodeM (NodeFunctionEntry funcFrom)
              _ <- G.insMapNodeM (NodeCallsite fromAddr)
              _ <- G.insMapNodeM (NodeFunctionEntry toAddr)
              pure ()
            Vec.forM_ v $ \(Hex functionFrom, Hex from, Hex to) -> do
              let funcFrom = MM.memWord functionFrom
              let fromAddr = MM.memWord from
              let toAddr = MM.memWord to
              G.insMapEdgeM (NodeFunctionEntry funcFrom, NodeCallsite fromAddr, ())
              G.insMapEdgeM (NodeCallsite fromAddr, NodeFunctionEntry toAddr, ())
       in return $ Right $ CallGraph{nodeMap = nm, graph = g}

repeatUntilFunction :: forall w. CallGraph w -> Gr.Node -> (Gr.Node -> [Gr.Node]) -> [MM.MemWord w]
repeatUntilFunction CallGraph{nodeMap = _, graph = g} node succs =
  List.concatMap apply (succs node)
 where
  apply :: Gr.Node -> [MM.MemWord w]
  apply nd =
    let label = Gr.lab g nd
     in case label of
          Just (NodeFunctionEntry m) -> [m]
          _ -> List.concatMap apply (succs nd)

cgCallees :: MM.MemWord w -> CallGraph w -> Maybe [MM.MemWord w]
cgCallees caller callgraph =
  let CallGraph{nodeMap = nm, graph = g} = callgraph
   in case G.lookupNode (NodeCallsite caller) nm of
        Just n -> Just $ repeatUntilFunction callgraph n $ Gr.suc g
        Nothing -> Nothing

callsiteGetContainingFunction :: CallGraph 64 -> MM.MemWord 64 -> Maybe (MM.MemWord 64)
callsiteGetContainingFunction CallGraph{graph = g, nodeMap = nm} callsite = do
  called <- G.lookupNode (NodeCallsite callsite) nm
  let preds = G.pre g called
  fstNode <- Maybe.listToMaybe preds
  NodeFunctionEntry addr <- G.lab g fstNode
  pure addr

cgCallsites :: MM.MemWord w -> CallGraph w -> Maybe [MM.MemWord w]
cgCallsites caller callgraph =
  let CallGraph{nodeMap = nm, graph = g} = callgraph
   in case G.lookupNode (NodeFunctionEntry caller) nm of
        Just n ->
          Just
            $ Maybe.mapMaybe
              ( \nd -> do
                  lbl <- Gr.lab g nd
                  case lbl of
                    NodeFunctionEntry _ -> Nothing
                    NodeCallsite addr -> Just addr
              )
            $ Gr.pre g n
        Nothing -> Nothing

cgCallers :: MM.MemWord w -> CallGraph w -> Maybe [MM.MemWord w]
cgCallers caller callgraph =
  let CallGraph{nodeMap = nm, graph = g} = callgraph
   in case G.lookupNode (NodeFunctionEntry caller) nm of
        Just n -> Just $ repeatUntilFunction callgraph n $ Gr.pre g
        Nothing -> Nothing

newtype CFGCache w = CFGCache (IORef.IORef (Map.Map (MM.MemSegmentOff w) (Some.Some CCC.AnyCFG)))

getCFG ::
  ( MM.MemWidth w
  , w ~ MC.RegAddrWidth (MC.ArchReg arch)
  , Symbolic.SymArchConstraints arch
  ) =>
  CFGCache w ->
  MM.Memory w ->
  MM.MemWord w ->
  ScreachLogAction ->
  GreaseLogAction ->
  CFH.HandleAllocator ->
  GRS.ArchContext arch ->
  MD.AddrSymMap (MC.ArchAddrWidth arch) ->
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  IO (Maybe (Some.Some CCC.AnyCFG))
getCFG (CFGCache cfgCache) mem addr sla gla halloc archCtx symMap pltStubs =
  case MM.resolveAbsoluteAddr mem addr of
    Just segOff -> do
      cache <- IORef.readIORef cfgCache
      case Map.lookup segOff cache of
        Just cfg -> pure $ Just cfg
        Nothing -> do
          CCR.SomeCFG cfgReg <- GMD.discoverFunction gla halloc archCtx mem symMap pltStubs segOff
          LJ.writeLog sla (ScrchDiag.DistanceDiagnostic $ Diagnositc.DiscoveredCFG addr (CCR.SomeCFG cfgReg))
          let cfg' = case CCS.toSSA cfgReg of
                CCC.SomeCFG cfg -> Some.Some $ CCC.AnyCFG cfg
          IORef.atomicModifyIORef' cfgCache (\c -> (Map.insert segOff cfg' c, ()))
          pure $ Just cfg'
    Nothing -> do
      LJ.writeLog sla (ScrchDiag.DistanceDiagnostic $ Diagnositc.FailedToResolveAddr addr)
      pure Nothing

-- TODO(internal#104): this is very hacky, we have to lift addresses back to locations since resolvecall +
-- the callgraph dont bind the address width together in the type so we cant compare as 'MM.MemWord's
addressToLoc :: MM.MemWord w -> WPL.ProgramLoc
addressToLoc addr = WPL.mkProgramLoc WFN.startFunctionName (WPL.OtherPos $ Text.pack $ show addr)

locToAddressMaybe :: (MM.MemWidth w) => WPL.ProgramLoc -> Maybe (MM.MemWord w)
locToAddressMaybe pLoc =
  case WPL.plSourceLoc pLoc of
    WPL.OtherPos addrText ->
      case Read.readMaybe $ Text.unpack addrText of
        Just addr -> Just $ MM.memWord addr
        Nothing -> Nothing
    _ -> Nothing

couldReachTarget ::
  (MM.MemWidth w) =>
  CallGraph w ->
  MM.MemWord w ->
  WPL.ProgramLoc ->
  Bool
couldReachTarget cg targetAddr callsite =
  Maybe.fromMaybe
    False
    ( do
        checkAddr <- locToAddressMaybe callsite
        let cs = NodeCallsite checkAddr
        let nm = nodeMap cg
        tgt <- GNM.lookupNode (NodeFunctionEntry targetAddr) nm
        let reachable = Gr.rdfs [tgt] (graph cg)
        let reachedNodes = Maybe.mapMaybe (Gr.lab $ graph cg) reachable
        pure $ elem cs reachedNodes
    )

resolveCall ::
  forall w arch.
  ( MM.MemWidth w
  , w ~ MC.RegAddrWidth (MC.ArchReg arch)
  , Symbolic.SymArchConstraints arch
  ) =>
  CallGraph w ->
  CFGCache w ->
  ScreachLogAction ->
  GreaseLogAction ->
  MM.Memory w ->
  CFH.HandleAllocator ->
  GRS.ArchContext arch ->
  MD.AddrSymMap (MC.ArchAddrWidth arch) ->
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  WPL.ProgramLoc ->
  WPL.ProgramLoc ->
  IO [Some.Some CCC.AnyCFG]
resolveCall callGraph cfgCaches sla gla mem hLoc archContext symMap pltStubs callerLoc callsiteLoc = do
  let mbCaller = locToAddressMaybe callerLoc
  let mbCallsite = locToAddressMaybe callsiteLoc
  case (mbCaller, mbCallsite) of
    (Just caller, Just callsite) ->
      case cgCallees callsite callGraph of
        Just callees -> do
          mCfgs <- mapM getCFGFromAddr callees
          pure $ Maybe.catMaybes mCfgs
        Nothing -> do
          LJ.writeLog
            sla
            (ScrchDiag.DistanceDiagnostic $ Diagnositc.NoCalleesForCallsite callsiteLoc callerLoc)
          case cgCallees caller callGraph of
            Just callees -> do
              mCfgs <- mapM getCFGFromAddr callees
              let result = Maybe.catMaybes mCfgs
              pure result
            Nothing -> do
              LJ.writeLog sla (ScrchDiag.DistanceDiagnostic $ Diagnositc.NoCalleesForFunction callerLoc)
              pure []
    _ -> do
      LJ.writeLog
        sla
        (ScrchDiag.DistanceDiagnostic $ Diagnositc.NoAddressesForCallsite callerLoc callsiteLoc)
      pure []
 where
  getCFGFromAddr :: MM.MemWord w -> IO (Maybe (Some.Some CCC.AnyCFG))
  getCFGFromAddr addr = getCFG cfgCaches mem addr sla gla hLoc archContext symMap pltStubs
