{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Screach.Distance (
  FunctionEntry (..),
  Callsite (..),
  IsTarget (..),
  TargetType (..),
  CrucibleStmt,
  emptyDijkstraCaches,
  StatementNode (..),
  cfgEntrySnode,
  Distance (..),
  DijkstraCaches,
  computeMinDistanceTargetsFromStatmementExt,
  ReturnResolutionInfo (..),
  ReturnHandler (..),
  AddressLocation (..),
  allStatements,
  CallStack (..),
  InterproceduralBlockID,
  interBlockIDFromFrame,
  DefaultReturnDist (..),
  DistanceConfig (..),
  DistanceMonad,
) where

import Control.Lens (Zoom (zoom), makeLenses, over)
import Control.Lens.Combinators (Lens')
import Control.Lens.Getter (view)
import Control.Lens.Setter ((%=))
import Control.Monad (forM, forM_)
import Control.Monad qualified as Monad
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Lazy (MonadIO (liftIO), MonadState (get, put), StateT, modify, runStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (mapReaderT)
import Data.List qualified as List
import Data.Macaw.Memory qualified as MM
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.OrdPSQ qualified as OrdPSQ
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Nonce qualified as Nonce
import Data.Parameterized.Some qualified as Some
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Parameterized.TraversableFC.WithIndex qualified as TFC
import Data.Word (Word64)
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.FunctionHandle qualified as CFH
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import Safe (minimumMay)
import Screach.Diagnostic (ScreachLogAction)
import Screach.Diagnostic qualified as ScrchDiagnostic
import Screach.Distance.Diagnostic qualified as Diagnostic
import Screach.Panic qualified as Scrch
import What4.ProgramLoc qualified as WPL

newtype FunctionEntry = FunctionEntry WPL.ProgramLoc deriving (Eq, Show, Ord)
newtype Callsite = Callsite WPL.ProgramLoc deriving (Ord, Eq, Show)

newtype Distance = Distance {distNumStatements :: Int} deriving (Eq, Show, Ord)

offsetDistance :: Distance -> Int -> Distance
offsetDistance (Distance x) num = Distance $ x + num

addDistance :: Distance -> Distance -> Distance
addDistance (Distance x) (Distance y) = Distance $ x + y

newtype CfgId = CfgId Word64 deriving (Eq, Show, Ord)

cfgIDFromHandle :: CFH.FnHandle init ret -> CfgId
cfgIDFromHandle = CfgId . Nonce.indexValue . CFH.handleID

data InterproceduralBlockID = InterproceduralBlockID
  { cfgId :: CfgId
  , statementNode :: StatementNode
  }
  deriving (Eq, Show, Ord)

-- | State holding already computed function return distances
-- assumes non overlapping function program locs which should be true
type DistCache = Map.Map InterproceduralBlockID (Maybe Distance)

type ResolveCall = FunctionEntry -> Callsite -> IO [Some.Some CCC.AnyCFG]

data AddressLocation = AddressLocation {addrFunctionEntry :: WPL.ProgramLoc, addrInsnLoc :: Maybe WPL.ProgramLoc}
  deriving (Show)

-- | Function we use to resolve a non searched return
type ResolveReturn = FunctionEntry -> Callsite -> [AddressLocation]

newtype CallStack = CallStack [InterproceduralBlockID] deriving (Show)

newtype CFGCache = CFGCache (Map.Map CfgId (Some.Some CCC.AnyCFG))

insertCFGIntoCache :: CfgId -> Some.Some CCC.AnyCFG -> CFGCache -> CFGCache
insertCFGIntoCache cid packedCfg (CFGCache mp) = CFGCache $ Map.insert cid packedCfg mp

type GetCFG = FunctionEntry -> IO (Maybe (Some.Some CCC.AnyCFG))

data ExplorationTask = ExplorationTask
  {expCfg :: Some.Some CCC.AnyCFG, expCallStack :: CallStack, expStatmentNode :: StatementNode}

instance Show ExplorationTask where
  show ExplorationTask{expCfg = _, expCallStack = cs, expStatmentNode = snode} = "ExplorationTask" ++ "{" ++ show cs ++ "," ++ show snode ++ "}"

type IOState s a = StateT s IO a

-- | The default assumed return distance for a call that (for whatever reason) does not have a path to a return.
-- Thi can happen when a function has missing callsites in the callgraph. This distance is used when computing
--  the distance between a source and a target with an intervening call.
newtype DefaultReturnDist = DefaultReturnDist Int

-- TODO(internal#104) we should instead parametrize the distance module over a width w for addresses
-- such that isInfiniteDistLoc can be an address set
data DistanceConfig = DistanceConfig {defaultRetDist :: DefaultReturnDist, isInfiniteDistLoc :: WPL.ProgramLoc -> Bool}

type DistanceMonad s = ReaderT DistanceConfig (StateT s IO)

getCFG :: FunctionEntry -> GetCFG -> IOState CFGCache (Maybe (Some.Some CCC.AnyCFG))
getCFG ent cfgGetter =
  do
    mbCfg <- lift $ cfgGetter ent
    case mbCfg of
      Just pked@(Some.Some (CCC.AnyCFG cfg)) ->
        let cid = cfgIDFromHandle $ CCC.cfgHandle cfg
         in modify $ insertCFGIntoCache cid pked
      Nothing -> pure ()
    pure mbCfg

getCFGFromID :: CfgId -> IOState CFGCache (Maybe (Some.Some CCC.AnyCFG))
getCFGFromID targetId = do
  CFGCache mp <- get
  pure $ Map.lookup targetId mp

firstStatementNodeOfBlock :: CCC.Block ext blocks ret ctx -> StatementNode
firstStatementNodeOfBlock block = StatementNode{blockId = (Ctx.indexVal . CCC.blockIDIndex . CCC.blockID) block, statId = 0}

cfgEntrySnode :: CCC.CFG x blocks init ret -> StatementNode
cfgEntrySnode cfg = firstStatementNodeOfBlock $ CCC.getBlock (CCC.cfgEntryBlockID cfg) (CCC.cfgBlockMap cfg)

allStatementsOfBlock ::
  forall m actext actblocks actinit actret args.
  (Monoid m) =>
  CCC.BlockID actblocks args ->
  CCC.CFG actext actblocks actinit actret ->
  ( forall blocks ext ret ctx ctx' ctx''.
    StatementNode ->
    CCC.Block ext blocks ret ctx'' ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    m
  ) ->
  m
allStatementsOfBlock bid cfg f =
  let x = CCC.getBlock bid $ CCC.cfgBlockMap cfg
      stats = view CCC.blockStmts x
   in applyToStats x (firstStatementNodeOfBlock x) stats
 where
  applyToStats ::
    CCC.Block ext blocks ret ctx'' -> StatementNode -> CCC.StmtSeq ext blocks ret ctx -> m
  applyToStats blk snode (CCC.TermStmt loc term) = f snode blk loc (Left term)
  applyToStats blk snode (CCC.ConsStmt loc stmt rst) =
    f snode blk loc (Right stmt)
      <> applyToStats blk (StatementNode{blockId = blockId snode, statId = statId snode + 1}) rst
allStatements ::
  forall m.
  (Monoid m) =>
  Some.Some CCC.AnyCFG ->
  ( forall blocks ext ret ctx ctx' ctx''.
    StatementNode ->
    CCC.Block ext blocks ret ctx'' ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    m
  ) ->
  m
allStatements (Some.Some (CCC.AnyCFG cfg)) f =
  let blockMap = CCC.cfgBlockMap cfg
   in TFC.ifoldMapFC
        ( \i _ ->
            allStatementsOfBlock (CCC.BlockID i) cfg f
        )
        blockMap

allCallNodes :: Some.Some CCC.AnyCFG -> [StatementNode]
allCallNodes cfg = allStatements cfg $ \snode _ _ stat ->
  case stat of
    Right CCC.CallHandle{} -> [snode]
    _ -> []

findInCFG :: Maybe WPL.ProgramLoc -> [StatementNode] -> Some.Some CCC.AnyCFG -> [StatementNode]
findInCFG Nothing defaultNodes _ = defaultNodes
findInCFG (Just loc) _ cfg = allStatements cfg $ \snode _ currLoc _ ->
  -- TODO(internal#104) fix this to compare addresses
  ([snode | WPL.plSourceLoc currLoc == WPL.plSourceLoc loc])

findCallingSites ::
  AddressLocation -> GetCFG -> IOState CFGCache [(Some.Some CCC.AnyCFG, StatementNode)]
findCallingSites loc cfgGetter =
  do
    mbLst <-
      runMaybeT
        ( do
            cfg <- MaybeT $ getCFG (FunctionEntry $ addrFunctionEntry loc) cfgGetter
            let lst = findInCFG (addrInsnLoc loc) (allCallNodes cfg) cfg
            pure $ (cfg,) <$> lst
        )
    pure (Maybe.fromMaybe [] mbLst)

cfgFunctionEntry :: Some.Some CCC.AnyCFG -> Maybe FunctionEntry
cfgFunctionEntry (Some.Some (CCC.AnyCFG cfg)) =
  -- TODO(internal#107): how to recognize that we resolved an override instead of a "real func"
  Just $ FunctionEntry $ CCC.blockLoc $ CCC.getBlock (CCC.cfgEntryBlockID cfg) (CCC.cfgBlockMap cfg)

collectReturnSite ::
  Some.Some CCC.AnyCFG ->
  CallStack ->
  StatementNode ->
  ResolveReturn ->
  GetCFG ->
  IOState CFGCache [ExplorationTask]
collectReturnSite cfg callstack snode rReturn cfgBuilder =
  withStatement snode cfg $ \_ loc stat ->
    case stat of
      -- we should only return to callsites
      Right (CCC.CallHandle{}) ->
        pure [ExplorationTask{expCfg = cfg, expCallStack = callstack, expStatmentNode = nextSnode snode}]
      Left (CCC.TailCall{}) ->
        maybe
          (pure [])
          (\ent -> applyReturn callstack ent (Callsite loc) rReturn cfgBuilder)
          (cfgFunctionEntry cfg)
      _ -> pure []

findReturnSites ::
  AddressLocation -> GetCFG -> CallStack -> ResolveReturn -> IOState CFGCache [ExplorationTask]
findReturnSites aloc cfgGetter callstack rReturn =
  do
    lst <- findCallingSites aloc cfgGetter
    List.concat <$> forM lst (\(cfg, snode) -> collectReturnSite cfg callstack snode rReturn cfgGetter)

type CrucibleStmt blocks ext ret ctx' ctx'' =
  Either (CCC.TermStmt blocks ret ctx') (CCC.Stmt ext ctx' ctx'')

-- TODO(internal#105) dont do this, we shouldn't be iterating repeatedly through statements to get a statement index
withStatementFromBlock ::
  forall ext blocks ret ctx r.
  Int ->
  CCC.Block ext blocks ret ctx ->
  (forall ctx' ctx''. WPL.ProgramLoc -> CrucibleStmt blocks ext ret ctx' ctx'' -> r) ->
  r
withStatementFromBlock idx blk cont =
  let stats = view CCC.blockStmts blk
   in getInd idx stats
 where
  getInd :: Int -> CCC.StmtSeq ext blocks ret someCtx -> r
  getInd 0 (CCC.ConsStmt loc stmt _) = cont loc $ Right stmt
  getInd 0 (CCC.TermStmt loc term) = cont loc $ Left term
  getInd i (CCC.ConsStmt _ _ rst) = getInd (i - 1) rst
  getInd _ ((CCC.TermStmt _ _)) = Scrch.panic "unreachable! out of bounds stat id" []

withStatement ::
  StatementNode ->
  Some.Some CCC.AnyCFG ->
  ( forall bctx tp ext blocks ret ctx ctx'.
    CCC.Block ext blocks tp bctx ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    r
  ) ->
  r
withStatement sNode (Some.Some (CCC.AnyCFG cfg)) cont =
  let bbMap = CCC.cfgBlockMap cfg
      bidRaw = blockId sNode
      cid = Ctx.intIndex bidRaw (Ctx.size bbMap)
   in case cid of
        Just (Some.Some ind) ->
          let bid = CCC.BlockID ind
              blk = CCC.getBlock bid bbMap
           in withStatementFromBlock (statId sNode) blk (cont blk)
        _ -> Scrch.panic "unreachable! out of bounds block id" []
applyReturn ::
  CallStack ->
  FunctionEntry ->
  Callsite ->
  ResolveReturn ->
  GetCFG ->
  IOState CFGCache [ExplorationTask]
applyReturn (CallStack (x : rst)) _ _ _ _ = do
  mbCfg <- getCFGFromID (cfgId x)
  pure $
    maybe
      []
      (\cfg -> [ExplorationTask{expCfg = cfg, expStatmentNode = statementNode x, expCallStack = CallStack rst}])
      mbCfg
applyReturn (CallStack []) fentry cs retResolve cfgGetter =
  let locs = retResolve fentry cs
   in List.concat
        <$> forM locs (\x -> findReturnSites x cfgGetter (CallStack []) retResolve)

-- | An identifier for a statement location
data StatementNode = StatementNode {blockId :: Int, statId :: Int} deriving (Eq, Show, Ord)

nextSnode :: StatementNode -> StatementNode
nextSnode snode = StatementNode{blockId = blockId snode, statId = statId snode + 1}

data InterNode = InterNode {stateNode :: StatementNode, funcEntry :: FunctionEntry}
  deriving (Eq, Show, Ord)

data DijkstraCaches = DijkstraCaches
  {_returnDistCache :: DistCache, _targetDistCache :: DistCache, _dijkstraCfgCache :: CFGCache}
$(makeLenses ''DijkstraCaches)

emptyDijkstraCaches :: DijkstraCaches
emptyDijkstraCaches =
  DijkstraCaches
    { _returnDistCache = Map.empty
    , _targetDistCache = Map.empty
    , _dijkstraCfgCache = CFGCache Map.empty
    }

type DijkstraQueue = OrdPSQ.OrdPSQ StatementNode Distance ()
data DijkstraState = DijkstraState
  { _visitedNodeMinDist :: Map.Map StatementNode (Maybe Distance)
  , _targetDistances :: Map.Map StatementNode (Maybe Distance)
  , _priorityQueue :: DijkstraQueue
  , _caches :: DijkstraCaches
  }
$(makeLenses ''DijkstraState)

-- | Dijkstras finding a minimal path in a CFG provided a way to get call distances
-- Terminates early when the shortest distance for all targets is resolved. When a call is reached there are two things that need to happen
-- 1. it may be the case that the call leads to target -> in which case we have a distance to target (curr + x) so update
-- 2. we also need to explore succ by (computeMinReturnDist target + x)
--
-- For returns, we only need to check if we can hit target.
--
-- In order to stop early we need to know all nodes that could reach our target of interest
type CallDistanceResolver =
  FunctionEntry -> Callsite -> DistanceMonad DijkstraCaches (Maybe Distance)

type InterTargetDistanceResolver =
  FunctionEntry -> StatementNode -> DistanceMonad DijkstraCaches (Maybe Distance)

-- | Collects the minimum distance in the state
minDist :: [StatementNode] -> IOState DijkstraState (Maybe Distance)
minDist statTargets = do
  visited <- zoom targetDistances get
  pure $ minimumMay $ Maybe.catMaybes $ Maybe.mapMaybe (`Map.lookup` visited) statTargets

allMapped :: [StatementNode] -> IOState DijkstraState Bool
allMapped statTargets = do
  visited <- zoom targetDistances get
  pure $ all (`Map.member` visited) statTargets

popMinPsq :: (Ord k, Ord p) => IOState (OrdPSQ.OrdPSQ k p v) (Maybe (k, p, v))
popMinPsq =
  do
    psq <- get
    let mbMin = OrdPSQ.findMin psq
    put (OrdPSQ.deleteMin psq)
    pure mbMin

withIntraSuccessors ::
  forall a.
  StatementNode ->
  Some.Some CCC.AnyCFG ->
  ( forall blocks ext ret ctx ctx'.
    CrucibleStmt blocks ext ret ctx ctx' -> StatementNode -> WPL.ProgramLoc -> a
  ) ->
  [a]
withIntraSuccessors snode cfg f = withStatement snode cfg $ \_ loc stat ->
  let normSucc :: CCC.BlockID blks tp -> a
      normSucc bid = f stat (makeJmpNorm bid) loc
   in case stat of
        -- jumps
        Left (CCC.Jump (CCC.JumpTarget bid _ _)) -> [normSucc bid]
        Left (CCC.Br _ (CCC.JumpTarget bid1 _ _) (CCC.JumpTarget bid2 _ _)) -> [normSucc bid1, normSucc bid2]
        Left (CCC.MaybeBranch _ _ (CCC.SwitchTarget bid1 _ _) (CCC.JumpTarget bid2 _ _)) -> [normSucc bid1, normSucc bid2]
        Left (CCC.VariantElim _ _ tgts) -> TFC.toListFC (\(CCC.SwitchTarget bid _ _) -> normSucc bid) tgts
        -- We only look for intra targets
        Left (CCC.Return _) -> []
        Left (CCC.TailCall{}) -> []
        Left (CCC.ErrorStmt _) -> []
        Right (CCC.CallHandle{}) -> [f stat (nextSnode snode) loc]
        -- Any non-term besides a call can just be treated as next
        Right _ ->
          [f stat (nextSnode snode) loc]
 where
  makeJmpNorm :: CCC.BlockID blks tp -> StatementNode
  makeJmpNorm bidTarget = StatementNode{blockId = Ctx.indexVal $ CCC.blockIDIndex bidTarget, statId = 0}

-- | finds distance from current node to intraprocedural successors, for normal successors this is
-- just a distance of 1, for calls, the intra successor is the shortest ret distance
getDistToSuccessors ::
  FunctionEntry ->
  Distance ->
  StatementNode ->
  Some.Some CCC.AnyCFG ->
  CallDistanceResolver ->
  DistanceMonad DijkstraCaches [(StatementNode, Distance)]
getDistToSuccessors fEntry currDist currSnode cfg cdResolver =
  Maybe.catMaybes
    <$> Monad.sequence
      ( withIntraSuccessors currSnode cfg $ \stmt snode loc -> do
          let
            dist :: DistanceMonad DijkstraCaches (Maybe Distance)
            dist = case stmt of
              Right (CCC.CallHandle{}) -> do
                let callsite = Callsite loc
                dists <- cdResolver fEntry callsite
                pure (addDistance currDist <$> dists)
              _ -> pure $ Just $ offsetDistance currDist 1
           in
            ( do
                mbDist <- dist
                pure $ (snode,) <$> mbDist
            ) ::
              DistanceMonad DijkstraCaches (Maybe (StatementNode, Distance))
      )

updateNode :: StatementNode -> Distance -> IOState DijkstraQueue ()
updateNode tgtNode distanceCandidate =
  do
    psq <- get
    let it = OrdPSQ.lookup tgtNode psq
    let update = put $ OrdPSQ.insert tgtNode distanceCandidate () psq
    case it of
      Just (currDist, _)
        | distanceCandidate < currDist -> update
      Nothing -> update
      _ -> pure ()

-- | The type of target that a given statement is.
-- An 'InterTarget' may reach the target interprocedurally
-- an 'IntraTarget' reaches the target within this function.
data TargetType
  = IntraTarget
  | InterTarget
  deriving (Eq, Show, Ord)

logDebug :: (MonadIO m, PP.Pretty a) => ScreachLogAction -> a -> m ()
logDebug sla item = LJ.writeLog sla (ScrchDiagnostic.DistanceDiagnostic $ Diagnostic.GenericDebugOutput item)

{-
First check if all statement nodes are in the visited set if so halt, the result is the min of all StatementNode->Distance for the targets

If we are not done grab a node from the queue and fetch the statement. Then we need to eval the statement.
If it is a target commit it by using the 'TargetDistanceResolver'.

Otherwise we need to updateSuccessor, for updateSuccessor it is "normal" then the successor
is the next stmt (potentially in a next block, potentially multiple) and each of these targets is curr dist +1 and get queued
if it is a call then use calldistance resolver to determine the distance
if it is a return and not a target bail for now but we should use a return target resolver instead
-}
dijkstras ::
  -- | All intra targets, these targets may reach the "real" target.
  -- When searching for an interprocedural target this is going to be the return (if the return may reach),
  -- and calls that may reach, and potentially the target statement itself if it is present
  [(StatementNode, TargetType)] ->
  Some.Some CCC.AnyCFG ->
  CallDistanceResolver ->
  InterTargetDistanceResolver ->
  DistanceMonad DijkstraState (Maybe Distance)
dijkstras statTargets currCfg@(Some.Some (CCC.AnyCFG bdCfg)) cdResolver targetResolver = do
  let fentry = FunctionEntry $ CCC.blockLoc $ CCC.getBlock (CCC.cfgEntryBlockID bdCfg) (CCC.cfgBlockMap bdCfg)
  let nextWork = dijkstras statTargets currCfg cdResolver targetResolver
  let targetSnodes = List.map fst statTargets
  let currMinDist = lift $ minDist targetSnodes
  minIt <- lift $ zoom priorityQueue popMinPsq
  earlyStop <- lift $ allMapped targetSnodes
  case (earlyStop, minIt) of
    -- we are done, collect min dist
    (_, Nothing) -> currMinDist
    (True, _) -> currMinDist
    (_, Just (statementToVisit, currDist, _)) -> do
      chc <- zoom visitedNodeMinDist get
      case Map.lookup statementToVisit chc of
        Nothing -> do
          case List.find (\(stTarget, _) -> statementToVisit == stTarget) statTargets of
            Just (_, ty) ->
              do
                mbRemDist <- zoom caches $
                  case ty of
                    InterTarget -> targetResolver fentry statementToVisit
                    IntraTarget -> pure $ Just $ Distance 0
                let dist = addDistance currDist <$> mbRemDist
                lift
                  $ zoom
                    targetDistances
                  $ modify (Map.insert statementToVisit dist)
            Nothing -> pure ()
          zoom visitedNodeMinDist $ modify (Map.insert statementToVisit (Just currDist))
          -- we've updated our distance now go see if we have any successors to update
          successors <-
            mapReaderT (zoom caches) (getDistToSuccessors fentry currDist statementToVisit currCfg cdResolver)
          lift $
            zoom priorityQueue $
              forM_ successors $
                uncurry updateNode
          pure ()
        Just _ -> pure ()
      nextWork

cfgEntryBlockID :: Some.Some CCC.AnyCFG -> InterproceduralBlockID
cfgEntryBlockID (Some.Some (CCC.AnyCFG cfg)) =
  InterproceduralBlockID
    { cfgId = cfgIDFromHandle $ CCC.cfgHandle cfg
    , statementNode = cfgEntrySnode cfg
    }

interBlockIDFromSnode :: Some.Some CCC.AnyCFG -> StatementNode -> InterproceduralBlockID
interBlockIDFromSnode (Some.Some (CCC.AnyCFG cfg)) snode =
  InterproceduralBlockID
    { cfgId = cfgIDFromHandle $ CCC.cfgHandle cfg
    , statementNode = snode
    }

findCallWithLoc ::
  CCC.CFG ext block init arg -> WPL.ProgramLoc -> CCC.BlockID block bargs -> Maybe StatementNode
findCallWithLoc cfg searchLoc bid =
  Maybe.listToMaybe $ allStatementsOfBlock bid cfg $ \snode _ loc stmt ->
    case stmt of
      (Right (CCC.CallHandle{})) | loc == searchLoc -> [snode]
      _ -> []

-- | Takes a call frame and caches the CFG against its ID so it can be referenced in the future and produces a block ID for it
interBlockIDFromFrame ::
  C.CallFrame sym ext blocks ret args -> IOState DijkstraCaches InterproceduralBlockID
interBlockIDFromFrame cf@C.CallFrame{C._frameCFG = cfg, C._frameBlockID = Some.Some bid} = do
  let packedAnyCfg = Some.Some $ CCC.AnyCFG cfg
  let cid = cfgIDFromHandle $ CCC.cfgHandle cfg
  let loc = C.frameProgramLoc cf
  dijkstraCfgCache %= insertCFGIntoCache cid packedAnyCfg
  -- Assumption: this should be safe since we are in a callframe a call frame should only result from
  -- A call (assuming tail call frames are discarded so we should be able to find the corresponding call to a frame we
  -- return from)
  let snodeOfCall = case findCallWithLoc cfg loc bid of
        Nothing -> Scrch.panic "Should be able to find call inside CFG for call!" []
        Just x -> x
  let fSnode = nextSnode snodeOfCall
  pure $ InterproceduralBlockID{cfgId = cid, statementNode = fSnode}

newtype IsTarget
  = IsTarget
      ( forall ext blocks ret ctx ctx'.
        FunctionEntry ->
        WPL.ProgramLoc ->
        CrucibleStmt blocks ext ret ctx ctx' ->
        Maybe TargetType
      )

-- | Either return a cached distance or compute a fresh result from the provided computation
cachedDistanceOrRun ::
  InterproceduralBlockID ->
  Lens' DijkstraCaches DistCache ->
  Some.Some CCC.AnyCFG ->
  DistanceMonad DijkstraCaches (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
cachedDistanceOrRun target cacheLens cfg cont = do
  let entryINode = cfgEntryBlockID cfg
  cache <- zoom cacheLens get
  res <-
    -- we place a temporary Nothing in the map so that if we hit
    -- a recursive call we return nothing on that path
    -- this should be safe because a recursive call
    -- should always be the long path the target since there should be a base case
    -- that we should just return out of.
    -- These are separate caches so we still allow a return.
    maybe
      ( do
          _ <- zoom cacheLens (put $ Map.insert entryINode Nothing cache)
          cont
      )
      pure
      (Map.lookup target cache)
  zoom cacheLens (put $ Map.insert target res cache)
  pure res

-- | The dist rsolver constructs an 'InterTargetDistanceResolver' from the current target resolution components (e.g. 'IsTarget')
-- This dist resolver is used within djikstras to reinvoke djikstras on an interprocedural call that might reach the target function.
-- Both TailCalls and calls are followed directly, but returns have to be handled specially. The 'ReturnHandler' declares
-- the stack of a valid returns for a context, callees are not allowed to return so when searching down from the callees of a given function the
-- return handler is set to none. Returns recur on the caller with a frame popped off the stack.
distResolver ::
  Some.Some CCC.AnyCFG ->
  ScreachLogAction ->
  ReturnHandler ->
  ResolveCall ->
  IsTarget ->
  FunctionEntry ->
  StatementNode ->
  DistanceMonad DijkstraCaches (Maybe Distance)
distResolver cfg sla retHandler rCall isTarget fentry callingNode =
  withStatement callingNode cfg $ \_ loc stmt ->
    let cs = Callsite loc
        followCall newRhandle =
          do
            _ <- liftIO $ logDebug sla "following call in distResolver"
            -- TODO(internal#106) rcall is going to need to coordinate the cache of cfgs right here
            newCFGS <- liftIO $ rCall fentry cs
            alldists <- forM newCFGS $ \newCFG ->
              let entrySnode = cfgEntryBlockID newCFG
               in computeMinDistanceTargetsFromStatmementExt
                    newCFG
                    sla
                    (statementNode entrySnode)
                    isTarget
                    newRhandle
                    rCall
            pure $ minimumMay $ Maybe.catMaybes alldists
     in case (stmt, retHandler) of
          (Right (CCC.CallHandle{}), _) -> followCall $ ReturnHandler Nothing
          -- if we are going to do a tail call we have to allow the callee to return out in the same state
          (Left (CCC.TailCall{}), _) -> followCall retHandler
          (Left (CCC.Return{}), ReturnHandler (Just rinfo)) -> do
            _ <- logDebug sla "handling return"
            tasks <-
              lift $
                zoom dijkstraCfgCache $
                  applyReturn (returnCallstack rinfo) fentry cs (returnResolver rinfo) (returnCfgBuidler rinfo)
            alldists <- forM tasks $ \expTask ->
              let nextRetHandler = ReturnHandler (Just $ rinfo{returnCallstack = expCallStack expTask})
               in computeMinDistanceTargetsFromStatmementExt
                    (expCfg expTask)
                    sla
                    (expStatmentNode expTask)
                    isTarget
                    nextRetHandler
                    rCall
            pure $ minimumMay $ Maybe.catMaybes alldists
          _ -> pure Nothing

-- | Collects locations in the CFG that match the given predicate
collectLocEquivs ::
  (MM.MemWidth 64) =>
  Some.Some CCC.AnyCFG ->
  -- | Whether this location matches the target location
  (WPL.ProgramLoc -> Bool) ->
  [StatementNode]
collectLocEquivs cfg isTgt =
  allStatements cfg $ \snode _ loc _ -> [snode | isTgt loc]

freshDijkstraState :: Some.Some CCC.AnyCFG -> DistanceMonad DijkstraCaches DijkstraState
freshDijkstraState cfg = do
  conf <- Reader.ask
  let infLocs = collectLocEquivs cfg (isInfiniteDistLoc conf)
  currCaches <- get
  pure $
    DijkstraState
      { _caches = currCaches
      , _visitedNodeMinDist = Map.fromList ((,Nothing) <$> infLocs)
      , _targetDistances = Map.empty
      , _priorityQueue = OrdPSQ.empty
      }

collectDistances :: DefaultReturnDist -> [Maybe Distance] -> Maybe Distance
collectDistances (DefaultReturnDist rdist) dists =
  case dists of
    [] -> Just $ Distance rdist
    _ -> minimumMay $ Maybe.catMaybes dists

callDistResolverFromResolveCall ::
  ScreachLogAction ->
  ResolveCall ->
  FunctionEntry ->
  Callsite ->
  DistanceMonad DijkstraCaches (Maybe Distance)
callDistResolverFromResolveCall sla callResolve fEntry callSite = do
  conf <- Reader.ask
  cfgs <- liftIO $ callResolve fEntry callSite
  dists <- forM cfgs $ \currCfg ->
    computeMinimumReturnDistance sla currCfg callResolve
  pure $ collectDistances (defaultRetDist conf) dists

runDijkstrasFromSnode ::
  Some.Some CCC.AnyCFG ->
  StatementNode ->
  DistanceMonad DijkstraState (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
runDijkstrasFromSnode cfg startSnode dijkstrasRun = do
  frsh <- freshDijkstraState cfg
  let nstate = queueEntry frsh
  (resDist, fullState) <-
    mapReaderT
      ( \(s :: (StateT DijkstraState IO) (Maybe Distance)) ->
          liftIO $ runStateT s nstate
      )
      dijkstrasRun
  put (_caches fullState)
  pure resDist
 where
  queueEntry :: DijkstraState -> DijkstraState
  queueEntry = over priorityQueue (OrdPSQ.insert startSnode (Distance 0) ())

runDijkstrasFromEntry ::
  Some.Some CCC.AnyCFG ->
  DistanceMonad DijkstraState (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
runDijkstrasFromEntry cfg djikstasRun =
  let snode = cfgEntryBlockID cfg
   in runDijkstrasFromSnode cfg (statementNode snode) djikstasRun

data ReturnResolutionInfo = ReturnResolutionInfo
  {returnResolver :: ResolveReturn, returnCfgBuidler :: GetCFG, returnCallstack :: CallStack}
newtype ReturnHandler = ReturnHandler (Maybe ReturnResolutionInfo)

computeMinDistanceTargetsFromStatmementExt ::
  Some.Some CCC.AnyCFG ->
  ScreachLogAction ->
  StatementNode ->
  IsTarget ->
  ReturnHandler ->
  ResolveCall ->
  DistanceMonad DijkstraCaches (Maybe Distance)
computeMinDistanceTargetsFromStatmementExt cfg sla startSnode tgts@(IsTarget isTarget) retHandler resolveCall =
  cachedDistanceOrRun
    (interBlockIDFromSnode cfg startSnode)
    targetDistCache
    cfg
    ( do
        let cloc = getCFGLoc cfg
        _ <-
          liftIO $ logDebug sla ("computing min dist for cfg: " ++ show cloc ++ " snode: " ++ show startSnode)
        let currDistReslver = distResolver cfg sla retHandler resolveCall tgts
        let mbFunEntry = cfgFunctionEntry cfg
        maybe
          (pure ())
          (\(FunctionEntry addr) -> LJ.writeLog sla (ScrchDiagnostic.DistanceDiagnostic $ Diagnostic.MinTarget addr))
          mbFunEntry
        let tgtList =
              maybe
                []
                ( \fentry ->
                    allStatements
                      cfg
                      ( \snode _ loc stmt ->
                          Maybe.maybeToList $ (snode,) <$> isTarget fentry loc stmt
                      )
                )
                mbFunEntry
        _ <- liftIO $ logDebug sla ("Target list: " ++ show tgtList)
        let dist = dijkstras tgtList cfg (callDistResolverFromResolveCall sla resolveCall) currDistReslver
        r <- runDijkstrasFromSnode cfg startSnode dist
        _ <- liftIO $ logDebug sla ("done computed min dist for cfg: " ++ show cloc)
        pure r
    )

collectReturnTargets :: Some.Some CCC.AnyCFG -> [(StatementNode, TargetType)]
collectReturnTargets cfg = allStatements cfg $ \snode _ _ stmt ->
  case stmt of
    Left (CCC.Return _) -> [(snode, IntraTarget)]
    Left (CCC.TailCall{}) -> [(snode, InterTarget)]
    _ -> []

minReturnDistResolver ::
  ScreachLogAction ->
  Some.Some CCC.AnyCFG ->
  ResolveCall ->
  FunctionEntry ->
  StatementNode ->
  DistanceMonad DijkstraCaches (Maybe Distance)
minReturnDistResolver sla cfg rCall fentry callingNode =
  withStatement callingNode cfg $ \_ loc stmt ->
    let cs = Callsite loc
        followCall =
          do
            -- TODO(internal#106)  rcall is going to need to coordinate the cache of cfgs right here
            newCFGS <- liftIO $ rCall fentry cs
            conf <- Reader.ask
            alldists <- forM newCFGS $ \newCFG ->
              computeMinimumReturnDistance sla newCFG rCall
            pure $ collectDistances (defaultRetDist conf) alldists
     in case stmt of
          Left (CCC.TailCall{}) -> followCall
          _ -> pure Nothing

getCFGLoc :: Some.Some CCC.AnyCFG -> WPL.ProgramLoc
getCFGLoc (Some.Some (CCC.AnyCFG up)) =
  let ent = CCC.getBlock (CCC.cfgEntryBlockID up) (CCC.cfgBlockMap up)
   in case view CCC.blockStmts ent of
        CCC.TermStmt cloc _ -> cloc
        CCC.ConsStmt cloc _ _ -> cloc

-- | Computes the minimum interprocedural distance to a return. We discard the target cache because we should only impact the
-- return dist cache
computeMinimumReturnDistance ::
  ScreachLogAction ->
  Some.Some CCC.AnyCFG ->
  ResolveCall ->
  DistanceMonad DijkstraCaches (Maybe Distance)
computeMinimumReturnDistance sla cfg rcall =
  cachedDistanceOrRun
    (cfgEntryBlockID cfg)
    returnDistCache
    cfg
    ( do
        let loc = getCFGLoc cfg
        _ <- liftIO $ logDebug sla ("computing return for cfg: " ++ show loc)
        let retTarget = collectReturnTargets cfg
        let dijkstraState =
              dijkstras
                retTarget
                cfg
                (callDistResolverFromResolveCall sla rcall)
                (minReturnDistResolver sla cfg rcall)
        res <- runDijkstrasFromEntry cfg dijkstraState
        _ <- liftIO $ logDebug sla ("Done with return: " ++ show loc ++ " dist: " ++ show res)
        pure res
    )
