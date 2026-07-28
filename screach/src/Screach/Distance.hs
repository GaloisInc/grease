{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Screach implements \"shortest-distance symbolic execution\" (SDSE), a form
-- of directed symbolic execution that prioritizes exploring states that are
-- closer to the target program point. (See [file:screach/doc/overview.md] for
-- more details). This module defines the relevant notion of "closeness", i.e.,
-- 'Distance', and the algorithm for calculating it, 'computeDistance'.
module Screach.Distance (
  FunctionEntry (..),
  Callsite (..),
  IsTarget (..),
  TargetType (..),
  CrucibleStmt,
  emptyDijkstraCaches,
  StmtIdx (..),
  LocalStmtId (..),
  cfgEntryStmtId,
  Distance (..),
  DijkstraCaches,
  computeDistance,
  ReturnResolutionInfo (..),
  ReturnHandler (..),
  AddressLocation (..),
  allStmts,
  CallStack (..),
  GlobalStmtId,
  globalStmtIdFromFrame,
  DefaultReturnDist (..),
  DistanceConfig (..),
  DistanceMonad,
) where

import Control.Monad (forM, forM_)
import Control.Monad qualified as Monad
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict (MonadIO (liftIO), MonadState (get, put), StateT, modify, runStateT)
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
import Lens.Micro (Lens', over)
import Lens.Micro.Extras (view)
import Lens.Micro.Mtl (zoom, (%=))
import Lens.Micro.TH (makeLenses)
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import Safe (minimumMay)
import Screach.Diagnostic (ScreachLogAction)
import Screach.Diagnostic qualified as ScrchDiagnostic
import Screach.Distance.Diagnostic qualified as Diagnostic
import Screach.Panic qualified as Scrch
import What4.ProgramLoc qualified as WPL

-- | Distance between program points is defined as the number of Crucible
-- statements on the shortest CFG path between them.
newtype Distance = Distance {distNumStmts :: Int} deriving (Eq, Show, Ord)

offsetDistance :: Distance -> Int -> Distance
offsetDistance (Distance x) num = Distance $ x + num

addDistance :: Distance -> Distance -> Distance
addDistance (Distance x) (Distance y) = Distance $ x + y

newtype CfgId = CfgId Word64 deriving (Eq, Show, Ord)

cfgIdFromHandle :: CFH.FnHandle init ret -> CfgId
cfgIdFromHandle = CfgId . Nonce.indexValue . CFH.handleID

-- | A zero-based statement index within a basic block.
newtype StmtIdx = StmtIdx Int deriving (Eq, Show, Ord)

-- | A CFG-local identifier for a statement (c.f. 'GlobalStmtId')
data LocalStmtId = LocalStmtId {blockId :: Int, stmtIdx :: StmtIdx} deriving (Eq, Show, Ord)

nextStmtId :: LocalStmtId -> LocalStmtId
nextStmtId sId =
  let StmtIdx idx = stmtIdx sId
   in LocalStmtId{blockId = blockId sId, stmtIdx = StmtIdx (idx + 1)}

firstLocalStmtIdOfBlock :: CCC.Block ext blocks ret ctx -> LocalStmtId
firstLocalStmtIdOfBlock block =
  LocalStmtId{blockId = (Ctx.indexVal . CCC.blockIDIndex . CCC.blockID) block, stmtIdx = StmtIdx 0}

cfgEntryStmtId :: CCC.CFG x blocks init ret -> LocalStmtId
cfgEntryStmtId cfg =
  firstLocalStmtIdOfBlock $ CCC.getBlock (CCC.cfgEntryBlockID cfg) (CCC.cfgBlockMap cfg)

-- | A global identifier for a statement (c.f. 'LocalStmtId')
data GlobalStmtId = GlobalStmtId
  { cfgId :: CfgId
  , localStmtId :: LocalStmtId
  }
  deriving (Eq, Show, Ord)

-- | State holding already computed function return distances.
--
-- Assumes that 'WPL.ProgramLoc's of distinct functions are distinct.
type DistCache = Map.Map GlobalStmtId (Maybe Distance)

newtype FunctionEntry = FunctionEntry WPL.ProgramLoc deriving (Eq, Show, Ord)
newtype Callsite = Callsite WPL.ProgramLoc deriving (Ord, Eq, Show)

type ResolveCall = FunctionEntry -> Callsite -> IO [Some.Some CCC.AnyCFG]

data AddressLocation = AddressLocation {addrFunctionEntry :: WPL.ProgramLoc, addrInsnLoc :: Maybe WPL.ProgramLoc}
  deriving (Show)

-- | Function we use to resolve a non searched return
type ResolveReturn = FunctionEntry -> Callsite -> [AddressLocation]

newtype CallStack = CallStack [GlobalStmtId] deriving (Show)

newtype CFGCache = CFGCache (Map.Map CfgId (Some.Some CCC.AnyCFG))

insertCFGIntoCache :: CfgId -> Some.Some CCC.AnyCFG -> CFGCache -> CFGCache
insertCFGIntoCache cid packedCfg (CFGCache mp) = CFGCache $ Map.insert cid packedCfg mp

type GetCFG = FunctionEntry -> IO (Maybe (Some.Some CCC.AnyCFG))

data ExplorationTask = ExplorationTask
  {expCfg :: Some.Some CCC.AnyCFG, expCallStack :: CallStack, expStmtId :: LocalStmtId}

instance Show ExplorationTask where
  show ExplorationTask{expCfg = _, expCallStack = cs, expStmtId = sId} =
    "ExplorationTask" ++ "{" ++ show cs ++ "," ++ show sId ++ "}"

type IOState s a = StateT s IO a

-- | The default assumed return distance for a call that (for whatever reason)
-- does not have a path to a return.
--
-- This can happen when a function has missing callsites in the callgraph. This
-- distance is used when computing the distance between a source and a target
-- with an intervening call.
--
-- [ref:default_return_dist]
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
        let cid = cfgIdFromHandle $ CCC.cfgHandle cfg
         in modify $ insertCFGIntoCache cid pked
      Nothing -> pure ()
    pure mbCfg

cfgFromId :: CfgId -> IOState CFGCache (Maybe (Some.Some CCC.AnyCFG))
cfgFromId targetId = do
  CFGCache mp <- get
  pure $ Map.lookup targetId mp

allStmtsOfBlock ::
  forall m actext actblocks actinit actret args.
  (Monoid m) =>
  CCC.BlockID actblocks args ->
  CCC.CFG actext actblocks actinit actret ->
  ( forall blocks ext ret ctx ctx' ctx''.
    LocalStmtId ->
    CCC.Block ext blocks ret ctx'' ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    m
  ) ->
  m
allStmtsOfBlock bid cfg f =
  let x = CCC.getBlock bid $ CCC.cfgBlockMap cfg
      stmts = view CCC.blockStmts x
   in applyToStmts x (firstLocalStmtIdOfBlock x) stmts
 where
  applyToStmts ::
    CCC.Block ext blocks ret ctx'' -> LocalStmtId -> CCC.StmtSeq ext blocks ret ctx -> m
  applyToStmts blk sId (CCC.TermStmt loc term) = f sId blk loc (Left term)
  applyToStmts blk sId (CCC.ConsStmt loc stmt rst) =
    f sId blk loc (Right stmt)
      <> applyToStmts blk (nextStmtId sId) rst

allStmts ::
  forall m.
  (Monoid m) =>
  Some.Some CCC.AnyCFG ->
  ( forall blocks ext ret ctx ctx' ctx''.
    LocalStmtId ->
    CCC.Block ext blocks ret ctx'' ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    m
  ) ->
  m
allStmts (Some.Some (CCC.AnyCFG cfg)) f =
  let blockMap = CCC.cfgBlockMap cfg
   in TFC.ifoldMapFC
        ( \i _ ->
            allStmtsOfBlock (CCC.BlockID i) cfg f
        )
        blockMap

allCallIds :: Some.Some CCC.AnyCFG -> [LocalStmtId]
allCallIds cfg = allStmts cfg $ \sId _ _ stmt ->
  case stmt of
    Right CCC.CallHandle{} -> [sId]
    _ -> []

findInCFG :: Maybe WPL.ProgramLoc -> [LocalStmtId] -> Some.Some CCC.AnyCFG -> [LocalStmtId]
findInCFG Nothing defaultIds _ = defaultIds
findInCFG (Just loc) _ cfg = allStmts cfg $ \sId _ currLoc _ ->
  -- TODO(internal#104) fix this to compare addresses
  ([sId | WPL.plSourceLoc currLoc == WPL.plSourceLoc loc])

findCallingSites ::
  AddressLocation -> GetCFG -> IOState CFGCache [(Some.Some CCC.AnyCFG, LocalStmtId)]
findCallingSites loc cfgGetter =
  do
    mbLst <-
      runMaybeT
        ( do
            cfg <- MaybeT $ getCFG (FunctionEntry $ addrFunctionEntry loc) cfgGetter
            let lst = findInCFG (addrInsnLoc loc) (allCallIds cfg) cfg
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
  LocalStmtId ->
  ResolveReturn ->
  GetCFG ->
  IOState CFGCache [ExplorationTask]
collectReturnSite cfg callstack sId rReturn cfgBuilder =
  withStatement sId cfg $ \_ loc stmt ->
    case stmt of
      -- we should only return to callsites
      Right (CCC.CallHandle{}) ->
        pure [ExplorationTask{expCfg = cfg, expCallStack = callstack, expStmtId = nextStmtId sId}]
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
    List.concat <$> forM lst (\(cfg, sId) -> collectReturnSite cfg callstack sId rReturn cfgGetter)

type CrucibleStmt blocks ext ret ctx' ctx'' =
  Either (CCC.TermStmt blocks ret ctx') (CCC.Stmt ext ctx' ctx'')

-- TODO(internal#105) dont do this, we shouldn't be iterating repeatedly through statements to get a statement index
withStatementFromBlock ::
  forall ext blocks ret ctx r.
  StmtIdx ->
  CCC.Block ext blocks ret ctx ->
  (forall ctx' ctx''. WPL.ProgramLoc -> CrucibleStmt blocks ext ret ctx' ctx'' -> r) ->
  r
withStatementFromBlock idx blk cont =
  let stmts = view CCC.blockStmts blk
   in getInd idx stmts
 where
  getInd :: StmtIdx -> CCC.StmtSeq ext blocks ret someCtx -> r
  getInd (StmtIdx 0) (CCC.ConsStmt loc stmt _) = cont loc $ Right stmt
  getInd (StmtIdx 0) (CCC.TermStmt loc term) = cont loc $ Left term
  getInd (StmtIdx i) (CCC.ConsStmt _ _ rst) = getInd (StmtIdx (i - 1)) rst
  getInd _ ((CCC.TermStmt _ _)) = Scrch.panic "unreachable! out of bounds stmt id" []

withStatement ::
  LocalStmtId ->
  Some.Some CCC.AnyCFG ->
  ( forall bctx tp ext blocks ret ctx ctx'.
    CCC.Block ext blocks tp bctx ->
    WPL.ProgramLoc ->
    CrucibleStmt blocks ext ret ctx ctx' ->
    r
  ) ->
  r
withStatement sId (Some.Some (CCC.AnyCFG cfg)) cont =
  let bbMap = CCC.cfgBlockMap cfg
      bidRaw = blockId sId
      cid = Ctx.intIndex bidRaw (Ctx.size bbMap)
   in case cid of
        Just (Some.Some ind) ->
          let bid = CCC.BlockID ind
              blk = CCC.getBlock bid bbMap
           in withStatementFromBlock (stmtIdx sId) blk (cont blk)
        _ -> Scrch.panic "unreachable! out of bounds block id" []
applyReturn ::
  CallStack ->
  FunctionEntry ->
  Callsite ->
  ResolveReturn ->
  GetCFG ->
  IOState CFGCache [ExplorationTask]
applyReturn (CallStack (x : rst)) _ _ _ _ = do
  mbCfg <- cfgFromId (cfgId x)
  pure $
    maybe
      []
      (\cfg -> [ExplorationTask{expCfg = cfg, expStmtId = localStmtId x, expCallStack = CallStack rst}])
      mbCfg
applyReturn (CallStack []) fentry cs retResolve cfgGetter =
  let locs = retResolve fentry cs
   in List.concat
        <$> forM locs (\x -> findReturnSites x cfgGetter (CallStack []) retResolve)

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

type DijkstraQueue = OrdPSQ.OrdPSQ LocalStmtId Distance ()
data DijkstraState = DijkstraState
  { _visitedIdMinDist :: Map.Map LocalStmtId (Maybe Distance)
  , _targetDistances :: Map.Map LocalStmtId (Maybe Distance)
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
  FunctionEntry -> LocalStmtId -> DistanceMonad DijkstraCaches (Maybe Distance)

-- | Collects the minimum distance in the state
minDist :: [LocalStmtId] -> IOState DijkstraState (Maybe Distance)
minDist stmtTargets = do
  visited <- zoom targetDistances get
  pure $ minimumMay $ Maybe.catMaybes $ Maybe.mapMaybe (`Map.lookup` visited) stmtTargets

allMapped :: [LocalStmtId] -> IOState DijkstraState Bool
allMapped stmtTargets = do
  visited <- zoom targetDistances get
  pure $ all (`Map.member` visited) stmtTargets

popMinPsq :: (Ord k, Ord p) => IOState (OrdPSQ.OrdPSQ k p v) (Maybe (k, p, v))
popMinPsq =
  do
    psq <- get
    let mbMin = OrdPSQ.findMin psq
    put (OrdPSQ.deleteMin psq)
    pure mbMin

withIntraSuccessors ::
  forall a.
  LocalStmtId ->
  Some.Some CCC.AnyCFG ->
  ( forall blocks ext ret ctx ctx'.
    CrucibleStmt blocks ext ret ctx ctx' -> LocalStmtId -> WPL.ProgramLoc -> a
  ) ->
  [a]
withIntraSuccessors sId cfg f = withStatement sId cfg $ \_ loc stmt ->
  let normSucc :: CCC.BlockID blks tp -> a
      normSucc bid = f stmt (makeJmpNorm bid) loc
   in case stmt of
        -- jumps
        Left (CCC.Jump (CCC.JumpTarget bid _ _)) -> [normSucc bid]
        Left (CCC.Br _ (CCC.JumpTarget bid1 _ _) (CCC.JumpTarget bid2 _ _)) -> [normSucc bid1, normSucc bid2]
        Left (CCC.MaybeBranch _ _ (CCC.SwitchTarget bid1 _ _) (CCC.JumpTarget bid2 _ _)) -> [normSucc bid1, normSucc bid2]
        Left (CCC.VariantElim _ _ tgts) -> TFC.toListFC (\(CCC.SwitchTarget bid _ _) -> normSucc bid) tgts
        -- We only look for intra targets
        Left (CCC.Return _) -> []
        Left (CCC.TailCall{}) -> []
        Left (CCC.ErrorStmt _) -> []
        Right (CCC.CallHandle{}) -> [f stmt (nextStmtId sId) loc]
        -- Any non-term besides a call can just be treated as next
        Right _ ->
          [f stmt (nextStmtId sId) loc]
 where
  makeJmpNorm :: CCC.BlockID blks tp -> LocalStmtId
  makeJmpNorm bidTarget =
    LocalStmtId{blockId = Ctx.indexVal $ CCC.blockIDIndex bidTarget, stmtIdx = StmtIdx 0}

-- | finds distance from current node to intraprocedural successors, for normal successors this is
-- just a distance of 1, for calls, the intra successor is the shortest ret distance
getDistToSuccessors ::
  FunctionEntry ->
  Distance ->
  LocalStmtId ->
  Some.Some CCC.AnyCFG ->
  CallDistanceResolver ->
  DistanceMonad DijkstraCaches [(LocalStmtId, Distance)]
getDistToSuccessors fEntry currDist currId cfg cdResolver =
  Maybe.catMaybes
    <$> Monad.sequence
      ( withIntraSuccessors currId cfg $ \stmt sId loc -> do
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
                pure $ (sId,) <$> mbDist
            ) ::
              DistanceMonad DijkstraCaches (Maybe (LocalStmtId, Distance))
      )

updateId :: LocalStmtId -> Distance -> IOState DijkstraQueue ()
updateId tgtId distanceCandidate =
  do
    psq <- get
    let it = OrdPSQ.lookup tgtId psq
    let update = put $ OrdPSQ.insert tgtId distanceCandidate () psq
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
First check if all statement nodes are in the visited set if so halt, the result is the min of all LocalStmtId->Distance for the targets

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
  [(LocalStmtId, TargetType)] ->
  Some.Some CCC.AnyCFG ->
  CallDistanceResolver ->
  InterTargetDistanceResolver ->
  DistanceMonad DijkstraState (Maybe Distance)
dijkstras stmtTargets currCfg@(Some.Some (CCC.AnyCFG bdCfg)) cdResolver targetResolver = do
  let fentry = FunctionEntry $ CCC.blockLoc $ CCC.getBlock (CCC.cfgEntryBlockID bdCfg) (CCC.cfgBlockMap bdCfg)
  let nextWork = dijkstras stmtTargets currCfg cdResolver targetResolver
  let targetIds = List.map fst stmtTargets
  let currMinDist = lift $ minDist targetIds
  minIt <- lift $ zoom priorityQueue popMinPsq
  earlyStop <- lift $ allMapped targetIds
  case (earlyStop, minIt) of
    -- we are done, collect min dist
    (_, Nothing) -> currMinDist
    (True, _) -> currMinDist
    (_, Just (statementToVisit, currDist, _)) -> do
      chc <- zoom visitedIdMinDist get
      case Map.lookup statementToVisit chc of
        Nothing -> do
          case List.find (\(stTarget, _) -> statementToVisit == stTarget) stmtTargets of
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
          zoom visitedIdMinDist $ modify (Map.insert statementToVisit (Just currDist))
          -- we've updated our distance now go see if we have any successors to update
          successors <-
            mapReaderT (zoom caches) (getDistToSuccessors fentry currDist statementToVisit currCfg cdResolver)
          lift $
            zoom priorityQueue $
              forM_ successors $
                uncurry updateId
          pure ()
        Just _ -> pure ()
      nextWork

cfgEntryGlobalStmtId :: Some.Some CCC.AnyCFG -> GlobalStmtId
cfgEntryGlobalStmtId (Some.Some (CCC.AnyCFG cfg)) =
  GlobalStmtId
    { cfgId = cfgIdFromHandle $ CCC.cfgHandle cfg
    , localStmtId = cfgEntryStmtId cfg
    }

globalStmtId :: Some.Some CCC.AnyCFG -> LocalStmtId -> GlobalStmtId
globalStmtId (Some.Some (CCC.AnyCFG cfg)) sId =
  GlobalStmtId
    { cfgId = cfgIdFromHandle $ CCC.cfgHandle cfg
    , localStmtId = sId
    }

findCallWithLoc ::
  CCC.CFG ext block init arg -> WPL.ProgramLoc -> CCC.BlockID block bargs -> Maybe LocalStmtId
findCallWithLoc cfg searchLoc bid =
  Maybe.listToMaybe $ allStmtsOfBlock bid cfg $ \sId _ loc stmt ->
    case stmt of
      (Right (CCC.CallHandle{})) | loc == searchLoc -> [sId]
      _ -> []

-- | Cache the CFG from a call frame and produce the global ID of the statement
-- to which the frame will return.
globalStmtIdFromFrame ::
  C.CallFrame sym ext blocks ret args -> IOState DijkstraCaches GlobalStmtId
globalStmtIdFromFrame cf@C.CallFrame{C._frameCFG = cfg, C._frameBlockID = Some.Some bid} = do
  let packedAnyCfg = Some.Some $ CCC.AnyCFG cfg
  let cid = cfgIdFromHandle $ CCC.cfgHandle cfg
  let loc = C.frameProgramLoc cf
  dijkstraCfgCache %= insertCFGIntoCache cid packedAnyCfg
  -- Assumption: this should be safe since we are in a callframe a call frame should only result from
  -- A call (assuming tail call frames are discarded so we should be able to find the corresponding call to a frame we
  -- return from)
  let callId = case findCallWithLoc cfg loc bid of
        Nothing -> Scrch.panic "Should be able to find call inside CFG for call!" []
        Just x -> x
  let fId = nextStmtId callId
  pure $ GlobalStmtId{cfgId = cid, localStmtId = fId}

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
  GlobalStmtId ->
  Lens' DijkstraCaches DistCache ->
  Some.Some CCC.AnyCFG ->
  DistanceMonad DijkstraCaches (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
cachedDistanceOrRun target cacheLens cfg cont = do
  let entryId = cfgEntryGlobalStmtId cfg
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
          _ <- zoom cacheLens (put $ Map.insert entryId Nothing cache)
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
  LocalStmtId ->
  DistanceMonad DijkstraCaches (Maybe Distance)
distResolver cfg sla retHandler rCall isTarget fentry callingId =
  withStatement callingId cfg $ \_ loc stmt ->
    let cs = Callsite loc
        followCall newRhandle =
          do
            _ <- liftIO $ logDebug sla "following call in distResolver"
            -- TODO(internal#106) rcall is going to need to coordinate the cache of cfgs right here
            newCFGS <- liftIO $ rCall fentry cs
            alldists <- forM newCFGS $ \newCFG ->
              let entryId = cfgEntryGlobalStmtId newCFG
               in computeDistance
                    sla
                    newCFG
                    (localStmtId entryId)
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
               in computeDistance
                    sla
                    (expCfg expTask)
                    (expStmtId expTask)
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
  [LocalStmtId]
collectLocEquivs cfg isTgt =
  allStmts cfg $ \sId _ loc _ -> [sId | isTgt loc]

freshDijkstraState :: Some.Some CCC.AnyCFG -> DistanceMonad DijkstraCaches DijkstraState
freshDijkstraState cfg = do
  conf <- Reader.ask
  let infLocs = collectLocEquivs cfg (isInfiniteDistLoc conf)
  currCaches <- get
  pure $
    DijkstraState
      { _caches = currCaches
      , _visitedIdMinDist = Map.fromList ((,Nothing) <$> infLocs)
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

runDijkstrasFromStmt ::
  Some.Some CCC.AnyCFG ->
  LocalStmtId ->
  DistanceMonad DijkstraState (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
runDijkstrasFromStmt cfg startStmtId dijkstrasRun = do
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
  queueEntry = over priorityQueue (OrdPSQ.insert startStmtId (Distance 0) ())

runDijkstrasFromEntry ::
  Some.Some CCC.AnyCFG ->
  DistanceMonad DijkstraState (Maybe Distance) ->
  DistanceMonad DijkstraCaches (Maybe Distance)
runDijkstrasFromEntry cfg djikstasRun =
  let sId = cfgEntryGlobalStmtId cfg
   in runDijkstrasFromStmt cfg (localStmtId sId) djikstasRun

data ReturnResolutionInfo = ReturnResolutionInfo
  {returnResolver :: ResolveReturn, returnCfgBuidler :: GetCFG, returnCallstack :: CallStack}
newtype ReturnHandler = ReturnHandler (Maybe ReturnResolutionInfo)

-- | Compute the minimum distance from a statement to a target.
--
-- Based on Dijkstra\'s algorithm.
computeDistance ::
  ScreachLogAction ->
  Some.Some CCC.AnyCFG ->
  LocalStmtId ->
  IsTarget ->
  ReturnHandler ->
  ResolveCall ->
  DistanceMonad DijkstraCaches (Maybe Distance)
computeDistance sla cfg startId tgts@(IsTarget isTarget) retHandler resolveCall =
  cachedDistanceOrRun
    (globalStmtId cfg startId)
    targetDistCache
    cfg
    ( do
        let cLoc = getCFGLoc cfg
        _ <-
          liftIO $ logDebug sla ("computing min dist for cfg: " ++ show cLoc ++ " sId: " ++ show startId)
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
                    allStmts
                      cfg
                      ( \sId _ loc stmt ->
                          Maybe.maybeToList $ (sId,) <$> isTarget fentry loc stmt
                      )
                )
                mbFunEntry
        _ <- liftIO $ logDebug sla ("Target list: " ++ show tgtList)
        let dist = dijkstras tgtList cfg (callDistResolverFromResolveCall sla resolveCall) currDistReslver
        r <- runDijkstrasFromStmt cfg startId dist
        _ <- liftIO $ logDebug sla ("done computed min dist for cfg: " ++ show cLoc)
        pure r
    )

collectReturnTargets :: Some.Some CCC.AnyCFG -> [(LocalStmtId, TargetType)]
collectReturnTargets cfg = allStmts cfg $ \sId _ _ stmt ->
  case stmt of
    Left (CCC.Return _) -> [(sId, IntraTarget)]
    Left (CCC.TailCall{}) -> [(sId, InterTarget)]
    _ -> []

minReturnDistResolver ::
  ScreachLogAction ->
  Some.Some CCC.AnyCFG ->
  ResolveCall ->
  FunctionEntry ->
  LocalStmtId ->
  DistanceMonad DijkstraCaches (Maybe Distance)
minReturnDistResolver sla cfg rCall fentry callingId =
  withStatement callingId cfg $ \_ loc stmt ->
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
        CCC.TermStmt cLoc _ -> cLoc
        CCC.ConsStmt cLoc _ _ -> cLoc

-- | Computes the minimum interprocedural distance to a return. We discard the target cache because we should only impact the
-- return dist cache
computeMinimumReturnDistance ::
  ScreachLogAction ->
  Some.Some CCC.AnyCFG ->
  ResolveCall ->
  DistanceMonad DijkstraCaches (Maybe Distance)
computeMinimumReturnDistance sla cfg rcall =
  cachedDistanceOrRun
    (cfgEntryGlobalStmtId cfg)
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
