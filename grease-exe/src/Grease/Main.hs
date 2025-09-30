{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Main (
  main,
  simulateLlvm,
  simulateLlvmSyntax,
  Results (..),
) where

import Control.Applicative (pure)
import Control.Concurrent.Async (Async, cancel)
import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens (to, (.~), (^.))
import Control.Monad
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (Either (..))
import Data.ElfEdit qualified as Elf
import Data.Eq ((==))
import Data.Foldable (traverse_)
import Data.Function
import Data.Functor
import Data.Functor.Const (Const (..))
import Data.Functor.Const qualified as Const
import Data.IntMap qualified as IntMap
import Data.LLVM.BitCode (parseBitCodeFromFileWithWarnings)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Memory.Lazy qualified as Symbolic
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord ((<=))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Parameterized.TraversableFC (traverseFC)
import Data.Parameterized.TraversableFC.WithIndex (imapFC)
import Data.Semigroup ((<>))
import Data.Sequence qualified as Seq
import Data.String (String)
import Data.Text qualified as Text
import Data.Traversable (for, traverse)
import Data.Traversable.WithIndex (iforM)
import Data.Tuple
import Data.Type.Equality (testEquality, (:~:) (Refl), type (~))
import Data.Vector qualified as Vec
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic
import Grease.Entrypoint
import Grease.ExecutionFeatures (greaseExecFeats)
import Grease.Heuristic
import Grease.LLVM qualified as LLVM
import Grease.LLVM.DebugInfo qualified as GLD
import Grease.LLVM.Overrides.SExp qualified as GLOS
import Grease.LLVM.SetupHook qualified as LLVM (SetupHook, moduleSetupHook, syntaxSetupHook)
import Grease.LLVM.SetupHook.Diagnostic qualified as LDiag (Diagnostic (LLVMTranslationWarning))
import Grease.Macaw
import Grease.Macaw.Arch
import Grease.Macaw.Dwarf (loadDwarfPreconditions)
import Grease.Macaw.Overrides.Address (AddressOverrides, loadAddressOverrides)
import Grease.Main.Diagnostic qualified as Diag
import Grease.MustFail qualified as MustFail
import Grease.Options
import Grease.Output
import Grease.Profiler.Feature (greaseProfilerFeature)
import Grease.Refine
import Grease.Setup
import Grease.Shape (ArgShapes (..), ExtShape, minimalShapeWithPtrs)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Simple qualified as Simple
import Grease.Solver (withSolverOnlineBackend)
import Grease.SymIO qualified as GSIO
import Grease.Syntax
import Grease.Time (time)
import Grease.Utility
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM qualified as CLLVM
import Lang.Crucible.LLVM.DataLayout (DataLayout)
import Lang.Crucible.LLVM.DataLayout qualified as DataLayout
import Lang.Crucible.LLVM.Debug qualified as Debug
import Lang.Crucible.LLVM.Extension qualified as CLLVM
import Lang.Crucible.LLVM.Globals qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Syntax (emptyParserHooks, llvmParserHooks)
import Lang.Crucible.LLVM.Translation qualified as Trans
import Lang.Crucible.LLVM.TypeContext qualified as TCtx
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Directory (Permissions)
import System.FilePath (FilePath)
import System.IO (IO)
import Text.LLVM qualified as L
import Text.Show (Show (..))
import What4.Expr qualified as W4
import What4.FunctionName qualified as W4
import What4.Protocol.Online qualified as W4
import Prelude (Int, Integral, Num (..), fromIntegral, undefined)

-- | Results of analysis, one per given 'Entrypoint'
newtype Results = Results {getResults :: Map Entrypoint Batch}

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (MainDiagnostic diag)

-- | Read a binary's file permissions and ELF header info.
readElfHeaderInfo ::
  ( MonadIO m
  , MonadThrow m
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  ) =>
  proxy arch ->
  FilePath ->
  m (Permissions, Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
readElfHeaderInfo _proxy path = undefined

-- Helper, not exported
--
-- Throw an exception if a user provides an entrypoint without a name in a
-- context where only named entrypoints make sense (e.g., when simulating an
-- S-expression program).
nonSymbolEntrypointException :: IO a
nonSymbolEntrypointException =
  throw $ GreaseException "Must provide entrypoint name"

textBounds ::
  Num (Elf.ElfWordType w) =>
  MML.LoadOptions ->
  Elf.ElfHeaderInfo w ->
  IO (Elf.ElfWordType w, Elf.ElfWordType w)
textBounds loadOpts elf =
  case Elf.headerNamedShdrs elf of
    Left (si, err) -> throw . GreaseException $ "Failed to lookup ELF section header at index " <> tshow si <> ": " <> tshow err
    Right shdrs ->
      case fmap NE.head . NE.nonEmpty . List.filter (\s -> Elf.shdrName s == ".text") $ Vec.toList shdrs of
        Nothing -> throw $ GreaseException "Could not find .text segment"
        Just s ->
          let loadOffset = fromIntegral $ fromMaybe 0 (MML.loadOffset loadOpts)
           in let sWithOffset = Elf.shdrAddr s + loadOffset
               in pure (sWithOffset, sWithOffset + Elf.shdrSize s)

-- | Define @?memOpts@ in a continuation, setting the 'Mem.laxLoadsAndStores'
-- option according to whether the user set the @--rust@ flag.
withMemOptions :: SimOpts -> ((?memOpts :: Mem.MemOptions) => r) -> r
withMemOptions _ _ = undefined

loadInitialPreconditions ::
  ExtShape ext ~ PtrShape ext w =>
  Mem.HasPtrWidth w =>
  MM.MemWidth w =>
  GreaseLogAction ->
  Maybe FilePath ->
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  Shape.ArgShapes ext NoTag tys ->
  IO (Shape.ArgShapes ext NoTag tys)
loadInitialPreconditions la preconds names initArgs = undefined

-- | Override 'Shape.ArgShapes' using 'Simple.SimpleShape's from the CLI
useSimpleShapes ::
  ExtShape ext ~ PtrShape ext w =>
  Mem.HasPtrWidth w =>
  MM.MemWidth w =>
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  Shape.ArgShapes ext NoTag tys ->
  Map Text.Text Simple.SimpleShape ->
  IO (Shape.ArgShapes ext NoTag tys)
useSimpleShapes argNames initArgs simpleShapes = undefined

toBatchBug ::
  Mem.HasPtrWidth wptr =>
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  W4.FloatModeRepr fm ->
  MC.AddrWidthRepr wptr ->
  Ctx.Assignment (Const String) args ->
  Ctx.Assignment C.TypeRepr args ->
  Shape.ArgShapes ext NoTag args ->
  Bug.BugInstance ->
  ConcretizedData sym ext args ->
  BatchBug
toBatchBug = undefined

toFailedPredicate ::
  Mem.HasPtrWidth wptr =>
  (sym ~ W4.ExprBuilder scope st (W4.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  W4.FloatModeRepr fm ->
  MC.AddrWidthRepr wptr ->
  Ctx.Assignment (Const String) args ->
  Ctx.Assignment C.TypeRepr args ->
  Shape.ArgShapes ext NoTag args ->
  NoHeuristic sym ext args ->
  FailedPredicate
toFailedPredicate = undefined

checkMustFail ::
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st (W4.Flags fm)
  , bak ~ C.OnlineBackend solver scope st (W4.Flags fm)
  , W4.OnlineSolver solver
  ) =>
  bak ->
  NE.NonEmpty (NoHeuristic sym ext tys) ->
  IO (Maybe BatchBug)
checkMustFail bak errs = do
  let noHeuristicPreds =
        NE.toList (NE.map (\err -> (noHeuristicGoal err, noHeuristicError err)) errs)
  mustFail <- MustFail.checkOneMustFail bak noHeuristicPreds
  let bug =
        Bug.BugInstance
          { Bug.bugType = Bug.OneMustFail
          , Bug.bugLoc = "<multiple locations>"
          , Bug.bugDetails =
              let goals = NE.toList (NE.map noHeuristicGoal errs)
                  print o =
                    let lp = C.proofGoal o
                     in let simError = lp ^. C.labeledPredMsg
                         in tshow (C.ppSimError simError)
               in Just $ Text.unlines (List.map print goals)
          , Bug.bugUb = Nothing -- TODO: check if they are all the same?
          }
  if mustFail
    -- TODO: concretized args
    then pure (Just (MkBatchBug{bugDesc = bug, bugArgs = [], bugShapes = ""}))
    else pure Nothing

-- | Machine code-specific arguments that are used throughout 'simulateMacawCfg'
-- and friends. This exists primarily to reduce the number of distinct arguments
-- we need to pass to these functions. Generally speaking, 'simulateMacawSyntax'
-- fills the fields of this data type with default or uninteresting values, as
-- S-expression programs do not require analyzing machine code.
data MacawCfgConfig arch = MacawCfgConfig
  { mcDataLayout :: DataLayout
  -- ^ The data layout to use in the underlying memory model.
  , mcMprotectAddr :: Maybe (MC.ArchSegmentOff arch)
  -- ^ 'Just' the address of @mprotect@ if it exists, and 'Nothing' otherwise.
  , mcLoadOptions :: MML.LoadOptions
  -- ^ The load options used to load addresses in the binary.
  , mcSymMap :: Discovery.AddrSymMap (MC.ArchAddrWidth arch)
  -- ^ Map of entrypoint addresses to their names.
  , mcPltStubs :: Map.Map (MC.ArchSegmentOff arch) W4.FunctionName
  -- ^ Map of addresses to PLT stub names.
  , mcDynFunMap :: Map.Map W4.FunctionName (MC.ArchSegmentOff arch)
  -- ^ Map of dynamic function names to their addresses.
  , mcRelocs :: Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (ArchReloc arch)
  -- ^ Map of relocation addresses and types.
  , mcMemory :: MC.Memory (MC.RegAddrWidth (MC.ArchReg arch))
  -- ^ The memory layout of the binary.
  , mcTxtBounds :: (Elf.ElfWordType (MC.ArchAddrWidth arch), Elf.ElfWordType (MC.ArchAddrWidth arch))
  -- ^ Bounds on the @.text@ segment's addresses.
  , mcElf :: Maybe (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  -- ^ 'Just' the ELF of the target binary if it exists, and 'Nothing' otherwise
  }

-- | Create a 'DataLayout' suitable for @macaw-symbolic@'s needs. Currently,
-- this simply overrides the 'DataLayout.defaultDataLayout' with a reasonable
-- endianness value based on the architecture.
macawDataLayout :: ArchContext arch -> DataLayout
macawDataLayout archCtx =
  DataLayout.defaultDataLayout
    & DataLayout.intLayout
      .~ (archCtx ^. archInfo . to (Symbolic.toCrucibleEndian . MI.archEndianness))

-- | Compute the initial 'ArgShapes' for a Macaw CFG.
--
-- Sources argument shapes from:
--
-- 1. A default initial shape for each register (via 'minimalArgShapes')
-- 2. DWARF debug info (via 'loadDwarfPreconditions') if
--    'initPrecondUseDebugInfo' is 'True'
-- 3. A shape DSL file (via 'loadInitialPreconditions')
-- 4. Simple shapes from the CLI (via 'useSimpleShapes')
--
-- Later steps override earlier ones.
macawInitArgShapes ::
  ( C.IsSymBackend sym bak
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , Mem.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GreaseLogAction ->
  bak ->
  ArchContext arch ->
  InitialPreconditionOpts ->
  MacawCfgConfig arch ->
  Ctx.Assignment (Const String) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  IO (ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)))
macawInitArgShapes la bak archCtx opts macawCfgConfig argNames mbCfgAddr = do
  let memory = mcMemory macawCfgConfig
  let mdEntryAbsAddr = fmap (segoffToAbsoluteAddr memory) mbCfgAddr
  initArgs0 <- minimalArgShapes bak archCtx mdEntryAbsAddr
  let shouldUseDwarf = initPrecondUseDebugInfo opts
  let getDwarfArgs = do
        -- Maybe
        elfHdr <- mcElf macawCfgConfig
        addr <- mbCfgAddr
        loadDwarfPreconditions
          addr
          memory
          (initPrecondTypeUnrollingBound opts)
          argNames
          initArgs0
          elfHdr
          archCtx
  let dwarfedArgs = if shouldUseDwarf then fromMaybe initArgs0 getDwarfArgs else initArgs0
  initArgs1 <-
    loadInitialPreconditions la (initPrecondPath opts) argNames dwarfedArgs
  useSimpleShapes argNames initArgs1 (initPrecondSimpleShapes opts)

llvmExecFeats ::
  forall p sym bak ext ret solver scope st fm.
  ( OnlineSolverAndBackend solver sym bak scope st (W4.Flags fm)
  , ext ~ CLLVM.LLVM
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  GreaseLogAction ->
  bak ->
  SimOpts ->
  C.GlobalVar Mem.Mem ->
  C.TypeRepr ret ->
  IO ([C.ExecutionFeature p sym ext (C.RegEntry sym ret)], Maybe (Async ()))
llvmExecFeats la bak simOpts memVar ret = do
  profFeatLog <- traverse greaseProfilerFeature (simProfileTo simOpts)
  let dbgOpts =
        if simDebug simOpts
          then
            Just
              ( Debug.llvmCommandExt
              , Debug.llvmExtImpl memVar
              , ret
              )
          else Nothing
  feats <- greaseExecFeats la bak dbgOpts (simBoundsOpts simOpts)
  pure (feats, snd <$> profFeatLog)

toSsaAnyCfg ::
  C.IsSyntaxExtension ext =>
  C.Reg.AnyCFG ext ->
  C.AnyCFG ext
toSsaAnyCfg (C.Reg.AnyCFG cfg) =
  case C.toSSA cfg of C.SomeCFG ssa -> C.AnyCFG ssa

entrypointCfgMap ::
  ( C.IsSyntaxExtension ext
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  CSyn.ParsedProgram ext ->
  [Entrypoint] ->
  IO (Map Entrypoint (EntrypointCfgs (C.Reg.AnyCFG ext)))
entrypointCfgMap la halloc prog entries =
  if List.null entries
    then do
      doLog la Diag.NoEntrypoints
      pure
        $ Map.map
          ( \cfg ->
              EntrypointCfgs
                { entrypointStartupOv = Nothing
                , entrypointCfg = cfg
                }
          )
        $ Map.mapKeys
          (entrypointNoStartupOv . EntrypointSymbolName)
          cfgs
    else do
      results <-
        forM entries $ \e ->
          case entrypointLocation e of
            EntrypointAddress{} -> nonSymbolEntrypointException
            EntrypointCoreDump{} -> nonSymbolEntrypointException
            EntrypointSymbolName nm ->
              case Map.lookup nm cfgs of
                Just cfg -> do
                  mbStartupOv <-
                    traverse
                      (parseEntrypointStartupOv halloc)
                      (entrypointStartupOvPath e)
                  let entrypointCfgs =
                        EntrypointCfgs
                          { entrypointStartupOv = mbStartupOv
                          , entrypointCfg = cfg
                          }
                  pure (e, entrypointCfgs)
                Nothing -> throw $ GreaseException $ "Could not find function: " <> nm
      pure (Map.fromList results)
 where
  cfgs = parsedProgramCfgMap prog

analyzeEntrypoint ::
  GreaseLogAction ->
  Entrypoint ->
  -- | Index of current entrypoint
  Int ->
  -- | Total number of entrypoints
  Int ->
  IO a ->
  IO a
analyzeEntrypoint la entry current total act = do
  Monad.unless (total <= 1) $
    doLog la (Diag.AnalyzingEntrypoint entry current total)
  (duration, a) <- time act
  doLog la (Diag.FinishedAnalyzingEntrypoint (entrypointLocation entry) duration)
  pure a

loadAddrOvs ::
  ( Symbolic.SymArchConstraints arch
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  ArchContext arch ->
  C.HandleAllocator ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  SimOpts ->
  IO (AddressOverrides arch)
loadAddrOvs archCtx halloc memory simOpts = do
  mbAddrOvs <- loadAddressOverrides (regStructRepr archCtx) halloc memory (simAddressOverrides simOpts)
  case mbAddrOvs of
    Left err -> do
      let msg = PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions (PP.pretty err))
      throw (GreaseException ("user error: " <> msg))
    Right addrOvs -> pure addrOvs

loadLLVMSExpOvs ::
  Mem.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Seq.Seq (W4.FunctionName, GLOS.LLVMSExpOverride))
loadLLVMSExpOvs sexpOvPaths halloc mvar = do
  mbOvs <- liftIO (GLOS.loadOverrides sexpOvPaths halloc mvar)
  case mbOvs of
    Left err -> do
      let msg = PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions (PP.pretty err))
      throw (GreaseException ("user error: " <> msg))
    Right ovs -> pure ovs

llvmInitArgShapes ::
  Mem.HasPtrWidth 64 =>
  GreaseLogAction ->
  InitialPreconditionOpts ->
  Maybe L.Module ->
  Ctx.Assignment (Const String) argTys ->
  -- | The CFG of the user-requested entrypoint function.
  C.CFG CLLVM.LLVM blocks argTys ret ->
  IO (ArgShapes CLLVM.LLVM NoTag argTys)
llvmInitArgShapes la opts llvmMod argNames cfg = do
  let argTys = C.cfgArgTypes cfg
  initArgs0 <-
    case llvmMod of
      Just m
        | initPrecondUseDebugInfo opts ->
            GLD.diArgShapes (C.handleName (C.cfgHandle cfg)) argTys m
      _ -> traverseFC (minimalShapeWithPtrs (pure . const NoTag)) argTys
  initArgs1 <-
    let path = initPrecondPath opts
     in loadInitialPreconditions la path argNames (ArgShapes initArgs0)
  useSimpleShapes argNames initArgs1 (initPrecondSimpleShapes opts)

simulateLlvmCfg ::
  forall sym bak arch solver t st fm argTys ret.
  ( Mem.HasPtrWidth (CLLVM.ArchWidth arch)
  , OnlineSolverAndBackend solver sym bak t st (W4.Flags fm)
  , ?parserHooks :: CSyn.ParserHooks CLLVM.LLVM
  ) =>
  GreaseLogAction ->
  SimOpts ->
  bak ->
  W4.FloatModeRepr fm ->
  C.HandleAllocator ->
  Trans.LLVMContext arch ->
  Maybe L.Module ->
  InitialMem sym ->
  LLVM.SetupHook sym arch ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG CLLVM.LLVM argTys (C.StructType argTys)) ->
  -- | The CFG of the user-requested entrypoint function.
  C.SomeCFG CLLVM.LLVM argTys ret ->
  IO BatchStatus
simulateLlvmCfg la simOpts bak fm halloc llvmCtx llvmMod initMem setupHook mbStartupOvCfg scfg@(C.SomeCFG cfg) = do
  doLog la (Diag.TargetCFG cfg)

  let memVar = Trans.llvmMemVar llvmCtx
  (execFeats, profLogTask) <-
    llvmExecFeats @(C.GlobalVar ToConc.ToConcretizeType) la bak simOpts memVar (C.cfgReturnType cfg)

  C.Refl <-
    case C.testEquality ?ptrWidth (knownNat @64) of
      Just r -> pure r
      Nothing -> throw $ GreaseException "Bad pointer width"

  let argTys = C.cfgArgTypes cfg
      -- Display the arguments as though they are unnamed LLVM virtual registers.
      argNames = imapFC (\i _ -> Const ('%' : show i)) argTys
  initArgShapes <-
    let opts = simInitPrecondOpts simOpts
     in llvmInitArgShapes la opts llvmMod argNames cfg

  let ?recordLLVMAnnotation = \_ _ _ -> pure ()
  let bounds = simBoundsOpts simOpts
  result <- withMemOptions simOpts $ do
    let valueNames = Ctx.generate (Ctx.size argTys) (\i -> ValueName ("arg" <> show i))
    let typeCtx = llvmCtx ^. Trans.llvmTypeCtx
    let dl = TCtx.llvmDataLayout typeCtx
    -- See comment above on heuristics in 'simulateMacawCfg'
    let heuristics =
          if simNoHeuristics simOpts
            then [mustFailHeuristic]
            else llvmHeuristics la List.++ [mustFailHeuristic]
    refinementLoop la bounds argNames initArgShapes $ \argShapes bbMapRef ->
      refineOnce
        la
        simOpts
        halloc
        bak
        fm
        dl
        valueNames
        argNames
        argTys
        argShapes
        initMem
        memVar
        bbMapRef
        heuristics
        execFeats
        $ \p setupMem initFs args ->
          LLVM.initState
            bak
            la
            (CLLVM.llvmExtensionImpl ?memOpts)
            p
            halloc
            (simErrorSymbolicFunCalls simOpts)
            setupMem
            -- TODO: just take the whole initFs
            (GSIO.initFs initFs)
            (GSIO.initFsGlobals initFs)
            (GSIO.initFsOverride initFs)
            llvmCtx
            setupHook
            (argVals args)
            mbStartupOvCfg
            scfg

  res <- case result of
    RefinementBug b cData ->
      pure (BatchBug (toBatchBug fm MM.Addr64 argNames argTys initArgShapes b cData))
    RefinementCantRefine b ->
      pure (BatchCantRefine b)
    RefinementItersExceeded ->
      pure BatchItersExceeded
    RefinementNoHeuristic errs -> do
      maybeBug <- checkMustFail bak errs
      case maybeBug of
        Just bug -> pure (BatchBug bug)
        Nothing ->
          pure $
            BatchCouldNotInfer $
              errs <&> \noHeuristic ->
                toFailedPredicate fm MM.Addr64 argNames argTys initArgShapes noHeuristic
    RefinementSuccess _argShapes -> pure (BatchChecks Map.empty)
  traverse_ cancel profLogTask
  pure res

simulateLlvmCfgs ::
  Mem.HasPtrWidth (CLLVM.ArchWidth arch) =>
  (?parserHooks :: CSyn.ParserHooks CLLVM.LLVM) =>
  GreaseLogAction ->
  SimOpts ->
  C.HandleAllocator ->
  Trans.LLVMContext arch ->
  Maybe L.Module ->
  (forall sym bak. C.IsSymBackend sym bak => bak -> IO (InitialMem sym)) ->
  (forall sym. LLVM.SetupHook sym arch) ->
  Map Entrypoint (EntrypointCfgs (C.AnyCFG CLLVM.LLVM)) ->
  IO Results
simulateLlvmCfgs la simOpts halloc llvmCtx llvmMod mkMem setupHook cfgs = do
  let fm = W4.FloatRealRepr
  withSolverOnlineBackend (simSolver simOpts) fm globalNonceGenerator $ \bak -> do
    initMem <- mkMem bak
    results <- do
      let nEntries = Map.size cfgs
      iforM (Map.toList cfgs) $ \i (entry, entrypointCfgs) ->
        analyzeEntrypoint la entry i nEntries $ do
          EntrypointCfgs
            { entrypointStartupOv = mbStartupOv
            , entrypointCfg = C.AnyCFG entrypointCfg'
            } <-
            pure entrypointCfgs
          mbStartupOvSomeCfg <-
            for (startupOvCfg <$> mbStartupOv) $ \(C.AnyCFG startupOvCfg') -> do
              let expectedArgTys = C.cfgArgTypes entrypointCfg'
              let expectedRetTy = C.StructRepr expectedArgTys
              let actualArgTys = C.cfgArgTypes startupOvCfg'
              let actualRetTy = C.cfgReturnType startupOvCfg'
              Refl <-
                case testEquality expectedArgTys actualArgTys of
                  Just r -> pure r
                  Nothing ->
                    throw $
                      GreaseException $
                        Text.unlines
                          [ "Startup override must have the same argument types as the entrypoint function"
                          , "Entrypoint function argument types: " <> Text.pack (show expectedArgTys)
                          , "Startup override argument types: " <> Text.pack (show actualArgTys)
                          ]
              Refl <-
                case testEquality expectedRetTy actualRetTy of
                  Just r -> pure r
                  Nothing ->
                    throw $
                      GreaseException $
                        Text.unlines
                          [ "Startup override must return a struct containing the argument types of the entrypoint function"
                          , "Entrypoint function argument types: " <> Text.pack (show expectedArgTys)
                          , "Startup override return type: " <> Text.pack (show actualRetTy)
                          ]
              pure $ C.SomeCFG startupOvCfg'
          status <- simulateLlvmCfg la simOpts bak fm halloc llvmCtx llvmMod initMem setupHook mbStartupOvSomeCfg (C.SomeCFG entrypointCfg')
          let result =
                Batch
                  { batchStatus = status
                  , batchLoadOffset = 0x0
                  }
          pure (entry, result)

    pure (Results (Map.fromList results))

simulateLlvmSyntax ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateLlvmSyntax simOpts la = do
  halloc <- C.newHandleAllocator
  mvar <- Mem.mkMemVar "grease:memmodel" halloc
  let mkMem = \_ -> InitialMem <$> Mem.emptyMem DataLayout.LittleEndian
  let ?ptrWidth = knownNat @64
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  prog <- parseProgram halloc (simProgPath simOpts)
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  regCfgs <- entrypointCfgMap la halloc prog (simEntryPoints simOpts)
  let cfgs = Map.map (fmap toSsaAnyCfg) regCfgs
  let dl = DataLayout.defaultDataLayout
  let (_errs, tyCtx) = TCtx.mkTypeContext dl IntMap.empty []
  let llvmCtx =
        Trans.LLVMContext
          { Trans.llvmArch = CLLVM.X86Repr ?ptrWidth
          , Trans.llvmPtrWidth = \k -> k ?ptrWidth
          , Trans.llvmMemVar = mvar
          , Trans._llvmTypeCtx = tyCtx
          , Trans.llvmGlobalAliases = Map.empty
          , Trans.llvmFunctionAliases = Map.empty
          }
  sexpOvs <- loadLLVMSExpOvs (simOverrides simOpts) halloc mvar
  let llvmMod = Nothing
  let setupHook :: forall sym arch. LLVM.SetupHook sym arch
      setupHook = LLVM.syntaxSetupHook la sexpOvs prog cfgs
  simulateLlvmCfgs la simOpts halloc llvmCtx llvmMod mkMem setupHook cfgs

-- | Helper, not exported
--
-- Parse bitcode from a 'FilePath', logging any parse warnings and throwing
-- 'GreaseException' on error.
parseBitcode :: GreaseLogAction -> FilePath -> IO L.Module
parseBitcode la path =
  parseBitCodeFromFileWithWarnings path >>= \case
    Left _err -> throw $ GreaseException "Could not parse LLVM module"
    Right (m, warns) -> do
      Monad.unless (Seq.null warns) $
        doLog la (Diag.BitcodeParseWarnings warns)
      pure m

simulateLlvm ::
  Trans.TranslationOptions ->
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateLlvm transOpts simOpts la = do
  llvmMod <- parseBitcode la (simProgPath simOpts)
  halloc <- C.newHandleAllocator
  mvar <- liftIO $ Mem.mkMemVar "grease:memmodel" halloc
  C.Some @_ @_ @arch trans <- do
    let ?transOpts = transOpts
    Trans.translateModule halloc mvar llvmMod

  entries <-
    if List.null (simEntryPoints simOpts)
      then do
        doLog la Diag.NoEntrypoints
        let convertSymbol (L.Symbol s) =
              entrypointNoStartupOv $ EntrypointSymbolName $ Text.pack s
        pure (List.map (convertSymbol . L.defName) (L.modDefines llvmMod))
      else pure (simEntryPoints simOpts)

  let llvmCtxt = trans ^. Trans.transContext
  Trans.llvmPtrWidth llvmCtxt $ \ptrW -> Mem.withPtrWidth ptrW $ do
    let mkMem :: forall sym bak. C.IsSymBackend sym bak => bak -> IO (InitialMem sym)
        mkMem bak =
          let ?lc = llvmCtxt ^. Trans.llvmTypeCtx
              ?recordLLVMAnnotation = \_ _ _ -> pure ()
           in withMemOptions simOpts $ do
                unpopulated <- CLLVM.initializeAllMemory bak llvmCtxt llvmMod
                initMem <-
                  case simMutGlobs simOpts of
                    Initialized ->
                      CLLVM.populateAllGlobals bak (trans ^. Trans.globalInitMap) unpopulated
                    Symbolic ->
                      throw (GreaseException "GREASE does not yet support symbolic globals for LLVM")
                    Uninitialized ->
                      CLLVM.populateConstGlobals bak (trans ^. Trans.globalInitMap) unpopulated
                pure (InitialMem initMem)

    let ?parserHooks = llvmParserHooks emptyParserHooks mvar
    cfgs <-
      fmap Map.fromList $
        forM entries $ \entry -> do
          case entrypointLocation entry of
            EntrypointAddress{} -> nonSymbolEntrypointException
            EntrypointCoreDump{} -> nonSymbolEntrypointException
            EntrypointSymbolName nm ->
              Trans.getTranslatedCFG trans (L.Symbol (Text.unpack nm)) >>= \case
                Just (_decl, cfg, warns) -> do
                  forM_ warns $ \warn ->
                    LJ.writeLog la (LLVMSetupHookDiagnostic (LDiag.LLVMTranslationWarning warn))
                  mbStartupOv <-
                    traverse
                      (parseEntrypointStartupOv halloc)
                      (entrypointStartupOvPath entry)
                  let mbStartupOvSsa = fmap toSsaAnyCfg <$> mbStartupOv
                  let entrypointCfgs =
                        EntrypointCfgs
                          { entrypointStartupOv = mbStartupOvSsa
                          , entrypointCfg = cfg
                          }
                  pure (entry, entrypointCfgs)
                Nothing -> throw $ GreaseException $ "Could not find function: " <> nm

    sexpOvs <- loadLLVMSExpOvs (simOverrides simOpts) halloc mvar
    let setupHook :: forall sym. LLVM.SetupHook sym arch
        setupHook = LLVM.moduleSetupHook la sexpOvs trans cfgs

    simulateLlvmCfgs la simOpts halloc llvmCtxt (Just llvmMod) mkMem setupHook cfgs

main :: IO ()
main = pure ()
