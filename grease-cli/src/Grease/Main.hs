{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Main
  ( main
  , simulateARM
  , simulateARMSyntax
  , simulatePPC32
  , simulatePPC32Syntax
  , simulatePPC64
  , simulatePPC64Syntax
  , simulateX86
  , simulateX86Syntax
  , simulateLlvm
  , simulateLlvmSyntax
  , simulateFile
  , Results(..)
  , logResults
  ) where

import System.IO (IO)
import System.FilePath (FilePath)

import Prelude (Num(..), fromIntegral, Integral)

import Control.Applicative (pure)
import Control.Concurrent.Async (cancel)
import Control.Lens ((^.), (.~), to)
import Control.Monad ((>>=), forM, forM_, mapM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception.Safe (MonadThrow, Handler(..), catches, throw)

import qualified Data.Aeson as Aeson
import Data.Bool (Bool(..), (&&), otherwise, not)
import qualified Data.ByteString as BS
import Data.Eq ((==))
import Data.Foldable (traverse_)
import Data.Function (($), (.), (&), id, const)
import Data.Functor ((<$>), fmap, (<&>))
import qualified Data.Functor.Const as Const
import Data.Functor.Const (Const(..))
import Data.IORef (newIORef, modifyIORef)
import Data.Ord ((<=))
import Data.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, catMaybes)
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (Either(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.String (String)
import Data.Text.IO (putStrLn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Traversable (for, traverse)
import Data.Tuple (fst, snd)
import Data.Type.Equality (type (~), (:~:)(Refl), testEquality)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import qualified Data.Vector as Vec
import qualified Prettyprinter as PP
import System.Directory (Permissions, getPermissions)

import qualified Lumberjack as LJ

import Text.Show (Show(..))

import Data.LLVM.BitCode (parseBitCodeFromFile)
import qualified Text.LLVM as L

-- parameterized-utils
import Data.Parameterized.Classes (IxedF'(ixF'))
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
import Data.Parameterized.TraversableFC.WithIndex (imapFC)

-- what4
import qualified What4.Interface as W4
import qualified What4.FunctionName as W4
import qualified What4.ProgramLoc as W4
import qualified What4.Protocol.Online as W4
import qualified What4.Expr as W4

-- crucible
import qualified Lang.Crucible.Analysis.Postdom as C (postdomInfo)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Backend.Online as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Reg as C.Reg
import qualified Lang.Crucible.CFG.Extension as C
import qualified Lang.Crucible.CFG.SSAConversion as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import qualified Lang.Crucible.Simulator.SimError as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM as CLLVM
import           Lang.Crucible.LLVM.DataLayout (DataLayout)
import qualified Lang.Crucible.LLVM.DataLayout as DataLayout
import qualified Lang.Crucible.LLVM.Debug as Debug
import qualified Lang.Crucible.LLVM.Extension as CLLVM
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as Mem
import qualified Lang.Crucible.LLVM.MemModel.Partial as Mem
import qualified Lang.Crucible.LLVM.Globals as CLLVM
import qualified Lang.Crucible.LLVM.Translation as Trans
import qualified Lang.Crucible.LLVM.TypeContext as TCtx

-- crucible-llvm-syntax
import Lang.Crucible.LLVM.Syntax (llvmParserHooks, emptyParserHooks)

-- crucible-syntax
import qualified Lang.Crucible.Syntax.Concrete as CSyn
import qualified Lang.Crucible.Syntax.Prog as CSyn

-- elf-edit
import qualified Data.ElfEdit as Elf

-- macaw-loader
import qualified Data.Macaw.BinaryLoader as Loader

-- macaw-loader-aarch32
import Data.Macaw.BinaryLoader.AArch32 ()

-- macaw-loader-x86
import Data.Macaw.BinaryLoader.X86 ()

-- macaw-base
import qualified Data.Macaw.Architecture.Info as MI
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as Discovery
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader.PLTStubs as PLT
import qualified Data.Macaw.Memory.LoadCommon as MML
import Data.Macaw.BinaryLoader (BinaryLoader)

-- crucible-macaw-debug
import qualified Data.Macaw.Symbolic.Debug as MDebug

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- macaw-symbolic-syntax
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)

-- macaw-aarch32
import qualified Data.Macaw.ARM as ARM

-- macaw-aarch32-symbolic
import qualified Data.Macaw.AArch32.Symbolic as AArch32Symbolic

-- macaw-aarch32-syntax
import qualified Data.Macaw.AArch32.Symbolic.Syntax as AArch32Syn

-- macaw-ppc
import qualified Data.Macaw.PPC as PPC

-- macaw-ppc-symbolic
import qualified Data.Macaw.PPC.Symbolic as PPCSymbolic

-- macaw-ppc-syntax
import qualified Data.Macaw.PPC.Symbolic.Syntax as PPCSyn

-- macaw-x86
import qualified Data.Macaw.X86 as X86

-- macaw-x86-symbolic
import qualified Data.Macaw.X86.Crucible as X86Symbolic

-- macaw-x86-syntax
import qualified Data.Macaw.X86.Symbolic.Syntax as X86Syn

-- stubs-common
import qualified Stubs.FunctionOverride as Stubs

import Grease.AssertProperty
import Grease.BranchTracer (greaseBranchTracerFeature)
import qualified Grease.Bug as Bug
import Grease.Cli (optsFromArgs)
import Grease.Concretize (ConcArgs(..), concArgs, printConcArgs)
import Grease.Concretize.JSON (concArgsToJson)
import Grease.Cursor.Pointer ()
import qualified Lang.Crucible.Debug as Dbg
import Grease.Diagnostic
import Grease.Diagnostic.Severity (Severity)
import Grease.Entrypoint
import Grease.FunctionOverride (builtinLLVMOverrides, builtinStubsOverrides)
import Grease.Heuristic
import qualified Grease.LLVM as LLVM
import qualified Grease.LLVM.Overrides as LLVM
import qualified Grease.Macaw as Macaw (SetupHook(..))
import Grease.Macaw
import Grease.Macaw.Arch
import Grease.Macaw.Arch.AArch32 (armCtx)
import Grease.Macaw.Arch.PPC32 (ppc32Ctx)
import Grease.Macaw.Arch.PPC64 (ppc64Ctx)
import Grease.Macaw.Arch.X86 (x86Ctx)
import Grease.Macaw.Discovery (discoverFunction)
import qualified Grease.Macaw.FunctionOverride as Macaw
import Grease.Macaw.Load (LoadedProgram(..), load)
import Grease.Macaw.Load.Relocation (RelocType(..), elfRelocationMap)
import Grease.Macaw.PLT
import Grease.Macaw.SimulatorState (GreaseSimulatorState, discoveredFnHandles, emptyGreaseSimulatorState)
import Grease.Pretty (prettyPtrFnMap)
import Grease.Profiler.Feature (greaseProfilerFeature)
import Grease.Macaw.RegName (RegName(..), RegNames(..), regNames, getRegName, mkRegName, regNameToString)
import qualified Grease.MustFail as MustFail
import qualified Grease.Main.Diagnostic as Diag
import Grease.Options
import Grease.Output
import Grease.Refine
import Grease.Requirement
import Grease.Setup
import Grease.Shape (ArgShapes(..), minimalShapeWithPtrs, ExtShape)
import Grease.Shape.Concretize (concShape)
import qualified Grease.Shape as Shape
import Grease.Shape.NoTag (NoTag(NoTag))
import qualified Grease.Shape.Parse as Parse
import Grease.Shape.Pointer (PtrShape)
import Grease.Solver (withSolverOnlineBackend)
import Grease.Syntax (parseProgram, parsedProgramCfgMap)
import Grease.Syscall
import Grease.Time (time)
import Grease.Utility

-- | Results of analysis, one per given 'Entrypoint'
newtype Results = Results { getResults :: Map Entrypoint Batch }

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
readElfHeaderInfo _proxy path =
  do perms <- liftIO $ getPermissions path
     bs <- liftIO $ BS.readFile path
     case Elf.decodeElfHeaderInfo bs of
       Right (Elf.SomeElf hdr) ->
         case ( Elf.headerClass (Elf.header hdr)
              , C.testEquality ?ptrWidth (knownNat @32)
              , C.testEquality ?ptrWidth (knownNat @64)
              ) of
           (Elf.ELFCLASS32, Just Refl, Nothing) -> pure (perms, hdr)
           (Elf.ELFCLASS64, Nothing, Just Refl) -> pure (perms, hdr)
           _ -> throw $ GreaseException "Internal error: bad pointer width!"
       Left _ -> userError ("expected AArch32, PowerPC, or x86_64 ELF binary, but found non-ELF file at " <> Text.pack path)
  where userError msg = throw $ GreaseException ("User error: " <> msg)

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
          let loadOffset = fromIntegral $ fromMaybe 0 (MML.loadOffset loadOpts) in
          let sWithOffset = Elf.shdrAddr s + loadOffset in
          pure (sWithOffset, sWithOffset + Elf.shdrSize s)

-- | Define @?memOpts@ in a continuation, setting the 'Mem.laxLoadsAndStores'
-- option according to whether the user set the @--rust@ flag.
withMemOptions :: SimOpts -> ((?memOpts :: Mem.MemOptions) => r) -> r
withMemOptions opts k =
  let ?memOpts =
        Mem.defaultMemOptions
          { -- If @--rust@ is enabled, then we enable lax loads and stores so
            -- that reading from uninitialized memory returns symbolic data
            -- rather than failing. This is a common pattern observed with the
            -- LLVM code generated from Rust enums. For instance, constructing
            -- a @None@ value requires allocating space for a payload (but not
            -- writing to it), and reading the payload's memory later should not
            -- induce failure on its own. See gitlab#177.
            Mem.laxLoadsAndStores = simRust opts
            -- This option tells Crucible-LLVM to invent a fresh boolean
            -- constant to use as a proof obligation when certain reads fail.
            -- This interacts poorly with the must-fail heuristic, as this proof
            -- obligation semantically should always fail, but as it is a fresh
            -- constant, there is no way for the heuristic to see that.
            -- See gitlab#256.
          , Mem.noSatisfyingWriteFreshConstant = False
          }
  in k

loadInitialPreconditions ::
  ExtShape ext ~ PtrShape ext w =>
  Mem.HasPtrWidth w =>
  Maybe FilePath ->
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  Shape.ArgShapes ext NoTag tys ->
  IO (Shape.ArgShapes ext NoTag tys)
loadInitialPreconditions preconds names initArgs =
  case preconds of
    Nothing -> pure initArgs
    Just path -> do
      txt <- Text.IO.readFile path
      parsed <-
        case Parse.parseShapes path txt of
          Left err -> throw (GreaseException (Text.pack (show (PP.pretty err))))
          Right parsed -> pure parsed
      case Parse.replaceShapes names initArgs parsed of
        Left err -> throw (GreaseException (Text.pack (show (PP.pretty err))))
        Right shapes -> pure shapes


toFailedPredicate ::
  NoHeuristic sym ext tys ->
  [Aeson.Value] ->
  Text.Text ->
  FailedPredicate
toFailedPredicate (NoHeuristic goal _cData _err) argsJson concShapes =
  let lp = C.proofGoal goal
      simErr = lp ^. C.labeledPredMsg
      -- We inline and modify 'ppSimError', because we don't need to repeat the
      -- function name nor location.
      msg =
        tshow $
          PP.vcat $
            let details = C.simErrorDetailsMsg (C.simErrorReason simErr) in
            (PP.pretty (C.simErrorReasonMsg (C.simErrorReason simErr)) :)
            (if List.null details
            then []
            else [ "Details:", PP.indent 2 (PP.pretty details) ])
  in FailedPredicate
     { _failedPredicateLocation =
         tshow (PP.pretty (W4.plSourceLoc (C.simErrorLoc simErr)))
     , _failedPredicateMessage = msg
     , _failedPredicateArgs = argsJson
     , _failedPredicateConcShapes = concShapes
     }

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
                  let lp = C.proofGoal o in
                  let simError = lp ^. C.labeledPredMsg in
                  tshow (C.ppSimError simError)
            in Just $ Text.unlines (List.map print goals)
        , Bug.bugUb = Nothing  -- TODO: check if they are all the same?
        }
  if mustFail
  then pure (Just (MkBatchBug bug []))  -- TODO: concretized args
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

-- | Integer argument registers
interestingRegs :: Set String
interestingRegs =
  Set.fromList
  [ -- AArch32
    "R0"
  , "R1"
  , "R2"
  , "R3"
    -- PPC32
  , "r3"
  , "r4"
  , "r5"
  , "r6"
  , "r7"
  , "r8"
  , "r9"
  , "r10"
    -- x86_64
  , "rdi"
  , "rsi"
  , "rdx"
  , "rcx"
  , "r8"  -- also listed above in PPC32, but it's a set, so...
  , "r9"
  ]

-- | Filter out \"uninteresting\" concretized shapes
--
-- They are interesting when:
--
-- * They have a non-default value, and
-- * The name is in a known \"interesting\" set ('interestingRegs')
--
-- This is clearly a bit of a hack, but leads to concise and readable output.
interestingConcretizedShapes ::
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  -- | Default/initial/minimal shapes
  Ctx.Assignment (Shape.Shape ext tag) argTys ->
  ConcArgs sym ext argTys ->
  Ctx.Assignment (Const Bool) argTys
interestingConcretizedShapes names initArgs (ConcArgs cArgs) =
  let cShapes = fmapFC (Shape.tagWithType . concShape) cArgs in
  let initArgs' = fmapFC Shape.tagWithType initArgs in
  Ctx.zipWith
    (\(Const name) (Const isDefault) ->
      Const (name `List.elem` interestingRegs && not isDefault))
    names
    (Ctx.zipWith (\s s' -> Const (Maybe.isJust (testEquality s s'))) cShapes initArgs')

simulateMacawCfg ::
  forall sym bak arch solver scope st fm.
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder scope st (W4.Flags fm)
  , bak ~ C.OnlineBackend solver scope st (W4.Flags fm)
  , W4.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GreaseLogAction ->
  bak ->
  W4.FloatModeRepr fm ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  ArchContext arch ->
  SimOpts ->
  Macaw.SetupHook sym ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  -- | The entrypoint-related CFGs.
  EntrypointCfgs (C.Reg.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  IO BatchStatus
simulateMacawCfg la bak fm halloc macawCfgConfig archCtx simOpts setupHook mbCfgAddr entrypointCfgs = do
  let sym = C.backendGetSym bak
  let userOvPaths = simOverrides simOpts
  let errorSymbolicFunCalls = simErrorSymbolicFunCalls simOpts

  let ?recordLLVMAnnotation = \_ _ _ -> pure ()
  (initMem, memPtrTable) <- emptyMacawMem bak archCtx memory (simMutGlobs simOpts) relocs
  let ?ptrWidth = knownNat @(MC.ArchAddrWidth arch)

  let (tyCtxErrs, tyCtx) = TCtx.mkTypeContext dl IntMap.empty []
  let ?lc = tyCtx
  mapM_ (doLog la . Diag.TypeContextError) tyCtxErrs

  let regTypes = Symbolic.crucArchRegTypes (archCtx ^. archVals . to Symbolic.archFunctions)
  let rNames@(RegNames rNamesAssign) = regNames (archCtx ^. archVals)
      rNamesAssign' = fmapFC (\(Const (RegName n)) -> Const n) rNamesAssign
  let overrideRegs =
        Ctx.traverseWithIndex
          (\idx reg -> do
            let regName = getRegName rNames idx
            let isStackPointer = regName == mkRegName @arch MC.sp_reg
            case Map.lookup regName (archCtx ^. archRegOverrides) of
              Just i
                | isStackPointer ->
                    throw $ GreaseException "Can't override stack pointer"
                | Mem.LLVMPointerRepr w <- regTypes ^. ixF' idx
                , Just C.Refl <- C.testEquality w ?ptrWidth -> do
                    let macawGlobalMemBlock = 1 -- TODO: don't hardcode this
                    blk <- liftIO $ W4.natLit sym macawGlobalMemBlock
                    off <- liftIO (W4.bvLit sym ?ptrWidth i)
                    let ptr = Mem.LLVMPointer blk off
                    pure $ C.RV $ ptr
                | otherwise ->
                    throw $ GreaseException "Can't override non-pointer register"
              Nothing -> pure reg)

  profFeatLog <- traverse greaseProfilerFeature (simProfileTo simOpts)

  let cmdExt = MDebug.macawCommandExt (archCtx ^. archVals)
  debuggerFeat <-
    liftIO $
      if simDebug simOpts
      then do
        dbgInputs <- Dbg.defaultDebuggerInputs cmdExt
        let mbElf = snd . Elf.getElf <$> mcElf macawCfgConfig
        Just <$>
          Dbg.debugger
            cmdExt
            (MDebug.macawExtImpl prettyPtrFnMap (archCtx ^. archVals) mbElf)
            prettyPtrFnMap
            dbgInputs
            Dbg.defaultDebuggerOutputs
            (regStructRepr archCtx)
      else pure Nothing
  let execFeats =
        catMaybes
        [ fmap fst profFeatLog
        , debuggerFeat
        , Just (greaseBranchTracerFeature la)
        ]
  let memCfg0 = memConfigInitial bak archCtx memPtrTable relocs
  let rNameAssign =
        Ctx.generate
          (Ctx.size regTypes)
          (\idx -> ValueName (regNameToString (getRegName rNames idx)))

  let mdEntryAbsAddr = fmap (segoffToAbsoluteAddr memory) mbCfgAddr
  initArgs_ <- minimalArgShapes bak archCtx mdEntryAbsAddr
  let argNames = fmapFC (Const . getValueName) rNameAssign
  initArgs <-
    loadInitialPreconditions (simInitialPreconditions simOpts) argNames initArgs_

  EntrypointCfgs
    { entrypointStartupOv = mbStartupOvSsa
    , entrypointCfg = ssa@(C.SomeCFG ssaCfg)
    } <- pure (toSsaSomeCfg <$> entrypointCfgs)
  let mbStartupOvSsaCfg = startupOvCfg <$> mbStartupOvSsa

  let mkInitState ::
        forall m.
        ( MonadIO m
        , MonadThrow m
        , Mem.HasLLVMAnn sym
        ) =>
        ArchRegs sym arch ->
        SetupMem sym ->
        C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) ->
        m (C.ExecState (GreaseSimulatorState sym arch) sym (Symbolic.MacawExt arch) (C.RegEntry sym (C.StructType (Symbolic.MacawCrucibleRegTypes arch))))
      mkInitState regs' mem' ssa'@(C.SomeCFG ssaCfg') = do
        mvar <- liftIO $ Mem.mkMemVar "grease:memmodel" halloc
        let builtinOvs = builtinStubsOverrides bak mvar memCfg0 archCtx
        fnOvsMap <- liftIO $ Macaw.mkMacawOverrideMap bak builtinOvs userOvPaths halloc mvar archCtx
        let memCfg1 = memConfigWithHandles bak la halloc archCtx memory symMap pltStubs dynFunMap fnOvsMap builtinGenericSyscalls errorSymbolicFunCalls memCfg0
        evalFn <- Symbolic.withArchEval @Symbolic.LLVMMemory @arch (archCtx ^. archVals) sym pure
        let macawExtImpl = Symbolic.macawExtensions evalFn mvar memCfg1
        let ssaCfgHdl = C.cfgHandle ssaCfg'
        -- At this point, the only function we have discovered is the user-requested
        -- entrypoint function. If we are simulating a binary, add it to the
        -- discovered function handles. (See Note [Incremental code discovery] in
        -- Grease.Macaw.SimulatorState.) If we are simulating an S-expression program,
        -- use an empty map instead. (See gitlab#118 for more discussion on this point.)
        let discoveredHdls = Maybe.maybe Map.empty (`Map.singleton` ssaCfgHdl) mbCfgAddr
        let personality = emptyGreaseSimulatorState & discoveredFnHandles .~ discoveredHdls
        initState bak la macawExtImpl halloc mvar mem' C.emptyGlobals archCtx memPtrTable setupHook personality regs' fnOvsMap mbStartupOvSsaCfg ssa'

  doLog la (Diag.TargetCFG ssaCfg)
  result <- refinementLoop la (simMaxIters simOpts) (simTimeout simOpts) rNamesAssign' initArgs $ \argShapes -> do
    (args, setupMem, setupAnns) <- setup la bak dl rNameAssign regTypes argShapes initMem
    regs' <- liftIO (overrideRegs (argVals args))
    bbMapRef <- liftIO (newIORef Map.empty)
    let ?recordLLVMAnnotation = \callStack (Mem.BoolAnn ann) bb ->
          modifyIORef bbMapRef $ Map.insert ann (callStack, bb)
    st <- mkInitState regs' setupMem ssa
    -- The order of the heuristics is significant, the 'macawHeuristics'
    -- find a sensible initial memory layout, which is necessary before
    -- applying the 'mustFailHeuristic' (which would otherwise report many
    -- memory-related errors as possible bugs, when they are actually just
    -- preconditions.)
    let heuristics =
          if simNoHeuristics simOpts
          then []
          else macawHeuristics la rNames List.++ [mustFailHeuristic]
    execAndRefine bak (simSolver simOpts) fm la setupAnns initMem heuristics argNames argShapes args bbMapRef (simLoopBound simOpts) execFeats st `catches`
      [ Handler $ \(ex :: X86Symbolic.MissingSemantics) ->
          pure $ ProveCantRefine $ MissingSemantics $ pshow ex
      , Handler (\(ex :: AArch32Symbolic.AArch32Exception) ->
          pure (ProveCantRefine (MissingSemantics (tshow ex))))
      , Handler (\(ex :: PPCSymbolic.SemanticsError) ->
          pure (ProveCantRefine (MissingSemantics (tshow ex))))
      ]

  res <- case result of
    RefinementBug b concData -> do
      let argsJson = concArgsToJson fm rNames (concArgs concData) regTypes
      pure (BatchBug (MkBatchBug b argsJson))
    RefinementCantRefine b ->
      pure (BatchCantRefine b)
    RefinementItersExceeded ->
      pure BatchItersExceeded
    RefinementTimeout ->
      pure BatchTimeout
    RefinementNoHeuristic errs -> do
      maybeBug <- checkMustFail bak errs
      case maybeBug of
        Just bug -> pure (BatchBug bug)
        Nothing ->
          pure $ BatchCouldNotInfer $ errs <&> \noHeuristic ->
            let cData = noHeuristicConcretizedData noHeuristic
                argsJson = concArgsToJson fm rNames (concArgs cData) regTypes
                addrWidth = archCtx ^. archInfo . to MI.archAddrWidth
                interestingShapes = interestingConcretizedShapes argNames (initArgs ^. Shape.argShapes) (concArgs cData)
                prettyArgs = printConcArgs addrWidth argNames interestingShapes (concArgs cData)
            in toFailedPredicate noHeuristic argsJson (Text.pack (show prettyArgs))
    RefinementSuccess argShapes -> do
      let pcReg = archCtx ^. archPcReg
      let assertInText = addPCBoundAssertion knownNat pcReg memory (MC.memWord $ fromIntegral starttext) (MC.memWord $ fromIntegral endtext) pltStubs
      let assertNoMprotect = case mprotectAddr of
            Just badAddr -> addNoDynJumpAssertion knownNat pcReg memory badAddr
            _ -> id
      let rs = simReqs simOpts
      asserts <- fmap (Map.fromList . List.zip rs) . forM rs $ \case
        InText -> pure assertInText
        NoMprotect -> pure assertNoMprotect
      doLog la $ Diag.SimulationTestingRequirements rs
      fmap BatchChecks . forM asserts $ \assert -> do
        C.Reg.SomeCFG assertingCfg <- pure $ assert $ entrypointCfg entrypointCfgs
        C.resetAssumptionState bak
        (args, setupMem, setupAnns) <- setup la bak dl rNameAssign regTypes argShapes initMem
        regs' <- liftIO (overrideRegs (argVals args))
        bbMapRef <- liftIO (newIORef Map.empty)
        let ?recordLLVMAnnotation = \callStack (Mem.BoolAnn ann) bb ->
              modifyIORef bbMapRef $ Map.insert ann (callStack, bb)
        let assertingSsa = C.toSSA assertingCfg
        st <- mkInitState regs' setupMem assertingSsa
        new <- execAndRefine bak (simSolver simOpts) fm la setupAnns initMem (macawHeuristics la rNames) argNames argShapes args bbMapRef (simLoopBound simOpts) execFeats st
        case new of
          ProveBug {} ->
            throw (GreaseException "CFG rewriting introduced a bug!")
          ProveCantRefine {} ->
            throw (GreaseException "CFG rewriting prevented refinement!")
          ProveSuccess -> do
            doLog la Diag.SimulationAllGoalsPassed
            pure CheckSuccess
          ProveNoHeuristic errs -> do
            doLog la Diag.SimulationGoalsFailed
            pure $ CheckAssertionFailure $ NE.toList errs <&> \noHeuristic ->
              let cData = noHeuristicConcretizedData noHeuristic
                  argsJson = concArgsToJson fm rNames (concArgs cData) regTypes
                  addrWidth = archCtx ^. archInfo . to MI.archAddrWidth
                  interestingShapes = interestingConcretizedShapes argNames (initArgs ^. Shape.argShapes) (concArgs cData)
                  prettyArgs = printConcArgs addrWidth argNames interestingShapes (concArgs cData)
              in toFailedPredicate noHeuristic argsJson (Text.pack (show prettyArgs))
          ProveRefine _ -> do
            doLog la Diag.SimulationGoalsFailed
            pure $ CheckAssertionFailure []
  traverse_ cancel (fmap snd profFeatLog)
  pure res
  where
    MacawCfgConfig
      { mcDataLayout = dl
      , mcMprotectAddr = mprotectAddr
      , mcSymMap = symMap
      , mcPltStubs = pltStubs
      , mcDynFunMap = dynFunMap
      , mcRelocs = relocs
      , mcMemory = memory
      , mcTxtBounds = (starttext, endtext)
      } = macawCfgConfig

simulateMacawCfgs ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  ArchContext arch ->
  SimOpts ->
  (forall sym. Macaw.SetupHook sym) ->
  Map Entrypoint (MacawEntrypointCfgs arch) ->
  IO Results
simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook cfgs = do
  let fm = W4.FloatRealRepr
  withSolverOnlineBackend (simSolver simOpts) fm globalNonceGenerator $ \bak -> do
    results <-
      forM (Map.toList cfgs) $ \(entry, MacawEntrypointCfgs entrypointCfgs mbCfgAddr) ->
        analyzeEntrypoint la entry $ do
          EntrypointCfgs
            { entrypointStartupOv = mbStartupOv
            , entrypointCfg = C.Reg.AnyCFG entrypointCfg'
            } <- pure entrypointCfgs
          let expectedArgTys = Ctx.singleton (regStructRepr archCtx)
          let expectedRet = regStructRepr archCtx
          let argTys = C.Reg.cfgArgTypes entrypointCfg'
          let ret = C.Reg.cfgReturnType entrypointCfg'
          Refl <-
            case testEquality argTys expectedArgTys of
              Just r -> pure r
              Nothing -> throw (GreaseException ("CFG must take a single argument, the Macaw register struct: " <> tshow (regStructRepr archCtx)))
          Refl <-
            case testEquality ret expectedRet of
              Just r -> pure r
              Nothing -> throw (GreaseException ("CFG must return the Macaw register struct: " <> tshow (regStructRepr archCtx)))
          mbStartupOvSome <- for mbStartupOv $ \startupOv -> do
            StartupOv
              { startupOvCfg = C.Reg.AnyCFG startupOvCfg'
              , startupOvForwardDecs = fwdDecs
              } <- pure startupOv
            Refl <-
              case testEquality (C.Reg.cfgArgTypes startupOvCfg') expectedArgTys of
                Just r -> pure r
                Nothing -> throw (GreaseException ("Startup override must take a single argument, the Macaw register struct: " <> tshow (regStructRepr archCtx)))
            Refl <-
              case testEquality (C.Reg.cfgReturnType startupOvCfg') expectedRet of
                Just r -> pure r
                Nothing -> throw (GreaseException ("Startup override must return the Macaw register struct: " <> tshow (regStructRepr archCtx)))
            pure $ StartupOv
                     { startupOvCfg = C.Reg.SomeCFG startupOvCfg'
                     , startupOvForwardDecs = fwdDecs
                     }
          let entrypointCfgsSome =
                EntrypointCfgs
                  { entrypointStartupOv = mbStartupOvSome
                  , entrypointCfg = C.Reg.SomeCFG entrypointCfg'
                  }
          status <- simulateMacawCfg la bak fm halloc macawCfgConfig archCtx simOpts setupHook mbCfgAddr entrypointCfgsSome
          let result =
                Batch
                  { batchStatus = status
                  , batchLoadOffset = fromMaybe 0 $
                                      MML.loadOffset $
                                      mcLoadOptions macawCfgConfig
                  }
          pure (entry, result)

    pure (Results (Map.fromList results))

-- | Convert a register-based CFG ('C.Reg.AnyCFG') to an SSA-based CFG
-- ('C.AnyCFG').
toSsaAnyCfg ::
  C.IsSyntaxExtension ext =>
  C.Reg.AnyCFG ext ->
  C.AnyCFG ext
toSsaAnyCfg (C.Reg.AnyCFG cfg) =
  case C.toSSA cfg of C.SomeCFG ssa -> C.AnyCFG ssa

-- | Convert a register-based CFG ('C.Reg.SomeCFG') to an SSA-based CFG
-- ('C.SomeCFG').
toSsaSomeCfg ::
  C.IsSyntaxExtension ext =>
  C.Reg.SomeCFG ext init ret ->
  C.SomeCFG ext init ret
toSsaSomeCfg (C.Reg.SomeCFG cfg) = C.toSSA cfg

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
    pure $ Map.map
             (\cfg -> EntrypointCfgs
                        { entrypointStartupOv = Nothing
                        , entrypointCfg = cfg
                        })
         $ Map.mapKeys
             (entrypointNoStartupOv . EntrypointSymbolName)
             cfgs
  else do
    results <-
      forM entries $ \e ->
        case entrypointLocation e of
          EntrypointAddress {} -> nonSymbolEntrypointException
          EntrypointCoreDump {} -> nonSymbolEntrypointException
          EntrypointSymbolName nm ->
            case Map.lookup nm cfgs of
              Just cfg -> do
                mbStartupOv <-
                  traverse (parseEntrypointStartupOv halloc)
                           (entrypointStartupOvPath e)
                let entrypointCfgs =
                      EntrypointCfgs
                        { entrypointStartupOv = mbStartupOv
                        , entrypointCfg = cfg
                        }
                pure (e, entrypointCfgs)
              Nothing -> throw $ GreaseException $ "Could not find function: " <> nm
    pure (Map.fromList results)
  where cfgs = parsedProgramCfgMap prog

analyzeEntrypoint :: GreaseLogAction -> Entrypoint -> IO a -> IO a
analyzeEntrypoint la entry act = do
  doLog la (Diag.AnalyzingEntrypoint entry)
  (duration, a) <- time act
  doLog la (Diag.FinishedAnalyzingEntrypoint (entrypointLocation entry) duration)
  pure a

simulateMacawSyntax ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  SimOpts ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  IO Results
simulateMacawSyntax la halloc archCtx simOpts parserHooks = do
  let ?parserHooks = machineCodeParserHooks (Proxy @arch) parserHooks
  prog <- parseProgram halloc (simProgPath simOpts)
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  cfgs <- entrypointCfgMap la halloc prog (simEntryPoints simOpts)
  let cfgs' = Map.map (\cfg -> MacawEntrypointCfgs cfg Nothing) cfgs
  let memory = MC.emptyMemory (archCtx ^. archInfo . to MI.archAddrWidth)
  let dl = DataLayout.defaultDataLayout
  let setupHook :: forall sym. SetupHook sym
      setupHook = Macaw.SetupHook $ \bak mvar funOvs -> do
        -- Register overrides, both user-defined ones and ones that are
        -- hard-coded into GREASE itself.
        forM_ (Map.elems funOvs) $ \mfo -> do
          let publicOvHdl = Macaw.mfoPublicFnHandle mfo
              publicOv = Macaw.mfoPublicOverride mfo
          Stubs.SomeFunctionOverride fnOv <- pure $ Macaw.mfoSomeFunctionOverride mfo
          C.bindFnHandle publicOvHdl (C.UseOverride publicOv)
          let auxFns = Stubs.functionAuxiliaryFnBindings fnOv
          forM_ auxFns $ \(C.FnBinding auxHdl auxSt) -> C.bindFnHandle auxHdl auxSt

        -- In addition to binding function handles for the user overrides,
        -- we must also redirect function handles resulting from parsing
        -- forward declarations (`declare`) to actually call the overrides.
        Macaw.registerMacawSexpProgForwardDeclarations bak la dl mvar funOvs (CSyn.parsedProgForwardDecs prog)
        forM_ (Map.elems funOvs) $ \mfo ->
          case Macaw.mfoSomeFunctionOverride mfo of
            Stubs.SomeFunctionOverride fnOv ->
              Macaw.registerMacawOvForwardDeclarations bak funOvs (Stubs.functionForwardDeclarations fnOv)
        forM_ (Map.elems cfgs) $ \entrypointCfgs ->
          forM_ (startupOvForwardDecs <$> entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
            Macaw.registerMacawOvForwardDeclarations bak funOvs startupOvFwdDecs
  let macawCfgConfig =
        MacawCfgConfig
          { mcDataLayout = dl
          , mcMprotectAddr = Nothing
          , mcLoadOptions = MML.defaultLoadOptions
          , mcSymMap = Map.empty
          , mcPltStubs = Map.empty
          , mcDynFunMap = Map.empty
          , mcRelocs = Map.empty
          , mcMemory = memory
          , mcTxtBounds = (0, 0)
          , mcElf = Nothing
          }
  simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook cfgs'

simulateMacaw ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  , Elf.IsRelocationType (ArchReloc arch)
  , Elf.RelocationWidth (ArchReloc arch) ~ MC.ArchAddrWidth arch
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , ?memOpts :: Mem.MemOptions
  ) =>
  GreaseLogAction ->
  C.HandleAllocator ->
  Elf.ElfHeaderInfo (MC.ArchAddrWidth arch) ->
  LoadedProgram arch ->
  Maybe (PLT.PLTStubInfo (ArchReloc arch)) ->
  ArchContext arch ->
  (Elf.ElfWordType (MC.ArchAddrWidth arch), Elf.ElfWordType (MC.ArchAddrWidth arch)) ->
  SimOpts ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  IO Results
simulateMacaw la halloc elf loadedProg mbPltStubInfo archCtx txtBounds simOpts parserHooks = do
  let ?parserHooks = machineCodeParserHooks (Proxy @arch) parserHooks
  let dl = progDataLayout loadedProg
  let memory = Loader.memoryImage $ progLoadedBinary loadedProg
  let loadOpts = progLoadOptions loadedProg
  let symMap = progSymMap loadedProg
  let dynFunMap = progDynFunMap loadedProg

  let relocs = elfRelocationMap (Proxy @(ArchReloc arch)) loadOpts elf
  let symbolRelocs =
       Map.mapMaybe
         (\(reloc, symb) ->
           if (archCtx ^. archRelocSupported) reloc == Just SymbolReloc
             then Just $ functionNameFromByteString symb
             else Nothing)
         relocs

  -- A map of each PLT stub address to its symbol name, which we consult
  -- later to check for the presence of `mprotect` (i.e., the `no-mprotect`
  -- requirement). This check only applies to dynamically linked binaries, and
  -- for statically linked binaries, this map will be empty.
  pltStubSegOffToNameMap <-
    resolvePltStubs mbPltStubInfo loadOpts elf symbolRelocs (simPltStubs simOpts) memory

  let -- The inverse of `pltStubSegOffToNameMap`.
      pltStubNameToSegOffMap :: Map.Map W4.FunctionName (MC.ArchSegmentOff arch)
      pltStubNameToSegOffMap =
        Map.foldrWithKey
          (\addr name -> Map.insert name addr)
          Map.empty
          pltStubSegOffToNameMap

  let mprotectAddr = Map.lookup "mprotect" pltStubNameToSegOffMap

  entries <-
    if List.null (simEntryPoints simOpts)
    then do
      doLog la Diag.NoEntrypoints
      pure (List.map
             (\(k, v) -> (entrypointFromBytestring v, k))
             (Map.toList (progSymMap loadedProg)))
    else
      forM (simEntryPoints simOpts) $ \entry -> do
        case Map.lookup entry (progEntrypointAddrs loadedProg) of
          Nothing -> throw . GreaseException $ "Impossible: entrypoint not in map"
          Just a -> pure (entry, a)

  cfgs <-
    fmap Map.fromList $
    forM entries $ \(entry, entryAddr) -> do
      C.Reg.SomeCFG cfg <-
        discoverFunction la halloc archCtx memory symMap pltStubSegOffToNameMap entryAddr
      mbStartupOv <-
        traverse (parseEntrypointStartupOv halloc) (entrypointStartupOvPath entry)
      let entrypointCfgs =
            EntrypointCfgs
              { entrypointStartupOv = mbStartupOv
              , entrypointCfg = C.Reg.AnyCFG cfg
              }
      pure (entry, MacawEntrypointCfgs entrypointCfgs (Just entryAddr))

  -- This setup hook does much less than the setup hook in simulateMacawSyntax.
  -- We don't need to register most functions here because that happens
  -- incrementally in Grease.Macaw.ResolveCall.lookupFunctionHandle.
  -- simulateMacawSyntax, on the other hand, looks up functions in a different
  -- way, so it must eagerly register all functions it might call ahead of time.
  --
  -- The exception to this rule is startup overrides. If a startup override
  -- exists and it contains forward declarations, then we redirect the function
  -- handles to actually call the respective overrides. (Alternatively, we could
  -- plumb the startup overrides' forward declarations into
  -- `lookupFunctionHandle` and register them incrementally, but that is more
  -- work. Given that startup overrides can't invoke anything defined in the
  -- main program itself, it's much less work to register them ahead of time
  -- here.)
  let setupHook :: forall sym. SetupHook sym
      setupHook = Macaw.SetupHook $ \bak _mvar funOvs ->
        forM_ (Map.elems cfgs) $ \(MacawEntrypointCfgs entrypointCfgs _) ->
          forM_ (startupOvForwardDecs <$> entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
            Macaw.registerMacawOvForwardDeclarations bak funOvs startupOvFwdDecs

  let macawCfgConfig =
        MacawCfgConfig
          { mcDataLayout = dl
          , mcMprotectAddr = mprotectAddr
          , mcLoadOptions = loadOpts
          , mcSymMap = symMap
          , mcPltStubs = pltStubSegOffToNameMap
          , mcDynFunMap = dynFunMap
          , mcRelocs = fst <$> relocs
          , mcMemory = memory
          , mcTxtBounds = txtBounds
          , mcElf = Just elf
          }
  simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook cfgs

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
  InitialMem sym ->
  LLVM.SetupHook sym ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG CLLVM.LLVM argTys (C.StructType argTys)) ->
  -- | The CFG of the user-requested entrypoint function.
  C.SomeCFG CLLVM.LLVM argTys ret ->
  IO BatchStatus
simulateLlvmCfg la simOpts bak fm halloc llvmCtx initMem setupHook mbStartupOvCfg scfg@(C.SomeCFG cfg) = do
  doLog la (Diag.TargetCFG cfg)

  profFeatLog <-
    traverse
      (greaseProfilerFeature @() @sym @CLLVM.LLVM @(C.RegEntry sym ret))
      (simProfileTo simOpts)

  C.Refl <-
    case C.testEquality ?ptrWidth (knownNat @64) of
      Just r -> pure r
      Nothing -> throw $ GreaseException "Bad pointer width"

  let argTys = C.cfgArgTypes cfg
      -- Display the arguments as though they are unnamed LLVM virtual registers.
      argNames = imapFC (\i _ -> Const ('%':show i)) argTys
  initArgs_ <- traverseFC (minimalShapeWithPtrs (pure . const NoTag)) argTys
  initArgs <-
    loadInitialPreconditions (simInitialPreconditions simOpts) argNames (ArgShapes initArgs_)

  let ?recordLLVMAnnotation = \_ _ _ -> pure ()
  result <- withMemOptions simOpts $
            refinementLoop la (simMaxIters simOpts) (simTimeout simOpts) argNames initArgs $ \argShapes -> do
    let valueNames = Ctx.generate (Ctx.size argTys) (\i -> ValueName ("arg" <> show i))
    let typeCtx = llvmCtx ^. Trans.llvmTypeCtx
    let dl = TCtx.llvmDataLayout typeCtx
    (args, setupMem, setupAnns) <- setup la bak dl valueNames argTys argShapes initMem
    bbMapRef <- liftIO (newIORef Map.empty)
    let ?recordLLVMAnnotation = \callStack (Mem.BoolAnn ann) bb ->
          modifyIORef bbMapRef $ Map.insert ann (callStack, bb)
    let llvmExtImpl = CLLVM.llvmExtensionImpl ?memOpts
    st <- LLVM.initState bak la llvmExtImpl halloc (simErrorSymbolicFunCalls simOpts) setupMem C.emptyGlobals llvmCtx setupHook (argVals args) mbStartupOvCfg scfg
    let cmdExt = Debug.llvmCommandExt
    debuggerFeat <-
      liftIO $
        if simDebug simOpts
        then do
          dbgInputs <- Dbg.defaultDebuggerInputs cmdExt
          Just <$>
            Dbg.debugger
              cmdExt
              (Debug.llvmExtImpl (Trans.llvmMemVar llvmCtx))
              prettyPtrFnMap
              dbgInputs
              Dbg.defaultDebuggerOutputs
              (C.cfgReturnType cfg)
        else pure Nothing
    let execFeats =
          catMaybes
          [ fmap fst profFeatLog
          , debuggerFeat
          , Just (greaseBranchTracerFeature la)
          ]
    -- See comment above on heuristics in 'simulateMacawCfg'
    let heuristics =
          if simNoHeuristics simOpts
          then []
          else llvmHeuristics la List.++ [mustFailHeuristic]
    execAndRefine bak (simSolver simOpts) fm la setupAnns initMem heuristics argNames argShapes args bbMapRef (simLoopBound simOpts) execFeats st

  res <- case result of
    RefinementBug b _ -> do
      let argsJson = []  -- TODO
      pure (BatchBug (MkBatchBug b argsJson))
    RefinementCantRefine b ->
      pure (BatchCantRefine b)
    RefinementItersExceeded ->
      pure BatchItersExceeded
    RefinementTimeout ->
      pure BatchTimeout
    RefinementNoHeuristic errs -> do
      maybeBug <- checkMustFail bak errs
      case maybeBug of
        Just bug -> pure (BatchBug bug)
        Nothing ->
          pure $ BatchCouldNotInfer $ NE.map (\n -> toFailedPredicate n [] "") errs
    RefinementSuccess _argShapes -> pure (BatchChecks Map.empty)
  traverse_ cancel (fmap snd profFeatLog)
  pure res

simulateLlvmCfgs ::
  Mem.HasPtrWidth (CLLVM.ArchWidth arch) =>
  (?parserHooks :: CSyn.ParserHooks CLLVM.LLVM) =>
  GreaseLogAction ->
  SimOpts ->
  C.HandleAllocator ->
  Trans.LLVMContext arch ->
  (forall sym bak. C.IsSymBackend sym bak => bak -> IO (InitialMem sym)) ->
  (forall sym. LLVM.SetupHook sym) ->
  Map Entrypoint (EntrypointCfgs (C.AnyCFG CLLVM.LLVM)) ->
  IO Results
simulateLlvmCfgs la simOpts halloc llvmCtx mkMem setupHook cfgs = do
  let fm = W4.FloatRealRepr
  withSolverOnlineBackend (simSolver simOpts) fm globalNonceGenerator $ \bak -> do
    initMem <- mkMem bak
    results <-
      forM (Map.toList cfgs) $ \(entry, entrypointCfgs) ->
        analyzeEntrypoint la entry $ do
          EntrypointCfgs
            { entrypointStartupOv = mbStartupOv
            , entrypointCfg = C.AnyCFG entrypointCfg'
            } <- pure entrypointCfgs
          mbStartupOvSomeCfg <-
            for (startupOvCfg <$> mbStartupOv) $ \(C.AnyCFG startupOvCfg') -> do
              let expectedArgTys = C.cfgArgTypes entrypointCfg'
              let expectedRetTy = C.StructRepr expectedArgTys
              let actualArgTys = C.cfgArgTypes startupOvCfg'
              let actualRetTy = C.cfgReturnType startupOvCfg'
              Refl <-
                case testEquality expectedArgTys actualArgTys of
                  Just r -> pure r
                  Nothing -> throw $ GreaseException $ Text.unlines
                    [ "Startup override must have the same argument types as the entrypoint function"
                    , "Entrypoint function argument types: " <> Text.pack (show expectedArgTys)
                    , "Startup override argument types: " <> Text.pack (show actualArgTys)
                    ]
              Refl <-
                case testEquality expectedRetTy actualRetTy of
                  Just r -> pure r
                  Nothing -> throw $ GreaseException $ Text.unlines
                    [ "Startup override must return a struct containing the argument types of the entrypoint function"
                    , "Entrypoint function argument types: " <> Text.pack (show expectedArgTys)
                    , "Startup override return type: " <> Text.pack (show actualRetTy)
                    ]
              pure $ C.SomeCFG startupOvCfg'
          status <- simulateLlvmCfg la simOpts bak fm halloc llvmCtx initMem setupHook mbStartupOvSomeCfg (C.SomeCFG entrypointCfg')
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
  let setupHook :: forall sym. LLVM.SetupHook sym
      setupHook = LLVM.SetupHook $ \bak halloc' llvmCtx' -> do
        -- Register built-in and user overrides.
        funOvs <-
          LLVM.registerLLVMSexpOverrides la builtinLLVMOverrides (simOverrides simOpts) bak halloc' llvmCtx' prog

        -- In addition to binding function handles for the user overrides,
        -- we must also redirect function handles resulting from parsing
        -- forward declarations (`declare`) to actually call the overrides.
        LLVM.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs (CSyn.parsedProgForwardDecs prog)
        forM_ (Map.elems cfgs) $ \entrypointCfgs ->
          forM_ (startupOvForwardDecs <$> entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
            LLVM.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs startupOvFwdDecs

        -- Register defined functions. If there is a user override of the same
        -- name, use the override's definition instead so that it takes
        -- precedence.
        forM_ (CSyn.parsedProgCFGs prog) $ \(C.Reg.AnyCFG defCfg) -> do
          let defHdl = C.Reg.cfgHandle defCfg
          let defName = C.handleName defHdl
          case Map.lookup defName funOvs of
            Nothing -> do
              C.SomeCFG defSsa <- pure $ C.toSSA defCfg
              -- This could probably be a helper defined in Crucible...
              let bindCfg c = C.bindFnHandle (C.cfgHandle c) (C.UseCFG c (C.postdomInfo c))
              bindCfg defSsa
            Just (CLLVM.SomeLLVMOverride llvmOverride) ->
              LLVM.bindLLVMOverrideFnHandle mvar defHdl llvmOverride
  simulateLlvmCfgs la simOpts halloc llvmCtx mkMem setupHook cfgs

simulateLlvm ::
  Trans.TranslationOptions ->
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateLlvm transOpts simOpts la = do
  llvmMod <-
    parseBitCodeFromFile (simProgPath simOpts) >>=
      \case
        Left _err -> throw $ GreaseException "Could not parse LLVM module"
        Right m -> pure m
  halloc <- C.newHandleAllocator
  mvar <- liftIO $ Mem.mkMemVar "grease:memmodel" halloc
  C.Some trans <- do
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
          EntrypointAddress {} -> nonSymbolEntrypointException
          EntrypointCoreDump {} -> nonSymbolEntrypointException
          EntrypointSymbolName nm ->
            Trans.getTranslatedCFG trans (L.Symbol (Text.unpack nm)) >>= \case
              Just (_decl, cfg, warns) -> do
                forM_ warns $ \warn -> doLog la (Diag.LLVMTranslationWarning warn)
                mbStartupOv <-
                  traverse (parseEntrypointStartupOv halloc)
                           (entrypointStartupOvPath entry)
                let mbStartupOvSsa = fmap toSsaAnyCfg <$> mbStartupOv
                let entrypointCfgs =
                      EntrypointCfgs
                        { entrypointStartupOv = mbStartupOvSsa
                        , entrypointCfg = cfg
                        }
                pure (entry, entrypointCfgs)
              Nothing -> throw $ GreaseException $ "Could not find function: " <> nm

    let dl = DataLayout.defaultDataLayout
    let setupHook :: forall sym. LLVM.SetupHook sym
        setupHook = LLVM.SetupHook $ \bak halloc' llvmCtx -> do
          -- Register defined functions...
          let handleTranslationWarning warn = doLog la (Diag.LLVMTranslationWarning warn)
          Trans.llvmPtrWidth llvmCtxt $ \ptrW' -> Mem.withPtrWidth ptrW' $
            CLLVM.registerLazyModule handleTranslationWarning trans
          -- ...and then register overrides. We register overrides *after*
          -- registering defined functions so that overrides take precedence over
          -- defined functions.
          funOvs <-
            LLVM.registerLLVMModuleOverrides la builtinLLVMOverrides (simOverrides simOpts) bak halloc' llvmCtx llvmMod
          -- If a startup override exists and it contains forward declarations,
          -- redirect then we redirect the function handles to actually call the
          -- respective overrides.
          forM_ (Map.elems cfgs) $ \entrypointCfgs ->
            forM_ (startupOvForwardDecs <$> entrypointStartupOv entrypointCfgs) $ \startupOvFwdDecs ->
              LLVM.registerLLVMSexpProgForwardDeclarations la dl mvar funOvs startupOvFwdDecs

    simulateLlvmCfgs la simOpts halloc llvmCtxt mkMem setupHook cfgs

-- | A 'GreaseLogAction' that diagnostics directly to @stderr@.
logAction :: Severity -> GreaseLogAction
logAction sev = LJ.LogAction $ \diag ->
  when (severity diag <= sev) $
    log (PP.pretty diag)

simulateARMSyntax ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateARMSyntax simOpts la = withMemOptions simOpts $ do
  halloc <- C.newHandleAllocator
  archCtx <- armCtx halloc Nothing (simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts AArch32Syn.aarch32ParserHooks

simulateARM :: SimOpts -> GreaseLogAction -> IO Results
simulateARM simOpts la = do
  let ?ptrWidth = knownNat @32
  let proxy = Proxy @ARM.ARM
  (perms, elf) <- readElfHeaderInfo proxy (simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- load la (simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds (progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- armCtx halloc (Just starttext) (simStackArgumentSlots simOpts)
    simulateMacaw la halloc elf loadedProg (Just ARM.armPLTStubInfo) archCtx txtBounds simOpts AArch32Syn.aarch32ParserHooks

simulatePPC32Syntax ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulatePPC32Syntax simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @32
  halloc <- C.newHandleAllocator
  -- We don't have an ELF file on hand, so there's no reasonable concrete
  -- default value to put on the stack as the return address, so we use fresh,
  -- symbolic bytes (Nothing).
  archCtx <- ppc32Ctx Nothing (simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts PPCSyn.ppc32ParserHooks

-- | Currently, @.ppc64.cbl@ files are not supported, as the @macaw-ppc@ API
-- does not make it straightforward to create a PPC64 'MI.ArchitectureInfo'
-- value without having access to a full binary. See
-- <https://github.com/GaloisInc/macaw/issues/415> for the full details.
simulatePPC64Syntax ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulatePPC64Syntax _simOpts _la =
  throw $ GreaseException "*.ppc64.cbl files are not currently supported."

simulatePPC32 :: SimOpts -> GreaseLogAction -> IO Results
simulatePPC32 simOpts la = do
  let ?ptrWidth = knownNat @32
  let proxy = Proxy @PPC.PPC32
  (perms, elf) <- readElfHeaderInfo proxy (simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- load la (simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds (progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- ppc32Ctx (Just starttext) (simStackArgumentSlots simOpts)
    -- See Note [Subtleties of resolving PLT stubs] (Wrinkle 3: PowerPC) in
    -- Grease.Macaw.PLT for why we use Nothing here.
    let ppcPltStubInfo = Nothing
    simulateMacaw la halloc elf loadedProg ppcPltStubInfo archCtx txtBounds simOpts PPCSyn.ppc32ParserHooks

simulatePPC64 :: SimOpts -> GreaseLogAction -> IO Results
simulatePPC64 simOpts la = do
  let ?ptrWidth = knownNat @64
  let proxy = Proxy @PPC.PPC64
  (perms, elf) <- readElfHeaderInfo proxy (simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- load la (simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds (progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- ppc64Ctx (Just starttext) (simStackArgumentSlots simOpts) (progLoadedBinary loadedProg)
    -- See Note [Subtleties of resolving PLT stubs] (Wrinkle 3: PowerPC) in
    -- Grease.Macaw.PLT for why we use Nothing here.
    let ppcPltStubInfo = Nothing
    simulateMacaw la halloc elf loadedProg ppcPltStubInfo archCtx txtBounds simOpts PPCSyn.ppc64ParserHooks

simulateX86Syntax ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateX86Syntax simOpts la = withMemOptions simOpts $ do
  halloc <- C.newHandleAllocator
  -- We don't have an ELF file on hand, so there's no reasonable concrete
  -- default value to put on the stack as the return address, so we use fresh,
  -- symbolic bytes (Nothing).
  archCtx <- x86Ctx halloc Nothing (simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts X86Syn.x86ParserHooks

simulateX86 :: SimOpts -> GreaseLogAction -> IO Results
simulateX86 simOpts la = do
  let ?ptrWidth = knownNat @64
  let proxy = Proxy @X86.X86_64
  (perms, elf) <- readElfHeaderInfo proxy (simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- load la (simEntryPoints simOpts) perms elf
    txtBounds@(startText, _) <- textBounds (progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- x86Ctx halloc (Just startText) (simStackArgumentSlots simOpts)
    simulateMacaw la halloc elf loadedProg (Just X86.x86_64PLTStubInfo) archCtx txtBounds simOpts X86Syn.x86ParserHooks

simulateElf ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateElf simOpts la = do
  bs <- liftIO $ BS.readFile (simProgPath simOpts)
  case Elf.decodeElfHeaderInfo bs of
    Right (Elf.SomeElf hdr) ->
      case (Elf.headerClass (Elf.header hdr), Elf.headerMachine (Elf.header hdr)) of
        (Elf.ELFCLASS32, Elf.EM_ARM) -> simulateARM simOpts la
        (Elf.ELFCLASS32, Elf.EM_PPC) -> simulatePPC32 simOpts la
        (Elf.ELFCLASS64, Elf.EM_PPC64) -> simulatePPC64 simOpts la
        (Elf.ELFCLASS64, Elf.EM_X86_64) -> simulateX86 simOpts la
        (_, mach) -> throw $ GreaseException $ "User error: unsupported ELF architecture: " <> tshow mach
    Left _ -> throw (GreaseException ("User error: expected ELF binary, but found non-ELF file at " <> Text.pack (simProgPath simOpts)))

simulateFile ::
  SimOpts ->
  GreaseLogAction ->
  IO Results
simulateFile opts =
  let path = simProgPath opts in
  if | ".armv7l.elf" `List.isSuffixOf` path -> simulateARM opts
     | ".ppc32.elf" `List.isSuffixOf` path -> simulatePPC32 opts
     | ".ppc64.elf" `List.isSuffixOf` path -> simulatePPC64 opts
     | ".x64.elf" `List.isSuffixOf` path -> simulateX86 opts
     | ".bc" `List.isSuffixOf` path -> simulateLlvm Trans.defaultTranslationOptions opts
     | ".armv7l.cbl" `List.isSuffixOf` path -> simulateARMSyntax opts
     | ".ppc32.cbl" `List.isSuffixOf` path -> simulatePPC32Syntax opts
     | ".ppc64.cbl" `List.isSuffixOf` path -> simulatePPC64Syntax opts
     | ".x64.cbl" `List.isSuffixOf` path -> simulateX86Syntax opts
     | ".llvm.cbl" `List.isSuffixOf` path -> simulateLlvmSyntax opts
     | otherwise -> simulateElf opts

-- | Also used in the test suite
logResults :: GreaseLogAction -> Results -> IO ()
logResults la (Results results) =
  forM_ (Map.toList results) $ \(entrypoint, result) ->
    doLog la $
    Diag.AnalyzedEntrypoint
      (entrypointLocation entrypoint)
      (batchStatus result)

main :: IO ()
main = do
  parsedOpts <- optsFromArgs
  let simOpts = optsSimOpts parsedOpts

      la :: GreaseLogAction
      la = logAction (optsVerbosity parsedOpts)
  rs@(Results results) <-
    simulateFile simOpts la
  if optsJSON parsedOpts
    then forM_ (Map.elems results) $ putStrLn . renderJSON
    else logResults la rs
