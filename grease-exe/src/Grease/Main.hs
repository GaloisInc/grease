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
  simulateARM,
  simulateARMSyntax,
  simulatePPC32,
  simulatePPC32Syntax,
  simulatePPC64,
  simulatePPC64Syntax,
  simulateX86,
  simulateX86Syntax,
  simulateLlvm,
  simulateLlvmSyntax,
  simulateFile,
  Results (..),
  logResults,
) where

import Control.Concurrent.Async (Async, cancel)
import Control.Exception.Safe (MonadThrow)
import Control.Exception.Safe qualified as X
import Control.Lens (to, (.~), (^.))
import Control.Monad (forM, forM_, when)
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const (Const (Const))
import Data.IntMap qualified as IntMap
import Data.LLVM.BitCode (formatError, parseBitCodeFromFileWithWarnings)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Macaw.AArch32.Symbolic qualified as AArch32Symbolic
import Data.Macaw.AArch32.Symbolic.Syntax qualified as AArch32Syn
import Data.Macaw.ARM qualified as ARM
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.BinaryLoader (BinaryLoader)
import Data.Macaw.BinaryLoader qualified as Loader
import Data.Macaw.BinaryLoader.AArch32 ()
import Data.Macaw.BinaryLoader.Raw ()
import Data.Macaw.BinaryLoader.X86 ()
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.ElfLoader.PLTStubs qualified as PLT
import Data.Macaw.Memory.LoadCommon qualified as MML
import Data.Macaw.PPC qualified as PPC
import Data.Macaw.PPC.Symbolic qualified as PPCSymbolic
import Data.Macaw.PPC.Symbolic.Syntax qualified as PPCSyn
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Debug qualified as MDebug
import Data.Macaw.Symbolic.Memory qualified as MSM (MacawProcessAssertion)
import Data.Macaw.Symbolic.Memory.Lazy qualified as Symbolic
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)
import Data.Macaw.X86 qualified as X86
import Data.Macaw.X86.Crucible qualified as X86Symbolic
import Data.Macaw.X86.Symbolic.Syntax qualified as X86Syn
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Parameterized.Classes (IxedF' (ixF'))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Parameterized.TraversableFC (fmapFC)
import Data.Parameterized.TraversableFC.WithIndex (imapFC)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Traversable (for)
import Data.Traversable.WithIndex (iforM)
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Vector qualified as Vec
import Data.Void (Void)
import GHC.TypeNats (KnownNat)
import Grease.AssertProperty (addNoDynJumpAssertion, addPCBoundAssertion)
import Grease.Bug qualified as Bug
import Grease.Cli (optsFromArgs)
import Grease.Concretize qualified as Conc
import Grease.Concretize.JSON (concArgsToJson)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic qualified as GDiag
import Grease.Diagnostic.Severity (Severity)
import Grease.Entrypoint qualified as EP
import Grease.ExecutionFeatures (greaseExecFeats)
import Grease.Heuristic qualified as H
import Grease.LLVM qualified as LLVM
import Grease.LLVM.Overrides qualified as GLO
import Grease.LLVM.Overrides.SExp qualified as GLOS
import Grease.LLVM.Personality qualified as GLP
import Grease.LLVM.SetupHook qualified as LLVM (SetupHook, moduleSetupHook, syntaxSetupHook)
import Grease.LLVM.SetupHook.Diagnostic qualified as LDiag (Diagnostic (LLVMTranslationWarning))
import Grease.LLVM.Shapes qualified as GLS
import Grease.Macaw qualified as GM
import Grease.Macaw.Arch qualified as GMA
import Grease.Macaw.Arch.AArch32 (armCtx)
import Grease.Macaw.Arch.PPC32 (ppc32Ctx)
import Grease.Macaw.Arch.PPC64 (ppc64Ctx)
import Grease.Macaw.Arch.X86 (x86Ctx)
import Grease.Macaw.Discovery (discoverFunction)
import Grease.Macaw.Entrypoint (checkMacawEntrypointCfgsSignatures)
import Grease.Macaw.Entrypoint qualified as GME
import Grease.Macaw.Load qualified as Load
import Grease.Macaw.Load.Relocation (RelocType (SymbolReloc), RelocationError (RelocationSymbolNotFound), elfRelocationMap)
import Grease.Macaw.Overrides (mkMacawOverrideMapWithBuiltins)
import Grease.Macaw.Overrides.Address (AddressOverrides, loadAddressOverrides)
import Grease.Macaw.Overrides.Address qualified as AddrOv
import Grease.Macaw.Overrides.SExp (MacawSExpOverride)
import Grease.Macaw.Overrides.SExp qualified as GMOS
import Grease.Macaw.PLT qualified as GMPLT
import Grease.Macaw.RegName (getRegName, mkRegName, regNameToString, regNames)
import Grease.Macaw.SetupHook qualified as Macaw (SetupHook, binSetupHook, syntaxSetupHook)
import Grease.Macaw.Shapes qualified as GMS
import Grease.Macaw.SimulatorHooks (ExecutingAddressAction (ExecutingAddressAction))
import Grease.Macaw.SimulatorState (GreaseSimulatorState, discoveredFnHandles, emptyGreaseSimulatorState)
import Grease.Main.Diagnostic qualified as Diag
import Grease.MustFail qualified as MustFail
import Grease.Options qualified as GO
import Grease.Output qualified as GOut
import Grease.Panic (Grease, panic)
import Grease.Pretty (prettyPtrFnMap)
import Grease.Profiler.Feature (greaseProfilerFeature)
import Grease.Refine
import Grease.Refine qualified as GRef
import Grease.Requirement
import Grease.Requirement qualified as GReq
import Grease.Setup
import Grease.Setup qualified as GSetup
import Grease.Setup qualified as Setup
import Grease.Shape (ArgShapes (..), ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.Concretize (concShape)
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Parse qualified as Parse
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Print qualified as ShapePrint
import Grease.Solver (withSolverOnlineBackend)
import Grease.SymIO qualified as GSIO
import Grease.Syntax (parseOverridesYaml, parsedProgramCfgMap, resolveOverridesYaml)
import Grease.Syntax qualified as GSyn
import Grease.Syscall (builtinGenericSyscalls)
import Grease.Time (time)
import Grease.Utility qualified as GUtil
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM qualified as CLLVM
import Lang.Crucible.LLVM.DataLayout (DataLayout)
import Lang.Crucible.LLVM.DataLayout qualified as DataLayout
import Lang.Crucible.LLVM.Debug qualified as LDebug
import Lang.Crucible.LLVM.Extension qualified as CLLVM
import Lang.Crucible.LLVM.Globals qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.LLVM.Syntax (emptyParserHooks, llvmParserHooks)
import Lang.Crucible.LLVM.Translation qualified as CLT
import Lang.Crucible.LLVM.TypeContext qualified as CLTC
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.SimError qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Lumberjack qualified as LJ
import Numeric (showHex)
import Panic qualified
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Directory (Permissions, getPermissions)
import System.Exit qualified as Exit
import System.IO (Handle, IOMode (WriteMode), withFile)
import Text.LLVM qualified as L
import Text.Read (readMaybe)
import What4.Expr qualified as WE
import What4.Expr.Builder qualified as WEB
import What4.FunctionName qualified as WFN
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL
import What4.Protocol.Online qualified as WPO

{- Note [Explicitly listed errors]

In this module, we frequently explicitly list all constructors of an error type,
even when performing the same action for many or all of them. This is so that
we get a compile error when new error constructors are added and can make an
informed judgment about whether they are in fact user errors, internal GREASE
errors, malformed inputs, etc.

-}

-- | Results of analysis, one per given 'EP.Entrypoint'
newtype Results = Results {getResults :: Map EP.Entrypoint GOut.Batch}

doLog :: MonadIO m => GDiag.GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (GDiag.MainDiagnostic diag)

malformedElf :: GDiag.GreaseLogAction -> PP.Doc Void -> IO a
malformedElf la doc = do
  doLog la (Diag.MalformedElf doc)
  Exit.exitFailure

malformedLlvm :: GDiag.GreaseLogAction -> PP.Doc Void -> IO a
malformedLlvm la doc = do
  doLog la (Diag.MalformedLlvm doc)
  Exit.exitFailure

unsupported :: GDiag.GreaseLogAction -> PP.Doc Void -> IO a
unsupported la doc = do
  doLog la (Diag.Unsupported doc)
  Exit.exitFailure

userErr :: GDiag.GreaseLogAction -> PP.Doc Void -> IO a
userErr la doc = do
  doLog la (Diag.UserError doc)
  Exit.exitFailure

-- | GREASE invoked a forward declaration, but it was unable to resolve the
-- 'C.FnHandle' corresponding to the declaration. Throw an exception suggesting
-- that the user try an override.
declaredFunNotFound :: GDiag.GreaseLogAction -> WFN.FunctionName -> IO a
declaredFunNotFound la decName =
  userErr la $
    PP.hsep
      [ "Function declared but not defined:"
      , PP.pretty (WFN.functionName decName) <> "."
      , "Try specifying an override using --overrides."
      ]

-- | Read a binary's file permissions and ELF header info.
readElfHeaderInfo ::
  ( MonadIO m
  , MonadThrow m
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  ) =>
  GDiag.GreaseLogAction ->
  proxy arch ->
  FilePath ->
  m (Permissions, Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
readElfHeaderInfo la _proxy path =
  do
    perms <- liftIO $ getPermissions path
    bs <- liftIO $ BS.readFile path
    case Elf.decodeElfHeaderInfo bs of
      Right (Elf.SomeElf hdr) ->
        case ( Elf.headerClass (Elf.header hdr)
             , C.testEquality ?ptrWidth (knownNat @32)
             , C.testEquality ?ptrWidth (knownNat @64)
             ) of
          (Elf.ELFCLASS32, Just Refl, Nothing) -> pure (perms, hdr)
          (Elf.ELFCLASS64, Nothing, Just Refl) -> pure (perms, hdr)
          _ -> panic "readElfHeaderInfo" ["Bad pointer width"]
      Left _ ->
        liftIO (userErr la ("expected AArch32, PowerPC, or x86_64 ELF binary, but found non-ELF file at " <> PP.pretty path))

-- Helper, not exported
--
-- Throw an exception if a user provides an entrypoint without a name in a
-- context where only named entrypoints make sense (e.g., when simulating an
-- S-expression program).
nonSymbolEntrypoint :: GDiag.GreaseLogAction -> IO a
nonSymbolEntrypoint la =
  userErr la "Must provide entrypoint name"

textBounds ::
  Num (Elf.ElfWordType w) =>
  GDiag.GreaseLogAction ->
  MML.LoadOptions ->
  Elf.ElfHeaderInfo w ->
  IO (Elf.ElfWordType w, Elf.ElfWordType w)
textBounds la loadOpts elf =
  case Elf.headerNamedShdrs elf of
    Left (si, err) ->
      malformedElf la $
        PP.hcat
          [ "Failed to lookup ELF section header at index "
          , PP.viaShow si
          , ": "
          , PP.viaShow err
          ]
    Right shdrs ->
      case fmap NE.head . NE.nonEmpty . List.filter (\s -> Elf.shdrName s == ".text") $ Vec.toList shdrs of
        Nothing -> malformedElf la "Could not find .text segment"
        Just s ->
          let loadOffset = fromIntegral $ fromMaybe 0 (MML.loadOffset loadOpts)
           in let sWithOffset = Elf.shdrAddr s + loadOffset
               in pure (sWithOffset, sWithOffset + Elf.shdrSize s)

-- | Define @?memOpts@ in a continuation, setting the 'CLM.laxLoadsAndStores'
-- option according to whether the user set the @--rust@ flag.
withMemOptions :: GO.SimOpts -> ((?memOpts :: CLM.MemOptions) => r) -> r
withMemOptions opts k =
  let ?memOpts =
        CLM.defaultMemOptions
          { -- If @--rust@ is enabled, then we enable lax loads and stores so
            -- that reading from uninitialized memory returns symbolic data
            -- rather than failing. This is a common pattern observed with the
            -- LLVM code generated from Rust enums. For instance, constructing
            -- a @None@ value requires allocating space for a payload (but not
            -- writing to it), and reading the payload's memory later should not
            -- induce failure on its own. See gitlab#177.
            CLM.laxLoadsAndStores = GO.simRust opts
          , -- This option tells Crucible-LLVM to invent a fresh boolean
            -- constant to use as a proof obligation when certain reads fail.
            -- This interacts poorly with the must-fail heuristic, as this proof
            -- obligation semantically should always fail, but as it is a fresh
            -- constant, there is no way for the heuristic to see that.
            -- See gitlab#256.
            CLM.noSatisfyingWriteFreshConstant = False
          }
   in k

loadInitialPreconditions ::
  ExtShape ext ~ PtrShape ext w =>
  CLM.HasPtrWidth w =>
  GDiag.GreaseLogAction ->
  FilePath ->
  IO (Parse.ParsedShapes ext)
loadInitialPreconditions la path =
  Parse.fromFile path
    >>= \case
      Left err -> userErr la err
      Right parsed -> pure parsed

toBatchBug ::
  CLM.HasPtrWidth wptr =>
  (sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  WE.FloatModeRepr fm ->
  MC.AddrWidthRepr wptr ->
  Ctx.Assignment (Const String) args ->
  Ctx.Assignment C.TypeRepr args ->
  Shape.ArgShapes ext NoTag args ->
  Bug.BugInstance ->
  Conc.ConcretizedData sym ext args ->
  GOut.BatchBug
toBatchBug fm addrWidth argNames argTys initArgs b cData =
  let argsJson = concArgsToJson fm argNames (Conc.concArgs cData) argTys
   in let interestingShapes = interestingConcretizedShapes argNames (initArgs ^. Shape.argShapes) (Conc.concArgs cData)
       in let prettyConc = Conc.printConcData addrWidth argNames interestingShapes cData
           in let prettyConc' = PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions prettyConc)
               in GOut.MkBatchBug
                    { GOut.bugDesc = b
                    , GOut.bugArgs = argsJson
                    , GOut.bugShapes = prettyConc'
                    }

toFailedPredicate ::
  CLM.HasPtrWidth wptr =>
  (sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  WE.FloatModeRepr fm ->
  MC.AddrWidthRepr wptr ->
  Ctx.Assignment (Const String) args ->
  Ctx.Assignment C.TypeRepr args ->
  Shape.ArgShapes ext NoTag args ->
  GRef.NoHeuristic sym ext args ->
  GOut.FailedPredicate
toFailedPredicate fm addrWidth argNames argTys initArgs (GRef.NoHeuristic goal cData _err) =
  let argsJson = concArgsToJson fm argNames (Conc.concArgs cData) argTys
      interestingShapes = interestingConcretizedShapes argNames (initArgs ^. Shape.argShapes) (Conc.concArgs cData)
      prettyConc = Conc.printConcData addrWidth argNames interestingShapes cData
      lp = CB.proofGoal goal
      simErr = lp ^. CB.labeledPredMsg
      -- We inline and modify 'ppSimError', because we don't need to repeat the
      -- function name nor location.
      msg =
        GUtil.tshow $
          PP.vcat $
            let details = C.simErrorDetailsMsg (C.simErrorReason simErr)
             in (PP.pretty (C.simErrorReasonMsg (C.simErrorReason simErr)) :)
                  ( if List.null details
                      then []
                      else ["Details:", PP.indent 2 (PP.pretty details)]
                  )
   in GOut.FailedPredicate
        { GOut._failedPredicateLocation =
            GUtil.tshow (PP.pretty (WPL.plSourceLoc (C.simErrorLoc simErr)))
        , GOut._failedPredicateMessage = msg
        , GOut._failedPredicateArgs = argsJson
        , GOut._failedPredicateConcShapes =
            PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions prettyConc)
        }

checkMustFail ::
  ( CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)
  , bak ~ CB.OnlineBackend solver scope st (WEB.Flags fm)
  , WPO.OnlineSolver solver
  ) =>
  bak ->
  NE.NonEmpty (GRef.NoHeuristic sym ext tys) ->
  IO (Maybe GOut.BatchBug)
checkMustFail bak errs = do
  let noHeuristicPreds =
        NE.toList (NE.map (\err -> (GRef.noHeuristicGoal err, GRef.noHeuristicError err)) errs)
  mustFail <- MustFail.checkOneMustFail bak noHeuristicPreds
  let bug =
        Bug.BugInstance
          { Bug.bugType = Bug.OneMustFail
          , Bug.bugLoc = "<multiple locations>"
          , Bug.bugDetails =
              let goals = NE.toList (NE.map GRef.noHeuristicGoal errs)
                  doPrint o =
                    let lp = CB.proofGoal o
                     in let simError = lp ^. CB.labeledPredMsg
                         in GUtil.tshow (C.ppSimError simError)
               in Just $ Text.unlines (List.map doPrint goals)
          , Bug.bugUb = Nothing -- TODO: check if they are all the same?
          }
  if mustFail
    -- TODO: concretized args
    then pure (Just (GOut.MkBatchBug{GOut.bugDesc = bug, GOut.bugArgs = [], GOut.bugShapes = ""}))
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
  , mcPltStubs :: Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName
  -- ^ Map of addresses to PLT stub names.
  , mcDynFunMap :: Map.Map WFN.FunctionName (MC.ArchSegmentOff arch)
  -- ^ Map of dynamic function names to their addresses.
  , mcRelocs :: Map.Map (MM.MemWord (MC.ArchAddrWidth arch)) (GMA.ArchReloc arch)
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
macawDataLayout :: GMA.ArchContext arch -> DataLayout
macawDataLayout archCtx =
  DataLayout.defaultDataLayout
    & DataLayout.intLayout
      .~ (archCtx ^. GMA.archInfo . to (Symbolic.toCrucibleEndian . MI.archEndianness))

-- | Integer argument registers
interestingRegs :: Set String
interestingRegs =
  Set.fromList
    [ -- AArch32
      "R0"
    , "R1"
    , "R2"
    , "R3"
    , -- PPC32
      "r3"
    , "r4"
    , "r5"
    , "r6"
    , "r7"
    , "r8"
    , "r9"
    , "r10"
    , -- x86_64
      "rdi"
    , "rsi"
    , "rdx"
    , "rcx"
    , "r8" -- also listed above in PPC32, but it's a set, so...
    , "r9"
    ]

-- | Filter out \"uninteresting\" concretized shapes
--
-- They are interesting when:
--
-- * They have a non-default value, and
-- * The name is in a known \"interesting\" set ('interestingRegs'), or is an
--   LLVM argument name (i.e., is prefixed by @%@).
--
-- This is clearly a bit of a hack, but leads to concise and readable output.
interestingConcretizedShapes ::
  CLM.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  -- | Default/initial/minimal shapes
  Ctx.Assignment (Shape.Shape ext tag) argTys ->
  Conc.ConcArgs sym ext argTys ->
  Ctx.Assignment (Const Bool) argTys
interestingConcretizedShapes names initArgs (Conc.ConcArgs cArgs) =
  let cShapes = fmapFC (Shape.tagWithType . concShape) cArgs
   in let initArgs' = fmapFC Shape.tagWithType initArgs
       in let isLlvmArg name = "%" `List.isPrefixOf` name
           in Ctx.zipWith
                ( \(Const name) (Const isDefault) ->
                    Const ((name `List.elem` interestingRegs || isLlvmArg name) && not isDefault)
                )
                names
                (Ctx.zipWith (\s s' -> Const (Maybe.isJust (testEquality s s'))) cShapes initArgs')

-- | Compute the initial 'ArgShapes' for a Macaw CFG.
--
-- See 'GMS.macawInitArgShapes' for details.
getMacawInitArgShapes ::
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GDiag.GreaseLogAction ->
  bak ->
  GMA.ArchContext arch ->
  GO.InitialPreconditionOpts ->
  MacawCfgConfig arch ->
  Ctx.Assignment (Const String) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  IO (ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)))
getMacawInitArgShapes la bak archCtx opts macawCfgConfig argNames mbCfgAddr = do
  parsed <-
    case GO.initPrecondPath opts of
      Nothing -> pure Nothing
      Just path -> Just <$> loadInitialPreconditions la path
  let elf = mcElf macawCfgConfig
  let memory = mcMemory macawCfgConfig
  GMS.macawInitArgShapes la bak archCtx opts parsed elf memory argNames mbCfgAddr
    >>= \case
      Left err -> userErr la (PP.pretty err)
      Right shapes -> pure shapes

-- | Implement 'GMA.archRegOverrides'
overrideRegs ::
  forall sym arch.
  ( CB.IsSymInterface sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Symbolic.SymArchConstraints arch
  ) =>
  GMA.ArchContext arch ->
  sym ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) ->
  IO (Ctx.Assignment (CS.RegValue' sym) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)))
overrideRegs archCtx sym =
  let rNames = regNames (archCtx ^. GMA.archVals)
      regTypes = Symbolic.crucArchRegTypes (archCtx ^. GMA.archVals . to Symbolic.archFunctions)
   in Ctx.traverseWithIndex
        ( \idx reg -> do
            let regName = getRegName rNames idx
            let isStackPointer = regName == mkRegName @arch MC.sp_reg
            case Map.lookup regName (archCtx ^. GMA.archRegOverrides) of
              Just i
                | isStackPointer ->
                    panic "overrideRegs" ["Can't override stack pointer"]
                | CLM.LLVMPointerRepr w <- regTypes ^. ixF' idx
                , Just C.Refl <- C.testEquality w ?ptrWidth -> do
                    let macawGlobalMemBlock = 1 -- TODO: don't hardcode this
                    blk <- liftIO $ WI.natLit sym macawGlobalMemBlock
                    off <- liftIO (WI.bvLit sym ?ptrWidth i)
                    let ptr = CLM.LLVMPointer blk off
                    pure $ CS.RV $ ptr
                | otherwise ->
                    panic "overrideRegs" ["Can't override non-pointer register"]
              Nothing -> pure reg
        )

macawExecFeats ::
  ( Symbolic.SymArchConstraints arch
  , GUtil.OnlineSolverAndBackend solver sym bak scope st (WEB.Flags fm)
  , ext ~ Symbolic.MacawExt arch
  , ?parserHooks :: CSyn.ParserHooks ext
  , Dbg.HasContext p MDebug.MacawCommand sym ext (Symbolic.ArchRegStruct arch)
  ) =>
  GDiag.GreaseLogAction ->
  bak ->
  C.GlobalVar CLM.Mem ->
  GMA.ArchContext arch ->
  MacawCfgConfig arch ->
  GO.SimOpts ->
  IO ([CS.ExecutionFeature p sym (Symbolic.MacawExt arch) (CS.RegEntry sym (Symbolic.ArchRegStruct arch))], Maybe (Async ()))
macawExecFeats la bak memVar archCtx macawCfgConfig simOpts = do
  profFeatLog <- traverse greaseProfilerFeature (GO.simProfileTo simOpts)
  let dbgOpts =
        if GO.debug (GO.simDebugOpts simOpts)
          then
            let mbElf = snd . Elf.getElf <$> mcElf macawCfgConfig
                extImpl = MDebug.macawExtImpl prettyPtrFnMap memVar (archCtx ^. GMA.archVals) mbElf
             in Just extImpl
          else Nothing
  feats <- greaseExecFeats la bak dbgOpts
  pure (feats, snd <$> profFeatLog)

llvmExecFeats ::
  forall p sym bak ext ret solver scope st fm.
  ( GUtil.OnlineSolverAndBackend solver sym bak scope st (WEB.Flags fm)
  , ext ~ CLLVM.LLVM
  , ?parserHooks :: CSyn.ParserHooks ext
  , Dbg.HasContext p LDebug.LLVMCommand sym ext ret
  ) =>
  GDiag.GreaseLogAction ->
  bak ->
  GO.SimOpts ->
  C.GlobalVar CLM.Mem ->
  IO ([CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)], Maybe (Async ()))
llvmExecFeats la bak simOpts memVar = do
  profFeatLog <- traverse greaseProfilerFeature (GO.simProfileTo simOpts)
  let dbgOpts =
        if GO.debug (GO.simDebugOpts simOpts)
          then Just (LDebug.llvmExtImpl memVar)
          else Nothing
  feats <- greaseExecFeats la bak dbgOpts
  pure (feats, snd <$> profFeatLog)

initDebugger ::
  WI.IsExpr (WI.SymExpr sym) =>
  PP.Pretty cExt =>
  PP.Pretty (Dbg.ResponseExt cExt) =>
  GDiag.GreaseLogAction ->
  GO.DebugOpts ->
  Dbg.CommandExt cExt ->
  C.TypeRepr t ->
  IO (Dbg.Context cExt sym ext t)
initDebugger la dbgOpts cExt t = do
  inps_ <- Dbg.defaultDebuggerInputs cExt
  stmts <-
    forM (GO.debugCmds dbgOpts) $ \cmdTxt ->
      case Dbg.parse cExt cmdTxt of
        Left err -> userErr la (PP.pretty (Dbg.renderParseError err))
        Right stmt -> pure stmt
  inps <- Dbg.prepend stmts inps_
  let pp = (PP.<> "\n") . PP.pretty
  let outps = Dbg.Outputs (doLog la . Diag.DebuggerOutput . pp)
  Dbg.initCtx cExt prettyPtrFnMap inps outps t

macawMemConfig ::
  ( Symbolic.SymArchConstraints arch
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)
  , bak ~ CB.OnlineBackend solver scope st (WEB.Flags fm)
  , WPO.OnlineSolver solver
  , Show (GMA.ArchReloc arch)
  , CLM.HasLLVMAnn sym
  , MSM.MacawProcessAssertion sym
  , ?memOpts :: CLM.MemOptions
  , ?lc :: CLTC.TypeContext
  , p ~ GreaseSimulatorState cExt sym arch
  ) =>
  GDiag.GreaseLogAction ->
  C.GlobalVar CLM.Mem ->
  CLSIO.LLVMFileSystem (MC.ArchAddrWidth arch) ->
  bak ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch) ->
  IO
    ( Symbolic.MemModelConfig p sym arch CLM.Mem
    , Map WFN.FunctionName (MacawSExpOverride p sym arch)
    )
macawMemConfig la mvar fs bak halloc macawCfgConfig archCtx simOpts memPtrTable = do
  let MacawCfgConfig
        { mcLoadOptions = loadOpts
        , mcSymMap = symMap
        , mcPltStubs = pltStubs
        , mcDynFunMap = dynFunMap
        , mcRelocs = relocs
        , mcMemory = memory
        } = macawCfgConfig
  let skipUnsupportedRelocs = GO.simSkipUnsupportedRelocs simOpts
  let memCfg_ = GM.memConfigInitial bak archCtx memPtrTable skipUnsupportedRelocs relocs
  let userOvPaths = GO.simOverrides simOpts
  fnOvsMap_ <- mkMacawOverrideMapWithBuiltins bak userOvPaths halloc mvar archCtx memCfg_ fs
  fnOvsMap <-
    case fnOvsMap_ of
      -- See Note [Explicitly listed errors]
      Left (GMOS.MacawSExpOverrideParseError e@GSyn.SExpressionParseError{}) -> userErr la (PP.pretty e)
      Left (GMOS.MacawSExpOverrideParseError e@GSyn.SyntaxParseError{}) -> userErr la (PP.pretty e)
      Left e@GMOS.MacawSExpOverrideLoaderError{} -> userErr la (PP.pretty e)
      Right ok -> pure ok
  fnAddrOvsRaw_ <-
    fmap mconcat . Monad.sequence
      <$> traverse parseOverridesYaml (GO.simOverridesYaml simOpts)
  fnAddrOvsRaw <-
    case fnAddrOvsRaw_ of
      Left err -> userErr la (PP.pretty err)
      Right ok -> pure ok
  fnAddrOvs_ <- resolveOverridesYaml loadOpts memory (Map.keysSet fnOvsMap) fnAddrOvsRaw
  fnAddrOvs <-
    case fnAddrOvs_ of
      -- See Note [Explicitly listed errors]
      Left e@GSyn.AddressUnresolvable{} -> userErr la (PP.pretty e)
      Left e@GSyn.FunctionNameNotFound{} -> userErr la (PP.pretty e)
      Right ok -> pure ok
  let errorSymbolicFunCalls = GO.simErrorSymbolicFunCalls simOpts
  let errorSymbolicSyscalls = GO.simErrorSymbolicSyscalls simOpts
  let skipInvalidCallAddrs = GO.simSkipInvalidCallAddrs simOpts
  let memCfg =
        GM.memConfigWithHandles
          bak
          la
          halloc
          archCtx
          memory
          symMap
          pltStubs
          dynFunMap
          fnOvsMap
          fnAddrOvs
          builtinGenericSyscalls
          (GO.simSkipFuns simOpts)
          errorSymbolicFunCalls
          errorSymbolicSyscalls
          skipInvalidCallAddrs
          (declaredFunNotFound la)
          memCfg_
  pure (memCfg, fnOvsMap)

macawInitState ::
  ( CB.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth wptr
  , MM.MemWidth wptr
  , Symbolic.SymArchConstraints arch
  , MSM.MacawProcessAssertion sym
  , GUtil.OnlineSolverAndBackend solver sym bak scope st (WEB.Flags fm)
  , Show (GMA.ArchReloc arch)
  , MSM.MacawProcessAssertion sym
  , wptr ~ MC.ArchAddrWidth arch
  , ext ~ Symbolic.MacawExt arch
  , ret ~ Symbolic.ArchRegStruct arch
  , p ~ GreaseSimulatorState MDebug.MacawCommand sym arch
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ?lc :: CLTC.TypeContext
  ) =>
  GDiag.GreaseLogAction ->
  GMA.ArchContext arch ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  GO.SimOpts ->
  bak ->
  C.GlobalVar CLM.Mem ->
  Symbolic.MemPtrTable sym wptr ->
  ExecutingAddressAction arch ->
  Macaw.SetupHook sym arch ->
  AddressOverrides arch ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  EP.EntrypointCfgs (C.SomeCFG ext (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) ret) ->
  C.GlobalVar ToConc.ToConcretizeType ->
  GSetup.SetupMem sym ->
  GSIO.InitializedFs sym wptr ->
  GSetup.Args sym ext (Symbolic.MacawCrucibleRegTypes arch) ->
  IO (CS.ExecState p sym ext (CS.RegEntry sym ret))
macawInitState la archCtx halloc macawCfgConfig simOpts bak memVar memPtrTable execCallback setupHook addrOvs mbCfgAddr entrypointCfgsSsa toConcVar setupMem initFs args = do
  let sym = CB.backendGetSym bak
  regs <- liftIO (overrideRegs archCtx sym (GSetup.argVals args))
  EP.EntrypointCfgs
    { EP.entrypointStartupOv = mbStartupOvSsa
    , EP.entrypointCfg = ssa@(C.SomeCFG ssaCfg)
    } <-
    pure entrypointCfgsSsa
  let mbStartupOvSsaCfg = EP.startupOvCfg <$> mbStartupOvSsa
  let fs = GSIO.initFs initFs
  (memCfg, fnOvsMap) <- macawMemConfig la memVar fs bak halloc macawCfgConfig archCtx simOpts memPtrTable
  evalFn <- Symbolic.withArchEval @Symbolic.LLVMMemory @_ (archCtx ^. GMA.archVals) sym pure
  let macawExtImpl = Symbolic.macawExtensions evalFn memVar memCfg
  let ssaCfgHdl = C.cfgHandle ssaCfg
  -- At this point, the only function we have discovered is the user-requested
  -- entrypoint function. If we are simulating a binary, add it to the
  -- discovered function handles. (See Note [Incremental code discovery] in
  -- Grease.Macaw.SimulatorState.) If we are simulating an S-expression program,
  -- use an empty map instead. (See gitlab#118 for more discussion on this point.)
  let discoveredHdls = Maybe.maybe Map.empty (`Map.singleton` ssaCfgHdl) mbCfgAddr

  let dbgOpts = GO.simDebugOpts simOpts
  let dbgCmdExt = MDebug.macawCommandExt (archCtx ^. GMA.archVals)
  dbgCtx <- initDebugger la dbgOpts dbgCmdExt (GM.regStructRepr archCtx)

  let personality =
        emptyGreaseSimulatorState toConcVar dbgCtx
          & discoveredFnHandles .~ discoveredHdls

  let globals = GSIO.initFsGlobals initFs
  let initFsOv = GSIO.initFsOverride initFs
  GM.initState bak la macawExtImpl execCallback halloc memVar setupMem globals initFsOv archCtx setupHook addrOvs personality regs fnOvsMap mbStartupOvSsaCfg ssa

macawRefineOnce ::
  ( CB.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth wptr
  , MM.MemWidth wptr
  , Symbolic.SymArchConstraints arch
  , GUtil.OnlineSolverAndBackend solver sym bak scope st (WEB.Flags fm)
  , Show (GMA.ArchReloc arch)
  , argTys ~ Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)
  , wptr ~ MC.ArchAddrWidth arch
  , ext ~ Symbolic.MacawExt arch
  , ret ~ Symbolic.ArchRegStruct arch
  , p ~ GreaseSimulatorState MDebug.MacawCommand sym arch
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ?lc :: CLTC.TypeContext
  , precond ~ ArgShapes ext NoTag argTys
  ) =>
  GDiag.GreaseLogAction ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  Symbolic.MemPtrTable sym wptr ->
  ExecutingAddressAction arch ->
  Macaw.SetupHook sym arch ->
  AddressOverrides arch ->
  bak ->
  WE.FloatModeRepr fm ->
  precond ->
  H.InitialMem sym ->
  C.GlobalVar CLM.Mem ->
  [H.RefineHeuristic sym bak ext argTys precond] ->
  [CS.ExecutionFeature p sym ext (CS.RegEntry sym ret)] ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  EP.EntrypointCfgs (C.SomeCFG ext (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) ret) ->
  IO (GRef.ProveRefineResult sym ext argTys precond)
macawRefineOnce la archCtx simOpts halloc macawCfgConfig memPtrTable execCallback setupHook addrOvs bak fm argShapes initMem memVar heuristics execFeats mbCfgAddr entrypointCfgsSsa = do
  let regTypes = Symbolic.crucArchRegTypes (archCtx ^. GMA.archVals . to Symbolic.archFunctions)
  let rNames = regNames (archCtx ^. GMA.archVals)
  let rNameAssign =
        Ctx.generate
          (Ctx.size regTypes)
          (\idx -> GSetup.ValueName (regNameToString (getRegName rNames idx)))
  let argNames = fmapFC (Const . GSetup.getValueName) rNameAssign
  GRef.refineOnce
    la
    simOpts
    halloc
    bak
    fm
    (mcDataLayout macawCfgConfig)
    rNameAssign
    argNames
    regTypes
    Setup.setup
    argShapes
    initMem
    memVar
    heuristics
    execFeats
    (macawInitState la archCtx halloc macawCfgConfig simOpts bak memVar memPtrTable execCallback setupHook addrOvs mbCfgAddr entrypointCfgsSsa)
    `X.catches` [ X.Handler $ \(ex :: X86Symbolic.MissingSemantics) ->
                    pure $ GRef.ProveCantRefine $ H.MissingSemantics $ GUtil.pshow ex
                , X.Handler
                    ( \(ex :: AArch32Symbolic.AArch32Exception) ->
                        pure (GRef.ProveCantRefine (H.MissingSemantics (GUtil.tshow ex)))
                    )
                , X.Handler
                    ( \(ex :: PPCSymbolic.SemanticsError) ->
                        pure (GRef.ProveCantRefine (H.MissingSemantics (GUtil.tshow ex)))
                    )
                ]

addressCallBack ::
  MM.MemWidth (MC.RegAddrWidth (MC.ArchReg arch)) =>
  Proxy arch ->
  Handle ->
  MM.MemSegmentOff (MC.RegAddrWidth (MC.ArchReg arch)) ->
  IO ()
addressCallBack _ hdl addr =
  let js = GOut.renderJSON $ Load.memSegOffToJson addr
   in Text.IO.hPutStrLn hdl js

withMaybeFile :: Maybe FilePath -> IOMode -> (Maybe Handle -> IO a) -> IO a
withMaybeFile Nothing _ f = f Nothing
withMaybeFile (Just pth) imd f = withFile pth imd (f . Just)

data PreconditionEnvironment sym bak ext argTys where
  PreconditionEnvironment ::
    (PP.Pretty fmted, pty ~ precond ext tag argTys) =>
    pty ->
    (pty -> fmted) ->
    [H.RefineHeuristic sym bak ext argTys pty] ->
    GRef.SetupFn precond ext tag argTys ->
    PreconditionEnvironment sym bak ext argTys

macawInitPreconditionEnv ::
  forall sym bak arch scope st fm solver.
  ( CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)
  , bak ~ CB.OnlineBackend solver scope st (WEB.Flags fm)
  , CLM.HasLLVMAnn sym
  , WPO.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GDiag.GreaseLogAction ->
  Proxy
    sym ->
  bak ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  MacawCfgConfig arch ->
  Ctx.Assignment (Const String) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  IO (PreconditionEnvironment sym bak (Symbolic.MacawExt arch) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)))
macawInitPreconditionEnv la _ bak archCtx simOpts macawCfgConfig argNames mbCfgAddr =
  do
    let rNames = regNames (archCtx ^. GMA.archVals)
    let heuristics =
          if GO.simNoHeuristics simOpts
            then [H.mustFailHeuristic]
            else H.macawHeuristics la rNames List.++ [H.mustFailHeuristic]
    let addrWidth = MC.addrWidthRepr (Proxy @(MC.ArchAddrWidth arch))
    let opts = GO.simInitPrecondOpts simOpts
    initArgShapes <- getMacawInitArgShapes la bak archCtx opts macawCfgConfig argNames mbCfgAddr
    let fmt = ShapePrint.PrintableShapes addrWidth argNames
    pure $ PreconditionEnvironment initArgShapes fmt heuristics Setup.setup

simulateMacawCfg ::
  forall sym bak arch solver scope st fm.
  ( CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)
  , bak ~ CB.OnlineBackend solver scope st (WEB.Flags fm)
  , WPO.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GDiag.GreaseLogAction ->
  bak ->
  WE.FloatModeRepr fm ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  ExecutingAddressAction arch ->
  Macaw.SetupHook sym arch ->
  AddressOverrides arch ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  -- | The entrypoint-related CFGs.
  EP.EntrypointCfgs (C.Reg.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  IO GOut.BatchStatus
simulateMacawCfg la bak fm halloc macawCfgConfig archCtx simOpts execCallback setupHook addrOvs mbCfgAddr entrypointCfgs = do
  let ?recordLLVMAnnotation = \_ _ _ -> pure ()
  globs <-
    case GO.simMutGlobs simOpts of
      GO.Initialized -> pure Symbolic.ConcreteMutable
      GO.Symbolic -> pure Symbolic.SymbolicMutable
      GO.Uninitialized ->
        unsupported la "Macaw does not support uninitialized globals (macaw#372)"
  (initMem, memPtrTable) <- GM.emptyMacawMem bak archCtx memory globs relocs

  let (tyCtxErrs, tyCtx) = CLTC.mkTypeContext dl IntMap.empty []
  let ?lc = tyCtx
  mapM_ (doLog la . Diag.TypeContextError) tyCtxErrs

  memVar <- CLM.mkMemVar "grease:memmodel" halloc
  (execFeats, profLogTask) <- macawExecFeats la bak memVar archCtx macawCfgConfig simOpts
  let regTypes = Symbolic.crucArchRegTypes (archCtx ^. GMA.archVals . to Symbolic.archFunctions)
  let rNames = regNames (archCtx ^. GMA.archVals)
  let argNames =
        Ctx.generate
          (Ctx.size regTypes)
          (\idx -> Const (regNameToString (getRegName rNames idx)))

  initArgShapes <-
    let opts = GO.simInitPrecondOpts simOpts
     in getMacawInitArgShapes la bak archCtx opts macawCfgConfig argNames mbCfgAddr

  entrypointCfgsSsa@EP.EntrypointCfgs{EP.entrypointCfg = C.SomeCFG ssaCfg} <-
    pure (EP.entrypointCfgsToSsa entrypointCfgs)
  doLog la (Diag.TargetCFG ssaCfg)

  let bounds = GO.simBoundsOpts simOpts
  -- The order of the heuristics is significant, the 'H.macawHeuristics'
  -- find a sensible initial memory layout, which is necessary before
  -- applying the 'H.mustFailHeuristic' (which would otherwise report many
  -- memory-related errors as possible bugs, when they are actually just
  -- preconditions.)
  let heuristics =
        if GO.simNoHeuristics simOpts
          then [H.mustFailHeuristic]
          else H.macawHeuristics la rNames List.++ [H.mustFailHeuristic]
  let addrWidth = MC.addrWidthRepr (Proxy @(MC.ArchAddrWidth arch))
  result <- GRef.refinementLoop la bounds initArgShapes (ShapePrint.PrintableShapes addrWidth argNames) $ \argShapes ->
    macawRefineOnce
      la
      archCtx
      simOpts
      halloc
      macawCfgConfig
      memPtrTable
      execCallback
      setupHook
      addrOvs
      bak
      fm
      argShapes
      initMem
      memVar
      heuristics
      execFeats
      mbCfgAddr
      entrypointCfgsSsa
  finalResult <-
    simulateRewrittenCfg
      la
      bak
      fm
      halloc
      macawCfgConfig
      archCtx
      simOpts
      setupHook
      addrOvs
      memPtrTable
      initMem
      initArgShapes
      result
      execFeats
      mbCfgAddr
      entrypointCfgs
      entrypointCfgsSsa
  traverse_ cancel profLogTask
  pure finalResult
 where
  MacawCfgConfig
    { mcDataLayout = dl
    , mcRelocs = relocs
    , mcMemory = memory
    } = macawCfgConfig

-- | See @doc/requirements.md@.
simulateRewrittenCfg ::
  forall sym bak arch solver scope st fm precond.
  ( CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st (WEB.Flags fm)
  , bak ~ CB.OnlineBackend solver scope st (WEB.Flags fm)
  , WPO.OnlineSolver solver
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , ?lc :: CLTC.TypeContext
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  , precond ~ ArgShapes (Symbolic.MacawExt arch) NoTag (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch))
  ) =>
  GDiag.GreaseLogAction ->
  bak ->
  WE.FloatModeRepr fm ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  Macaw.SetupHook sym arch ->
  AddressOverrides arch ->
  Symbolic.MemPtrTable sym (MC.ArchAddrWidth arch) ->
  H.InitialMem sym ->
  precond ->
  GRef.RefinementSummary sym (Symbolic.MacawExt arch) (Symbolic.CtxToCrucibleType (Symbolic.ArchRegContext arch)) precond ->
  [CS.ExecutionFeature (GreaseSimulatorState MDebug.MacawCommand sym arch) sym (Symbolic.MacawExt arch) (CS.RegEntry sym (Symbolic.ArchRegStruct arch))] ->
  -- | If simulating a binary, this is 'Just' the address of the user-requested
  -- entrypoint function. Otherwise, this is 'Nothing'.
  Maybe (MC.ArchSegmentOff arch) ->
  -- | The registerized entrypoint-related CFGs.
  EP.EntrypointCfgs (C.Reg.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  -- | The SSA entrypoint-related CFGs.
  EP.EntrypointCfgs (C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)) ->
  IO GOut.BatchStatus
simulateRewrittenCfg la bak fm halloc macawCfgConfig archCtx simOpts setupHook addrOvs memPtrTable initMem initArgShapes result execFeats mbCfgAddr entrypointCfgs entrypointCfgsSsa = do
  let regTypes = Symbolic.crucArchRegTypes (archCtx ^. GMA.archVals . to Symbolic.archFunctions)
  let rNames = regNames (archCtx ^. GMA.archVals)
  let argNames =
        Ctx.generate
          (Ctx.size regTypes)
          (\idx -> Const (regNameToString (getRegName rNames idx)))

  res <- case result of
    GRef.RefinementBug b cData ->
      let addrWidth = archCtx ^. GMA.archInfo . to MI.archAddrWidth
       in pure (GOut.BatchBug (toBatchBug fm addrWidth argNames regTypes initArgShapes b cData))
    GRef.RefinementCantRefine b ->
      pure (GOut.BatchCantRefine b)
    GRef.RefinementItersExceeded ->
      pure GOut.BatchItersExceeded
    GRef.RefinementNoHeuristic errs -> do
      maybeBug <- checkMustFail bak errs
      case maybeBug of
        Just bug -> pure (GOut.BatchBug bug)
        Nothing ->
          pure $
            GOut.BatchCouldNotInfer $
              errs <&> \noHeuristic ->
                let addrWidth = archCtx ^. GMA.archInfo . to MI.archAddrWidth
                 in toFailedPredicate fm addrWidth argNames regTypes initArgShapes noHeuristic
    GRef.RefinementSuccess argShapes -> do
      let pcReg = archCtx ^. GMA.archPcReg
      let assertInText = addPCBoundAssertion knownNat pcReg memory (MC.memWord $ fromIntegral starttext) (MC.memWord $ fromIntegral endtext) pltStubs
      let assertNoMprotect = case mprotectAddr of
            Just badAddr -> addNoDynJumpAssertion knownNat pcReg memory badAddr
            _ -> id
      let rs = GO.simReqs simOpts
      asserts <- fmap (Map.fromList . List.zip rs) . forM rs $ \case
        GReq.InText -> pure assertInText
        GReq.NoMprotect -> pure assertNoMprotect
      doLog la $ Diag.SimulationTestingRequirements rs
      fmap GOut.BatchChecks . forM asserts $ \assert -> do
        assertingCfg <- pure $ assert $ EP.entrypointCfg entrypointCfgs
        let entrypointCfgsSsa' = entrypointCfgsSsa{EP.entrypointCfg = EP.toSsaSomeCfg assertingCfg}
        CB.resetAssumptionState bak
        memVar <- CLM.mkMemVar "grease:memmodel" halloc
        let heuristics = H.macawHeuristics la rNames
        new <-
          macawRefineOnce
            la
            archCtx
            simOpts
            halloc
            macawCfgConfig
            memPtrTable
            (ExecutingAddressAction $ \_ -> pure ())
            setupHook
            addrOvs
            bak
            fm
            argShapes
            initMem
            memVar
            heuristics
            execFeats
            mbCfgAddr
            entrypointCfgsSsa'
        case new of
          GRef.ProveBug{} ->
            panic "simulateRewrittenCfg" ["CFG rewriting introduced a bug!"]
          GRef.ProveCantRefine{} ->
            panic "simulateRewrittenCfg" ["CFG rewriting prevented refinement!"]
          GRef.ProveSuccess -> do
            doLog la Diag.SimulationAllGoalsPassed
            pure GOut.CheckSuccess
          GRef.ProveNoHeuristic errs -> do
            doLog la Diag.SimulationGoalsFailed
            pure $
              GOut.CheckAssertionFailure $
                NE.toList errs <&> \noHeuristic ->
                  let addrWidth = archCtx ^. GMA.archInfo . to MI.archAddrWidth
                   in toFailedPredicate fm addrWidth argNames regTypes initArgShapes noHeuristic
          GRef.ProveRefine _ -> do
            doLog la Diag.SimulationGoalsFailed
            pure $ GOut.CheckAssertionFailure []
  pure res
 where
  MacawCfgConfig
    { mcMprotectAddr = mprotectAddr
    , mcPltStubs = pltStubs
    , mcMemory = memory
    , mcTxtBounds = (starttext, endtext)
    } = macawCfgConfig

simulateMacawCfgs ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  MacawCfgConfig arch ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  (forall sym. Macaw.SetupHook sym arch) ->
  AddressOverrides arch ->
  Map EP.Entrypoint (EP.MacawEntrypointCfgs arch) ->
  IO Results
simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook addrOvs cfgs = do
  let fm = WE.FloatRealRepr
  withMaybeFile (GO.simDumpCoverage simOpts) WriteMode $ \mbCovHandle -> do
    let mem = mcMemory macawCfgConfig
    let secsJson = Load.dumpSections mem
    case mbCovHandle of
      Just covHandle -> Text.IO.hPutStrLn covHandle (GOut.renderJSON secsJson)
      Nothing -> pure ()
    withSolverOnlineBackend (GO.simSolver simOpts) fm globalNonceGenerator $ \bak -> do
      results <- do
        let nEntries = Map.size cfgs
        iforM (Map.toList cfgs) $ \i (entry, EP.MacawEntrypointCfgs entrypointCfgs mbCfgAddr) ->
          analyzeEntrypoint la entry i nEntries $ do
            entrypointCfgsSome <- do
              let usrErr err = do
                    let url = "https://galoisinc.github.io/grease/sexp-progs.html"
                    userErr la (PP.pretty err <> "\n" <> "For more information, see " <> url)
              case checkMacawEntrypointCfgsSignatures archCtx entrypointCfgs of
                -- See Note [Explicitly listed errors]
                Left err@GME.BadArgs{} -> usrErr err
                Left err@GME.BadRet{} -> usrErr err
                Right ok -> pure ok
            let
              addressHandle :: ExecutingAddressAction arch
              addressHandle = ExecutingAddressAction $ Maybe.maybe (\_ -> pure ()) (addressCallBack @arch Proxy) mbCovHandle
            status <- simulateMacawCfg la bak fm halloc macawCfgConfig archCtx simOpts addressHandle setupHook addrOvs mbCfgAddr entrypointCfgsSome
            let result =
                  GOut.Batch
                    { GOut.batchStatus = status
                    , GOut.batchLoadOffset =
                        fromMaybe 0 $
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

entrypointCfgMap ::
  ( C.IsSyntaxExtension ext
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  CSyn.ParsedProgram ext ->
  [EP.Entrypoint] ->
  IO (Map EP.Entrypoint (EP.EntrypointCfgs (C.Reg.AnyCFG ext)))
entrypointCfgMap la halloc prog entries =
  if List.null entries
    then do
      doLog la Diag.NoEntrypoints
      pure
        $ Map.map
          ( \cfg ->
              EP.EntrypointCfgs
                { EP.entrypointStartupOv = Nothing
                , EP.entrypointCfg = cfg
                }
          )
        $ Map.mapKeys
          (EP.entrypointNoStartupOv . EP.EntrypointSymbolName)
          cfgs
    else do
      results <-
        forM entries $ \e ->
          case EP.entrypointLocation e of
            EP.EntrypointAddress{} -> nonSymbolEntrypoint la
            EP.EntrypointCoreDump{} -> nonSymbolEntrypoint la
            EP.EntrypointSymbolName nm ->
              case Map.lookup nm cfgs of
                Just cfg -> do
                  mbStartupOv <-
                    case EP.entrypointStartupOvPath e of
                      Nothing -> pure Nothing
                      Just startupOvPath -> do
                        result <- EP.parseEntrypointStartupOv halloc startupOvPath
                        case result of
                          Left err -> userErr la (PP.pretty err)
                          Right startupOv -> pure $ Just startupOv
                  let entrypointCfgs =
                        EP.EntrypointCfgs
                          { EP.entrypointStartupOv = mbStartupOv
                          , EP.entrypointCfg = cfg
                          }
                  pure (e, entrypointCfgs)
                Nothing ->
                  userErr la (PP.pretty ("Could not find function:" <> nm))
      pure (Map.fromList results)
 where
  cfgs = parsedProgramCfgMap prog

analyzeEntrypoint ::
  GDiag.GreaseLogAction ->
  EP.Entrypoint ->
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
  doLog la (Diag.FinishedAnalyzingEntrypoint (EP.entrypointLocation entry) duration)
  pure a

loadAddrOvs ::
  ( Symbolic.SymArchConstraints arch
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  GDiag.GreaseLogAction ->
  GMA.ArchContext arch ->
  C.HandleAllocator ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  GO.SimOpts ->
  IO (AddressOverrides arch)
loadAddrOvs la archCtx halloc memory simOpts = do
  mbAddrOvs <- loadAddressOverrides (GM.regStructRepr archCtx) halloc memory (GO.simAddressOverrides simOpts)
  let usrErr = userErr la . PP.pretty
  case mbAddrOvs of
    -- See Note [Explicitly listed errors]
    Left e@AddrOv.BadAddress{} -> usrErr e
    Left e@AddrOv.BadAddressOverrideArgs{} -> usrErr e
    Left e@AddrOv.BadAddressOverrideReturn{} -> usrErr e
    Left e@AddrOv.OverrideNameError{} -> usrErr e
    Left e@AddrOv.AddressOverrideParseError{} -> usrErr e
    Right addrOvs -> pure addrOvs

loadLLVMSExpOvs ::
  CLM.HasPtrWidth w =>
  GDiag.GreaseLogAction ->
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar CLM.Mem ->
  IO (Seq.Seq (WFN.FunctionName, GLOS.LLVMSExpOverride))
loadLLVMSExpOvs la sexpOvPaths halloc mvar = do
  mbOvs <- liftIO (GLOS.loadOverrides sexpOvPaths halloc mvar)
  case mbOvs of
    Left err -> userErr la (PP.pretty err)
    Right ovs -> pure ovs

doParse ::
  ( ?parserHooks :: CSyn.ParserHooks ext
  , C.IsSyntaxExtension ext
  ) =>
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  FilePath ->
  IO (CSyn.ParsedProgram ext)
doParse la halloc path = do
  r <- GSyn.parseProgram halloc path
  case r of
    Left err -> userErr la (PP.pretty err)
    Right p -> pure p

simulateMacawSyntax ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  ) =>
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  IO Results
simulateMacawSyntax la halloc archCtx simOpts parserHooks = do
  let ?parserHooks = machineCodeParserHooks (Proxy @arch) parserHooks
  prog <- doParse la halloc (GO.simProgPath simOpts)
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  cfgs <- entrypointCfgMap la halloc prog (GO.simEntryPoints simOpts)
  let cfgs' = Map.map (\cfg -> EP.MacawEntrypointCfgs cfg Nothing) cfgs
  let memory = MC.emptyMemory (archCtx ^. GMA.archInfo . to MI.archAddrWidth)
  let dl = macawDataLayout archCtx
  let setupHook :: forall sym. Macaw.SetupHook sym arch
      setupHook =
        let errCb = GLO.CantResolveOverrideCallback $ \nm _hdl -> liftIO (declaredFunNotFound la nm)
         in Macaw.syntaxSetupHook la errCb dl (GO.simSkipFuns simOpts) cfgs prog
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
  addrOvs <- loadAddrOvs la archCtx halloc memory simOpts
  simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook addrOvs cfgs'

simulateMacaw ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Elf.IsRelocationType (GMA.ArchReloc arch)
  , Elf.RelocationWidth (GMA.ArchReloc arch) ~ MC.ArchAddrWidth arch
  , BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , ?memOpts :: CLM.MemOptions
  ) =>
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  Elf.ElfHeaderInfo (MC.ArchAddrWidth arch) ->
  Load.LoadedProgram arch ->
  Maybe (PLT.PLTStubInfo (GMA.ArchReloc arch)) ->
  GMA.ArchContext arch ->
  (Elf.ElfWordType (MC.ArchAddrWidth arch), Elf.ElfWordType (MC.ArchAddrWidth arch)) ->
  GO.SimOpts ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  IO Results
simulateMacaw la halloc elf loadedProg mbPltStubInfo archCtx txtBounds simOpts parserHooks = do
  let ?parserHooks = machineCodeParserHooks (Proxy @arch) parserHooks
  let dl = macawDataLayout archCtx
  let memory = Loader.memoryImage $ Load.progLoadedBinary loadedProg
  let loadOpts = Load.progLoadOptions loadedProg
  let symMap = Load.progSymMap loadedProg
  let dynFunMap = Load.progDynFunMap loadedProg

  relocs <-
    case elfRelocationMap (Proxy @(GMA.ArchReloc arch)) loadOpts elf of
      Left err@RelocationSymbolNotFound{} -> malformedElf la (PP.pretty err)
      Right rs -> pure rs
  let symbolRelocs =
        Map.mapMaybe
          ( \(reloc, symb) ->
              if (archCtx ^. GMA.archRelocSupported) reloc == Just SymbolReloc
                then Just $ GUtil.functionNameFromByteString symb
                else Nothing
          )
          relocs

  -- A map of each PLT stub address to its symbol name, which we consult
  -- later to check for the presence of `mprotect` (i.e., the `no-mprotect`
  -- requirement). This check only applies to dynamically linked binaries, and
  -- for statically linked binaries, this map will be empty.
  pltStubSegOffToNameMap <-
    GMPLT.resolvePltStubs (GO.simProgPath simOpts) mbPltStubInfo loadOpts elf symbolRelocs (GO.simPltStubs simOpts) memory
      >>= \case
        Left err@GMPLT.CouldNotResolvePltStub{} -> malformedElf la (PP.pretty err)
        Right pltStubs -> pure pltStubs

  let
    -- The inverse of `pltStubSegOffToNameMap`.
    pltStubNameToSegOffMap :: Map.Map WFN.FunctionName (MC.ArchSegmentOff arch)
    pltStubNameToSegOffMap =
      Map.foldrWithKey
        (\addr name -> Map.insert name addr)
        Map.empty
        pltStubSegOffToNameMap

  let mprotectAddr = Map.lookup "mprotect" pltStubNameToSegOffMap
  entries <-
    if List.null (GO.simEntryPoints simOpts)
      then do
        doLog la Diag.NoEntrypoints
        pure
          ( List.map
              (\(k, v) -> (EP.entrypointFromBytestring v, k))
              (Map.toList (Load.progSymMap loadedProg))
          )
      else forM (GO.simEntryPoints simOpts) $ \entry -> do
        case Map.lookup entry (Load.progEntrypointAddrs loadedProg) of
          Nothing -> panic "simulateMacaw" ["Entrypoint not in map"]
          Just a -> pure (entry, a)

  cfgs <-
    fmap Map.fromList $
      forM entries $ \(entry, entryAddr) -> do
        C.Reg.SomeCFG cfg <-
          discoverFunction la halloc archCtx memory symMap pltStubSegOffToNameMap entryAddr
        mbStartupOv <-
          case EP.entrypointStartupOvPath entry of
            Nothing -> pure Nothing
            Just startupOvPath -> do
              result <- EP.parseEntrypointStartupOv halloc startupOvPath
              case result of
                Left err -> userErr la (PP.pretty err)
                Right startupOv -> pure $ Just startupOv
        let entrypointCfgs =
              EP.EntrypointCfgs
                { EP.entrypointStartupOv = mbStartupOv
                , EP.entrypointCfg = C.Reg.AnyCFG cfg
                }
        pure (entry, EP.MacawEntrypointCfgs entrypointCfgs (Just entryAddr))

  addrOvs <- loadAddrOvs la archCtx halloc memory simOpts
  let setupHook :: forall sym. Macaw.SetupHook sym arch
      setupHook =
        let errCb = GLO.CantResolveOverrideCallback $ \nm _hdl -> liftIO (declaredFunNotFound la nm)
         in Macaw.binSetupHook errCb addrOvs cfgs

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
  simulateMacawCfgs la halloc macawCfgConfig archCtx simOpts setupHook addrOvs cfgs

-- | Compute the initial 'ArgShapes' for an LLVM CFG.
--
-- See 'GLS.llvmInitArgShapes' for details.
getLlvmInitArgShapes ::
  CLM.HasPtrWidth 64 =>
  GDiag.GreaseLogAction ->
  GO.InitialPreconditionOpts ->
  Maybe L.Module ->
  Ctx.Assignment (Const String) argTys ->
  -- | The CFG of the user-requested entrypoint function.
  C.CFG CLLVM.LLVM blocks argTys ret ->
  IO (ArgShapes CLLVM.LLVM NoTag argTys)
getLlvmInitArgShapes la opts llvmMod argNames cfg = do
  parsed <-
    case GO.initPrecondPath opts of
      Nothing -> pure Nothing
      Just path -> Just <$> loadInitialPreconditions la path
  case GLS.llvmInitArgShapes opts llvmMod argNames parsed cfg of
    Left (GLS.ParseError err) -> userErr la err
    Left (GLS.MinimalShapeError err) -> unsupported la (PP.pretty err)
    Left (GLS.TypeMismatch err) -> userErr la (PP.pretty err)
    Right ok -> pure ok

llvmInitPreconds ::
  forall sym bak solver t st fm argTys blocks ret arch.
  ( CLM.HasPtrWidth (CLLVM.ArchWidth arch)
  , GUtil.OnlineSolverAndBackend solver sym bak t st (WEB.Flags fm)
  , ?parserHooks :: CSyn.ParserHooks CLLVM.LLVM
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  ) =>
  GDiag.GreaseLogAction ->
  Proxy arch ->
  GO.SimOpts ->
  bak ->
  Maybe L.Module ->
  Ctx.Assignment (Const String) argTys ->
  C.CFG CLLVM.LLVM blocks argTys ret ->
  IO (PreconditionEnvironment sym bak CLLVM.LLVM argTys)
llvmInitPreconds la _ simOpts _bak llvmMod argNames cfg =
  do
    C.Refl <-
      case C.testEquality ?ptrWidth (knownNat @64) of
        Just r -> pure r
        Nothing -> panic "simulateLlvmCfg" ["Bad pointer width", show ?ptrWidth]

    initArgShapes <-
      let opts = GO.simInitPrecondOpts simOpts
       in getLlvmInitArgShapes la opts llvmMod argNames cfg
    let heuristics =
          if GO.simNoHeuristics simOpts
            then [H.mustFailHeuristic]
            else H.llvmHeuristics la List.++ [H.mustFailHeuristic]
    let addrWidth = MC.addrWidthRepr (Proxy @(CLLVM.ArchWidth arch))
    let fmt = ShapePrint.PrintableShapes addrWidth argNames
    pure $ PreconditionEnvironment initArgShapes fmt heuristics Setup.setup

simulateLlvmCfg ::
  forall sym bak arch solver t st fm argTys ret.
  ( CLM.HasPtrWidth (CLLVM.ArchWidth arch)
  , GUtil.OnlineSolverAndBackend solver sym bak t st (WEB.Flags fm)
  , ?parserHooks :: CSyn.ParserHooks CLLVM.LLVM
  ) =>
  GDiag.GreaseLogAction ->
  GO.SimOpts ->
  bak ->
  WE.FloatModeRepr fm ->
  C.HandleAllocator ->
  CLT.LLVMContext arch ->
  Maybe L.Module ->
  H.InitialMem sym ->
  LLVM.SetupHook sym arch ->
  -- | An optional startup override to run just before the entrypoint function.
  Maybe (C.SomeCFG CLLVM.LLVM argTys (C.StructType argTys)) ->
  -- | The CFG of the user-requested entrypoint function.
  C.SomeCFG CLLVM.LLVM argTys ret ->
  IO GOut.BatchStatus
simulateLlvmCfg la simOpts bak fm halloc llvmCtx llvmMod initMem setupHook mbStartupOvCfg scfg@(C.SomeCFG cfg) = do
  doLog la (Diag.TargetCFG cfg)

  let memVar = CLT.llvmMemVar llvmCtx
  (execFeats, profLogTask) <-
    llvmExecFeats
      @(GLP.GreaseLLVMPersonality LDebug.LLVMCommand sym ret)
      la
      bak
      simOpts
      memVar

  C.Refl <-
    case C.testEquality ?ptrWidth (knownNat @64) of
      Just r -> pure r
      Nothing -> panic "simulateLlvmCfg" ["Bad pointer width", show ?ptrWidth]

  let argTys = C.cfgArgTypes cfg
      -- Display the arguments as though they are unnamed LLVM virtual registers.
      argNames = imapFC (\i _ -> Const ('%' : show i)) argTys

  let dbgOpts = GO.simDebugOpts simOpts
  let dbgCmdExt = LDebug.llvmCommandExt
  dbgCtx <- initDebugger la dbgOpts dbgCmdExt (C.cfgReturnType cfg)

  let ?recordLLVMAnnotation = \_ _ _ -> pure ()

  initArgShapes <-
    let opts = GO.simInitPrecondOpts simOpts
     in getLlvmInitArgShapes la opts llvmMod argNames cfg

  let bounds = GO.simBoundsOpts simOpts
  let addrWidth = MC.addrWidthRepr (Proxy @(CLLVM.ArchWidth arch))

  result <-
    withMemOptions simOpts $ do
      let valueNames = Ctx.generate (Ctx.size argTys) (\i -> GSetup.ValueName ("arg" <> show i))
      let typeCtx = llvmCtx ^. CLT.llvmTypeCtx
      let dl = CLTC.llvmDataLayout typeCtx
      -- See comment above on heuristics in 'simulateMacawCfg'
      let heuristics =
            if GO.simNoHeuristics simOpts
              then [H.mustFailHeuristic]
              else H.llvmHeuristics la List.++ [H.mustFailHeuristic]

      GRef.refinementLoop la bounds initArgShapes (ShapePrint.PrintableShapes addrWidth argNames) $ \argShapes ->
        GRef.refineOnce
          la
          simOpts
          halloc
          bak
          fm
          dl
          valueNames
          argNames
          argTys
          GSetup.setup
          argShapes
          initMem
          memVar
          heuristics
          execFeats
          $ \toConc setupMem initFs args -> do
            let p =
                  GLP.GreaseLLVMPersonality
                    { GLP._dbgContext = dbgCtx
                    , GLP._toConcretize = toConc
                    }
            LLVM.initState
              bak
              la
              (CLLVM.llvmExtensionImpl ?memOpts)
              p
              halloc
              (GO.simErrorSymbolicFunCalls simOpts)
              setupMem
              -- TODO: just take the whole initFs
              (GSIO.initFs initFs)
              (GSIO.initFsGlobals initFs)
              (GSIO.initFsOverride initFs)
              llvmCtx
              setupHook
              (GSetup.argVals args)
              mbStartupOvCfg
              scfg

  res <-
    case result of
      GRef.RefinementBug b cData ->
        pure (GOut.BatchBug (toBatchBug fm MM.Addr64 argNames argTys initArgShapes b cData))
      GRef.RefinementCantRefine b ->
        pure (GOut.BatchCantRefine b)
      GRef.RefinementItersExceeded ->
        pure GOut.BatchItersExceeded
      GRef.RefinementNoHeuristic errs -> do
        maybeBug <- checkMustFail bak errs
        case maybeBug of
          Just bug -> pure (GOut.BatchBug bug)
          Nothing ->
            pure $
              GOut.BatchCouldNotInfer $
                errs <&> \noHeuristic ->
                  toFailedPredicate fm MM.Addr64 argNames argTys initArgShapes noHeuristic
      GRef.RefinementSuccess _argShapes -> pure (GOut.BatchChecks Map.empty)
  traverse_
    cancel
    profLogTask
  pure
    res

simulateLlvmCfgs ::
  CLM.HasPtrWidth (CLLVM.ArchWidth arch) =>
  (?parserHooks :: CSyn.ParserHooks CLLVM.LLVM) =>
  GDiag.GreaseLogAction ->
  GO.SimOpts ->
  C.HandleAllocator ->
  CLT.LLVMContext arch ->
  Maybe L.Module ->
  (forall sym bak. CB.IsSymBackend sym bak => bak -> IO (H.InitialMem sym)) ->
  (forall sym. LLVM.SetupHook sym arch) ->
  Map EP.Entrypoint (EP.EntrypointCfgs (C.AnyCFG CLLVM.LLVM)) ->
  IO Results
simulateLlvmCfgs la simOpts halloc llvmCtx llvmMod mkMem setupHook cfgs = do
  let fm = WE.FloatRealRepr
  withSolverOnlineBackend (GO.simSolver simOpts) fm globalNonceGenerator $ \bak -> do
    initMem <- mkMem bak
    results <- do
      let nEntries = Map.size cfgs
      iforM (Map.toList cfgs) $ \i (entry, entrypointCfgs) ->
        analyzeEntrypoint la entry i nEntries $ do
          EP.EntrypointCfgs
            { EP.entrypointStartupOv = mbStartupOv
            , EP.entrypointCfg = C.AnyCFG entrypointCfg'
            } <-
            pure entrypointCfgs
          mbStartupOvSomeCfg <-
            for (EP.startupOvCfg <$> mbStartupOv) $ \(C.AnyCFG startupOvCfg') -> do
              let expectedArgTys = C.cfgArgTypes entrypointCfg'
              let expectedRetTy = C.StructRepr expectedArgTys
              let actualArgTys = C.cfgArgTypes startupOvCfg'
              let actualRetTy = C.cfgReturnType startupOvCfg'
              Refl <-
                case testEquality expectedArgTys actualArgTys of
                  Just r -> pure r
                  Nothing ->
                    userErr la $
                      PP.vcat
                        [ "Startup override must have the same argument types as the entrypoint function"
                        , "Entrypoint function argument types: " <> PP.viaShow expectedArgTys
                        , "Startup override argument types: " <> PP.viaShow actualArgTys
                        ]
              Refl <-
                case testEquality expectedRetTy actualRetTy of
                  Just r -> pure r
                  Nothing ->
                    userErr la $
                      PP.vcat
                        [ "Startup override must return a struct containing the argument types of the entrypoint function"
                        , "Entrypoint function argument types: " <> PP.viaShow expectedArgTys
                        , "Startup override return type: " <> PP.viaShow actualRetTy
                        ]
              pure $ C.SomeCFG startupOvCfg'
          status <- simulateLlvmCfg la simOpts bak fm halloc llvmCtx llvmMod initMem setupHook mbStartupOvSomeCfg (C.SomeCFG entrypointCfg')
          let result =
                GOut.Batch
                  { GOut.batchStatus = status
                  , GOut.batchLoadOffset = 0x0
                  }
          pure (entry, result)

    pure (Results (Map.fromList results))

simulateLlvmSyntax ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateLlvmSyntax simOpts la = do
  halloc <- C.newHandleAllocator
  mvar <- CLM.mkMemVar "grease:memmodel" halloc
  let mkMem = \_ -> H.InitialMem <$> CLM.emptyMem DataLayout.LittleEndian
  let ?ptrWidth = knownNat @64
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  prog <- doParse la halloc (GO.simProgPath simOpts)
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  regCfgs <- entrypointCfgMap la halloc prog (GO.simEntryPoints simOpts)
  let cfgs = Map.map (fmap toSsaAnyCfg) regCfgs
  let dl = DataLayout.defaultDataLayout
  let (_errs, tyCtx) = CLTC.mkTypeContext dl IntMap.empty []
  let llvmCtx =
        CLT.LLVMContext
          { CLT.llvmArch = CLLVM.X86Repr ?ptrWidth
          , CLT.llvmPtrWidth = \k -> k ?ptrWidth
          , CLT.llvmMemVar = mvar
          , CLT._llvmTypeCtx = tyCtx
          , CLT.llvmGlobalAliases = Map.empty
          , CLT.llvmFunctionAliases = Map.empty
          }
  sexpOvs <- loadLLVMSExpOvs la (GO.simOverrides simOpts) halloc mvar
  let llvmMod = Nothing
  let setupHook :: forall sym arch. LLVM.SetupHook sym arch
      setupHook =
        LLVM.syntaxSetupHook la sexpOvs (GO.simSkipFuns simOpts) prog cfgs $
          GLO.CantResolveOverrideCallback $
            \nm _hdl -> liftIO (declaredFunNotFound la nm)
  simulateLlvmCfgs la simOpts halloc llvmCtx llvmMod mkMem setupHook cfgs

-- | Helper, not exported
--
-- Parse bitcode from a 'FilePath', logging any parse warnings and calling
-- 'malformedLlvm' on error.
parseBitcode :: GDiag.GreaseLogAction -> FilePath -> IO L.Module
parseBitcode la path =
  parseBitCodeFromFileWithWarnings path >>= \case
    Left err ->
      malformedLlvm la (PP.pretty (formatError err))
    Right (m, warns) -> do
      Monad.unless (Seq.null warns) $
        doLog la (Diag.BitcodeParseWarnings warns)
      pure m

simulateLlvm ::
  CLT.TranslationOptions ->
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateLlvm transOpts simOpts la = do
  llvmMod <- parseBitcode la (GO.simProgPath simOpts)
  halloc <- C.newHandleAllocator
  mvar <- liftIO $ CLM.mkMemVar "grease:memmodel" halloc
  C.Some @_ @_ @arch trans <- do
    let ?transOpts = transOpts
    CLT.translateModule halloc mvar llvmMod

  entries <-
    if List.null (GO.simEntryPoints simOpts)
      then do
        doLog la Diag.NoEntrypoints
        let convertSymbol (L.Symbol s) =
              EP.entrypointNoStartupOv $ EP.EntrypointSymbolName $ Text.pack s
        pure (List.map (convertSymbol . L.defName) (L.modDefines llvmMod))
      else pure (GO.simEntryPoints simOpts)

  let llvmCtxt = trans ^. CLT.transContext
  CLT.llvmPtrWidth llvmCtxt $ \ptrW -> CLM.withPtrWidth ptrW $ do
    let mkMem :: forall sym bak. CB.IsSymBackend sym bak => bak -> IO (H.InitialMem sym)
        mkMem bak =
          let ?lc = llvmCtxt ^. CLT.llvmTypeCtx
              ?recordLLVMAnnotation = \_ _ _ -> pure ()
           in withMemOptions simOpts $ do
                unpopulated <- CLLVM.initializeAllMemory bak llvmCtxt llvmMod
                initMem <-
                  case GO.simMutGlobs simOpts of
                    GO.Initialized ->
                      CLLVM.populateAllGlobals bak (trans ^. CLT.globalInitMap) unpopulated
                    GO.Symbolic ->
                      unsupported la "GREASE does not yet support symbolic globals for LLVM"
                    GO.Uninitialized ->
                      CLLVM.populateConstGlobals bak (trans ^. CLT.globalInitMap) unpopulated
                pure (H.InitialMem initMem)

    let ?parserHooks = llvmParserHooks emptyParserHooks mvar
    cfgs <-
      fmap Map.fromList $
        forM entries $ \entry -> do
          case EP.entrypointLocation entry of
            EP.EntrypointAddress{} -> nonSymbolEntrypoint la
            EP.EntrypointCoreDump{} -> nonSymbolEntrypoint la
            EP.EntrypointSymbolName nm ->
              CLT.getTranslatedCFG trans (L.Symbol (Text.unpack nm)) >>= \case
                Just (_decl, cfg, warns) -> do
                  forM_ warns $ \warn ->
                    LJ.writeLog la (GDiag.LLVMSetupHookDiagnostic (LDiag.LLVMTranslationWarning warn))
                  mbStartupOv <-
                    case EP.entrypointStartupOvPath entry of
                      Nothing -> pure Nothing
                      Just startupOvPath -> do
                        result <- EP.parseEntrypointStartupOv halloc startupOvPath
                        case result of
                          -- See Note [Explicitly listed errors]
                          Left err@EP.StartupOvParseError{} ->
                            userErr la (PP.pretty err)
                          Left err@EP.StartupOvCFGNotFound{} ->
                            userErr la (PP.pretty err)
                          Right startupOv -> pure $ Just startupOv
                  let mbStartupOvSsa = fmap toSsaAnyCfg <$> mbStartupOv
                  let entrypointCfgs =
                        EP.EntrypointCfgs
                          { EP.entrypointStartupOv = mbStartupOvSsa
                          , EP.entrypointCfg = cfg
                          }
                  pure (entry, entrypointCfgs)
                Nothing -> userErr la (PP.pretty ("Could not find function: " <> nm))

    sexpOvs <- loadLLVMSExpOvs la (GO.simOverrides simOpts) halloc mvar
    let setupHook :: forall sym. LLVM.SetupHook sym arch
        setupHook =
          LLVM.moduleSetupHook la sexpOvs (GO.simSkipFuns simOpts) trans cfgs $
            GLO.CantResolveOverrideCallback $
              \nm _hdl -> liftIO (declaredFunNotFound la nm)

    simulateLlvmCfgs la simOpts halloc llvmCtxt (Just llvmMod) mkMem setupHook cfgs

-- | A 'GDiag.GreaseLogAction' that diagnostics directly to @stderr@.
logAction :: Severity -> GDiag.GreaseLogAction
logAction sev = LJ.LogAction $ \diag ->
  when (GDiag.severity diag <= sev) $
    GDiag.log (PP.pretty diag)

simulateARMSyntax ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateARMSyntax simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @32
  halloc <- C.newHandleAllocator
  archCtx <- armCtx halloc Nothing (GO.simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts AArch32Syn.aarch32ParserHooks

simulateMacawRaw ::
  forall arch.
  ( C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  ) =>
  GDiag.GreaseLogAction ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  C.HandleAllocator ->
  GMA.ArchContext arch ->
  GO.SimOpts ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  IO Results
simulateMacawRaw la memory halloc archCtx simOpts parserHooks =
  do
    let entries = GO.simEntryPoints simOpts
    let
      mbOrError :: PP.Doc Void -> Maybe a -> IO a
      mbOrError doc = Maybe.maybe (userErr la doc) pure
    -- When loading a binary in raw mode
    -- symbol entrypoints cannot be used
    let
      numericToHex :: Integral a => a -> String
      numericToHex n = "0x" List.++ showHex n ""
    let getAddressEntrypoint ent = do
          (txt, fpath) <- case ent of
            EP.Entrypoint{EP.entrypointLocation = EP.EntrypointAddress x, EP.entrypointStartupOvPath = override} -> pure (x, override)
            e ->
              userErr
                la
                ( "An entrypoint was provided that was not an address in raw mode:"
                    PP.<+> PP.pretty e
                )
          intAddr <-
            mbOrError
              ( "Provided address was not parseable:"
                  PP.<+> PP.pretty txt
              )
              (readMaybe $ Text.unpack txt :: Maybe Integer)
          addr <-
            mbOrError
              ( "A provided address entrypoint could not be resolved to an address in memory:"
                  PP.<+> (PP.pretty $ numericToHex intAddr)
              )
              (MM.resolveAbsoluteAddr memory $ fromIntegral intAddr)
          pure (fpath, txt, addr)

    entryAddrs :: [(Maybe FilePath, Text.Text, MM.MemSegmentOff (MC.RegAddrWidth (MC.ArchReg arch)))] <- forM entries getAddressEntrypoint
    let ?parserHooks = machineCodeParserHooks (Proxy @arch) parserHooks
    cfgs <-
      fmap Map.fromList $ forM entryAddrs $ \(mbOverride, entText, entAddr) -> do
        C.Reg.SomeCFG cfg <- discoverFunction la halloc archCtx memory Map.empty Map.empty entAddr
        mbStartupOv <-
          case mbOverride of
            Nothing -> pure Nothing
            Just startupOvPath -> do
              result <- EP.parseEntrypointStartupOv halloc startupOvPath
              case result of
                -- See Note [Explicitly listed errors]
                Left err@EP.StartupOvParseError{} -> userErr la (PP.pretty err)
                Left err@EP.StartupOvCFGNotFound{} -> userErr la (PP.pretty err)
                Right startupOv -> pure $ Just startupOv
        let entrypointCfgs =
              EP.EntrypointCfgs
                { EP.entrypointStartupOv = mbStartupOv
                , EP.entrypointCfg = C.Reg.AnyCFG cfg
                }
        pure
          ( EP.Entrypoint{EP.entrypointLocation = EP.EntrypointAddress entText, EP.entrypointStartupOvPath = mbOverride}
          , EP.MacawEntrypointCfgs entrypointCfgs (Just entAddr)
          )
    addrOvs <- loadAddrOvs la archCtx halloc memory simOpts
    let setupHook :: forall sym. Macaw.SetupHook sym arch
        setupHook =
          let errCb = GLO.CantResolveOverrideCallback $ \nm _hdl -> liftIO (declaredFunNotFound la nm)
           in Macaw.binSetupHook errCb addrOvs cfgs
    let dl = macawDataLayout archCtx
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
    simulateMacawCfgs
      la
      halloc
      macawCfgConfig
      archCtx
      simOpts
      setupHook
      addrOvs
      cfgs

-- | Given an arch runs the raw loader
-- loading a binary in a single segment at a fixed offset
simulateRawArch ::
  forall arch.
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth
      (MC.RegAddrWidth (MC.ArchReg arch))
  , KnownNat
      (MC.RegAddrWidth (MC.ArchReg arch))
  , C.IsSyntaxExtension (Symbolic.MacawExt arch)
  , Symbolic.SymArchConstraints arch
  , Integral
      (Elf.ElfWordType (MC.RegAddrWidth (MC.ArchReg arch)))
  , Show (GMA.ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  ) =>
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  C.HandleAllocator ->
  CSyn.ParserHooks (Symbolic.MacawExt arch) ->
  GMA.ArchContext arch ->
  MM.Endianness ->
  IO Results
simulateRawArch simOpts la halloc hooks archCtx end = do
  bs <- BS.readFile (GO.simProgPath simOpts)
  let ldr = Loader.RawBin bs end
  -- TODO: we should allow setting a load offset via an option
  let opts = MML.LoadOptions{MML.loadOffset = Just $ GO.simRawBinaryOffset simOpts}
  lded <- Loader.loadBinary @arch opts ldr
  simulateMacawRaw la (Loader.memoryImage lded) halloc archCtx simOpts hooks

simulateARMRaw :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulateARMRaw simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @32
  halloc <- C.newHandleAllocator
  archCtx <- armCtx halloc Nothing (GO.simStackArgumentSlots simOpts)
  simulateRawArch simOpts la halloc AArch32Syn.aarch32ParserHooks archCtx MM.LittleEndian

-- TODO(#287) We cannot load ppc64 raw because of https://github.com/GaloisInc/macaw/issues/415 we
-- do not have a TOC
simulatePPC32Raw :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulatePPC32Raw simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @32
  halloc <- C.newHandleAllocator
  archCtx <- ppc32Ctx Nothing (GO.simStackArgumentSlots simOpts)
  simulateRawArch simOpts la halloc PPCSyn.ppc32ParserHooks archCtx MM.LittleEndian

simulateX86Raw :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulateX86Raw simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @64
  halloc <- C.newHandleAllocator
  archCtx <- x86Ctx halloc Nothing (GO.simStackArgumentSlots simOpts)
  simulateRawArch simOpts la halloc X86Syn.x86ParserHooks archCtx MM.LittleEndian

doLoad ::
  ( 16 C.<= MC.ArchAddrWidth arch
  , Symbolic.SymArchConstraints arch
  , Loader.BinaryLoader arch (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch))
  , ?memOpts :: CLM.MemOptions
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  ) =>
  GDiag.GreaseLogAction ->
  proxy arch ->
  [EP.Entrypoint] ->
  Permissions ->
  Elf.ElfHeaderInfo (MC.ArchAddrWidth arch) ->
  IO (Load.LoadedProgram arch)
doLoad la _proxy entries perms elf = do
  let usrErr = userErr la . PP.pretty
  let badElf = malformedElf la . PP.pretty
  Load.load la entries perms elf
    >>= \case
      Right ok -> pure ok
      -- See Note [Explicitly listed errors]
      -- User errors
      Left e@Load.UnsupportedObjectFile{} -> usrErr e
      Left e@Load.EntrypointNotFound{} -> usrErr e
      Left e@Load.InvalidEntrypointAddress{} -> usrErr e
      -- Bad inputs
      Left e@Load.ElfParseError{} -> badElf e
      Left e@Load.DynamicFunctionAddressUnresolvable{} -> badElf e
      Left e@Load.CoreDumpClassMismatch{} -> badElf e
      Left e@Load.CoreDumpNotesError{} -> badElf e
      Left e@Load.CoreDumpPcError{} -> badElf e
      Left e@Load.CoreDumpAddressUnresolvable{} -> badElf e
      Left e@Load.CoreDumpNoEntrypoint{} -> badElf e

simulateARM :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulateARM simOpts la = do
  let ?ptrWidth = knownNat @32
  let proxy = Proxy @ARM.ARM
  (perms, elf) <- readElfHeaderInfo la proxy (GO.simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- doLoad la proxy (GO.simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds la (Load.progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- armCtx halloc (Just starttext) (GO.simStackArgumentSlots simOpts)
    simulateMacaw la halloc elf loadedProg (Just ARM.armPLTStubInfo) archCtx txtBounds simOpts AArch32Syn.aarch32ParserHooks

simulatePPC32Syntax ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulatePPC32Syntax simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @32
  halloc <- C.newHandleAllocator
  -- We don't have an ELF file on hand, so there's no reasonable concrete
  -- default value to put on the stack as the return address, so we use fresh,
  -- symbolic bytes (Nothing).
  archCtx <- ppc32Ctx Nothing (GO.simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts PPCSyn.ppc32ParserHooks

-- | Currently, @.ppc64.cbl@ files are not supported, as the @macaw-ppc@ API
-- does not make it straightforward to create a PPC64 'MI.ArchitectureInfo'
-- value without having access to a full binary. See
-- <https://github.com/GaloisInc/macaw/issues/415> for the full details.
simulatePPC64Syntax ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulatePPC64Syntax _simOpts la =
  unsupported la "*.ppc64.cbl files are not currently supported."

simulatePPC32 :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulatePPC32 simOpts la = do
  let ?ptrWidth = knownNat @32
  let proxy = Proxy @PPC.PPC32
  (perms, elf) <- readElfHeaderInfo la proxy (GO.simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- doLoad la proxy (GO.simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds la (Load.progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- ppc32Ctx (Just starttext) (GO.simStackArgumentSlots simOpts)
    -- See Note [Subtleties of resolving PLT stubs] (Wrinkle 3: PowerPC) in
    -- Grease.Macaw.PLT for why we use Nothing here.
    let ppcPltStubInfo = Nothing
    simulateMacaw la halloc elf loadedProg ppcPltStubInfo archCtx txtBounds simOpts PPCSyn.ppc32ParserHooks

simulatePPC64 :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulatePPC64 simOpts la = do
  let ?ptrWidth = knownNat @64
  let proxy = Proxy @PPC.PPC64
  (perms, elf) <- readElfHeaderInfo la proxy (GO.simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- doLoad la proxy (GO.simEntryPoints simOpts) perms elf
    txtBounds@(starttext, _) <- textBounds la (Load.progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- ppc64Ctx (Just starttext) (GO.simStackArgumentSlots simOpts) (Load.progLoadedBinary loadedProg)
    -- See Note [Subtleties of resolving PLT stubs] (Wrinkle 3: PowerPC) in
    -- Grease.Macaw.PLT for why we use Nothing here.
    let ppcPltStubInfo = Nothing
    simulateMacaw la halloc elf loadedProg ppcPltStubInfo archCtx txtBounds simOpts PPCSyn.ppc64ParserHooks

simulateX86Syntax ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateX86Syntax simOpts la = withMemOptions simOpts $ do
  let ?ptrWidth = knownNat @64
  halloc <- C.newHandleAllocator
  -- We don't have an ELF file on hand, so there's no reasonable concrete
  -- default value to put on the stack as the return address, so we use fresh,
  -- symbolic bytes (Nothing).
  archCtx <- x86Ctx halloc Nothing (GO.simStackArgumentSlots simOpts)
  simulateMacawSyntax la halloc archCtx simOpts X86Syn.x86ParserHooks

simulateX86 :: GO.SimOpts -> GDiag.GreaseLogAction -> IO Results
simulateX86 simOpts la = do
  let ?ptrWidth = knownNat @64
  let proxy = Proxy @X86.X86_64
  (perms, elf) <- readElfHeaderInfo la proxy (GO.simProgPath simOpts)
  halloc <- C.newHandleAllocator
  withMemOptions simOpts $ do
    loadedProg <- doLoad la proxy (GO.simEntryPoints simOpts) perms elf
    txtBounds@(startText, _) <- textBounds la (Load.progLoadOptions loadedProg) elf
    -- Return address must be in .text to satisfy the `in-text` requirement.
    archCtx <- x86Ctx halloc (Just startText) (GO.simStackArgumentSlots simOpts)
    simulateMacaw la halloc elf loadedProg (Just X86.x86_64PLTStubInfo) archCtx txtBounds simOpts X86Syn.x86ParserHooks

simulateElf ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateElf simOpts la = do
  bs <- liftIO $ BS.readFile (GO.simProgPath simOpts)
  case Elf.decodeElfHeaderInfo bs of
    Right (Elf.SomeElf hdr) ->
      case (Elf.headerClass (Elf.header hdr), Elf.headerMachine (Elf.header hdr)) of
        (Elf.ELFCLASS32, Elf.EM_ARM) -> simulateARM simOpts la
        (Elf.ELFCLASS32, Elf.EM_PPC) -> simulatePPC32 simOpts la
        (Elf.ELFCLASS64, Elf.EM_PPC64) -> simulatePPC64 simOpts la
        (Elf.ELFCLASS64, Elf.EM_X86_64) -> simulateX86 simOpts la
        (_, mach) -> unsupported la ("unsupported ELF architecture: " <> PP.viaShow mach)
    Left _ -> userErr la ("User error: expected ELF binary, but found non-ELF file at " <> PP.pretty (GO.simProgPath simOpts))

simulateFile ::
  GO.SimOpts ->
  GDiag.GreaseLogAction ->
  IO Results
simulateFile opts la =
  let path = GO.simProgPath opts
   in -- This list also appears in {overrides,sexp}.md
      if
        | GO.simRawBinaryMode opts ->
            if
              | ".armv7l.elf" `List.isSuffixOf` path -> simulateARMRaw opts la
              | ".ppc32.elf" `List.isSuffixOf` path -> simulatePPC32Raw opts la
              | ".x64.elf" `List.isSuffixOf` path -> simulateX86Raw opts la
              | otherwise -> userErr la "Unsupported file suffix for raw binary mode"
        | ".armv7l.elf" `List.isSuffixOf` path -> simulateARM opts la
        | ".ppc32.elf" `List.isSuffixOf` path -> simulatePPC32 opts la
        | ".ppc64.elf" `List.isSuffixOf` path -> simulatePPC64 opts la
        | ".x64.elf" `List.isSuffixOf` path -> simulateX86 opts la
        | ".bc" `List.isSuffixOf` path -> simulateLlvm CLT.defaultTranslationOptions opts la
        | ".armv7l.cbl" `List.isSuffixOf` path -> simulateARMSyntax opts la
        | ".ppc32.cbl" `List.isSuffixOf` path -> simulatePPC32Syntax opts la
        | ".ppc64.cbl" `List.isSuffixOf` path -> simulatePPC64Syntax opts la
        | ".x64.cbl" `List.isSuffixOf` path -> simulateX86Syntax opts la
        | ".llvm.cbl" `List.isSuffixOf` path -> simulateLlvmSyntax opts la
        | otherwise -> simulateElf opts la

-- | Also used in the test suite
logResults :: GDiag.GreaseLogAction -> Results -> IO ()
logResults la (Results results) =
  forM_ (Map.toList results) $ \(entrypoint, result) ->
    doLog la $
      Diag.AnalyzedEntrypoint
        (EP.entrypointLocation entrypoint)
        (GOut.batchStatus result)

-- | Run GREASE in a top-level exception handler.
--
-- This exception handler catches known/expected exceptions, forwards their
-- messages to the 'GDiag.GreaseLogAction', and exits. For unknown/unexpected
-- exceptions, it upgrades them to panics and requests that the user file a bug
-- about them.
--
-- This is part of an overall error-handling strategy that involves avoiding
-- uncaught exceptions. See @doc/dev/errors.md@.
withTopLevelExceptionHandler :: GDiag.GreaseLogAction -> IO a -> IO a
withTopLevelExceptionHandler la act =
  X.catches
    act
    [ X.Handler @_ @_ @X.IOException $ \e -> do
        doLog la (Diag.Exception (PP.viaShow e))
        Exit.exitFailure
    , X.Handler @_ @_ @(Panic.Panic Grease) $ \e -> do
        doLog la (Diag.Exception (PP.viaShow e))
        Exit.exitFailure
    , X.Handler @_ @_ @Exit.ExitCode $ \e ->
        X.throw e
    , X.Handler @_ @_ @X.SomeException $ \e -> do
        let msg = "Uncaught exception"
        r <- X.try @_ @(Panic.Panic Grease) (panic "main" [msg, show e])
        case r of
          Right () -> Exit.exitFailure -- impossible
          Left e' -> do
            doLog la (Diag.Exception (PP.viaShow e'))
            Exit.exitFailure
    ]

main :: IO ()
main = do
  parsedOpts <- optsFromArgs
  let la :: GDiag.GreaseLogAction
      la = logAction (GO.optsVerbosity parsedOpts)
  withTopLevelExceptionHandler la $ do
    let simOpts = GO.optsSimOpts parsedOpts
    rs@(Results results) <-
      simulateFile simOpts la
    if GO.optsJSON parsedOpts
      then forM_ (Map.elems results) $ Text.IO.putStrLn . GOut.renderJSON
      else logResults la rs
