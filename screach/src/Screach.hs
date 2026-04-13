{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- TODO(internal#141): Stop using exceptions
{- HLINT ignore "Use Either" -}

module Screach (
  defaultLogAction,
  runScreach,
  loadElfFromConfig,
) where

import Control.Exception (throw)
import Control.Exception qualified as X
import Control.Lens (to, (&), (.~), (^.))
import Control.Monad qualified as Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ElfEdit qualified as Elf
import Data.ElfEdit.Ecfs qualified as Ecfs
import Data.Functor.Const (Const (Const))
import Data.IORef qualified as IORef
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Macaw.Architecture.Info qualified as MAI
import Data.Macaw.BinaryLoader qualified as Loader
import Data.Macaw.BinaryLoader.ELF qualified as MBLE
-- BinaryLoader instance
import Data.Macaw.BinaryLoader.X86 ()
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Memory.LoadCommon qualified as LC
import Data.Macaw.Symbolic qualified as MS
import Data.Macaw.Symbolic.Debug qualified as MDebug
import Data.Macaw.Symbolic.Memory.Lazy qualified as MSML
import Data.Macaw.Symbolic.Syntax qualified as MSS
import Data.Macaw.X86 qualified as MX86
-- GenArchInfo instance
import Data.Macaw.X86.Symbolic ()
import Data.Macaw.X86.Symbolic.Syntax qualified as MX86Syn
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Nonce qualified as Nonce
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock qualified as Time
import Data.Time.Format qualified as Time
import Data.Type.Equality (testEquality)
import Data.Void (Void, absurd)
import Data.Word (Word64)
import GHC.IO.Handle (Handle)
import GHC.IO.Handle.FD (withFile)
import Grease.Reachability.AnalysisLoc qualified as GAL
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcArgs (..))
import Grease.Concretize qualified as Conc
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic (GreaseLogAction)
import Grease.Diagnostic qualified as GD
import Grease.Diagnostic.Severity qualified as GD
import Grease.Entrypoint qualified as GE
import Grease.ExecutionFeatures qualified as GEF
import Grease.Reachability.GoalEvaluator qualified as GGE
import Grease.Reachability.Verify qualified as GRV
import Grease.Heuristic qualified as GH
import Grease.Macaw qualified as GM
import Grease.Macaw.Arch (ArchContext, ArchReloc)
import Grease.Macaw.Arch qualified as Arch
import Grease.Macaw.Arch.X86 (x64RelocSupported, x86Ctx)
import Grease.Macaw.Discovery qualified as GMD
import Grease.Macaw.Entrypoint qualified as GME
import Grease.Macaw.Load qualified as GL
import Grease.Macaw.Overrides qualified as GMO
import Grease.Macaw.Overrides.Address qualified as GMOA
import Grease.Macaw.Overrides.Builtin (builtinStubsOverrides, reachabilityBuiltinOverrides)
import Grease.Macaw.Overrides.SExp qualified as GMOS
import Grease.Macaw.PLT qualified as GMP
import Grease.Macaw.ResolveCall qualified as ResolveCall
import Grease.Macaw.SetupHook qualified as GMS
import Grease.Macaw.Shapes qualified as GMShp
import Grease.Macaw.SimulatorHooks qualified as GMSH
import Grease.Macaw.SimulatorState qualified as GMSS
import Grease.Options qualified as GO
import Grease.Output (renderJSON)
import Grease.Personality qualified as GP
import Grease.Pretty (prettyPtrFnMap)
import Grease.Refine qualified as GR
import Grease.Refine.Diagnostic qualified as RDiag
import Grease.Refine.RefinementData qualified as GR
import Grease.Scheduler qualified as Sched
import Grease.Setup qualified as GS
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag qualified as Shape
import Grease.Shape.Parse qualified as Parse
import Grease.Shape.Parse qualified as Shape
import Grease.Shape.Pointer qualified as Shape
import Grease.Solver qualified as Solver
import Grease.SymIO qualified as GSIO
import Grease.Syntax qualified as Syntax
import Grease.Syscall (builtinGenericSyscalls)
import Grease.ValueName (ValueName, getValueName)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.CFG.Core qualified as CCC
import Lang.Crucible.CFG.Extension qualified as CCE
import Lang.Crucible.CFG.Reg qualified as CCR
import Lang.Crucible.CFG.SSAConversion as CCS
import Lang.Crucible.Debug qualified as Dbg
import Lang.Crucible.FunctionHandle qualified as CFH
import Lang.Crucible.LLVM.DataLayout qualified as CLD
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.TypeContext qualified as CLTC
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.RecordAndReplay qualified as RR
import Lang.Crucible.Simulator.RecordAndReplay qualified as SR
import Lang.Crucible.Syntax.Concrete qualified as CSC
import Lumberjack qualified as LJ
import Numeric (showHex)
import Numeric.Natural (Natural)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Screach.AnalysisLoc qualified as SAL
import Screach.CallGraph qualified as CG
import Screach.Config qualified as Conf
import Screach.Diagnostic (Diagnostic (RunDiagnostic), ScreachLogAction)
import Screach.Diagnostic qualified as SD
import Screach.Distance qualified as Dist
import Screach.Ecfs qualified as SE
import Screach.LoadedELF
import Screach.LocationExecutionFeature qualified as LocLog
import Screach.Panic (panic)
import Screach.Personality qualified as SP
import Screach.RefineFeature qualified as RFT
import Screach.ResolveCall (ecfsLookupFunctionHandleDispatch)
import Screach.Run.Diagnostic qualified as Diag
import Screach.ShortestDistanceScheduler qualified as SDSE
import System.Directory (Permissions, getPermissions)
import System.Exit qualified as Exit
import System.FilePath (isExtensionOf)
import System.IO (IOMode (WriteMode), stderr)
import What4.Config qualified as W4C
import What4.Expr.Builder qualified as WEB
import What4.FloatMode qualified as W4
import What4.FunctionName as WFN
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL
import What4.Protocol.Online qualified as WPO
import Prelude hiding (log, userError)

{- Note [Explicitly listed errors]

In this module, we frequently explicitly list all constructors of an error type,
even when performing the same action for many or all of them. This is so that
we get a compile error when new error constructors are added and can make an
informed judgment about whether they are in fact user errors, internal Screach
errors, malformed inputs, etc.

-}

doLog :: MonadIO m => ScreachLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (RunDiagnostic diag)

malformedElf :: ScreachLogAction -> PP.Doc Void -> IO a
malformedElf la doc = do
  doLog la (Diag.MalformedElf doc)
  Exit.exitFailure

unsupported :: ScreachLogAction -> PP.Doc Void -> IO a
unsupported la doc = do
  doLog la (Diag.Unsupported doc)
  Exit.exitFailure

userError :: ScreachLogAction -> PP.Doc Void -> IO a
userError la doc = do
  doLog la (Diag.UserError doc)
  Exit.exitFailure

doParse ::
  ( ?parserHooks :: CSC.ParserHooks ext
  , CCE.IsSyntaxExtension ext
  ) =>
  ScreachLogAction ->
  CFH.HandleAllocator ->
  FilePath ->
  IO (CSC.ParsedProgram ext)
doParse la halloc path = do
  r <- Syntax.parseProgram halloc path
  case r of
    -- See Note [Explicitly listed errors]
    Left e@Syntax.SExpressionParseError{} -> userError la (PP.pretty e)
    Left e@Syntax.SyntaxParseError{} -> userError la (PP.pretty e)
    Right p -> pure p

data UserError
  = AddressOverrideError (PP.Doc Void)
  | BadElf32Bit FilePath
  | NotAnElf FilePath
  | -- | The pointer width in bits
    UnsupportedElfPointerWidth
      Natural
  | UnknownEcfsPltStub
      -- | The PLT stub address
      Word64
      -- | The PLT stub name
      Text.Text
  | SyntaxProgUnknownEntrypointSymbol WFN.FunctionName
  | -- | The entrypoint address
    SyntaxProgWithEntrypointAddr
      Word64
  | -- | The target address
    SyntaxProgWithTargetAddr
      Word64
  | SyntaxProgExterns
  | InitialPreconditionParseError Shape.ParseError
  | InitialPreconditionTypeMismatch Shape.TypeMismatch
  | TargetOverrideNoAddress FilePath
  | TargetAddressWithoutContainingFunction
  | AvoidSymbolDidNotResolve WFN.FunctionName

instance X.Exception UserError

instance Show UserError where
  show = ("User error: " ++) . show . PP.pretty

instance PP.Pretty UserError where
  pretty =
    \case
      AddressOverrideError err ->
        absurd <$> err
      BadElf32Bit path ->
        "Expected x86_64 ELF, but found 32-bit ELF at " <> PP.pretty path
      NotAnElf path ->
        "Expected x86_64 ELF, but found non-ELF file at " <> PP.pretty path
      UnsupportedElfPointerWidth ptrWidth ->
        "Cannot analyze ELF file with a pointer width of"
          PP.<+> PP.pretty ptrWidth
          PP.<+> "bits"
      UnknownEcfsPltStub addr name ->
        "Could not resolve PLT stub address"
          PP.<+> PP.pretty (showHex addr "")
          <> PP.colon
          PP.<+> PP.pretty name
      SyntaxProgUnknownEntrypointSymbol name ->
        "Could not find function named"
          PP.<+> PP.squotes (PP.pretty name)
          PP.<+> "in the supplied S-expression (.cbl) program"
      SyntaxProgWithEntrypointAddr addr ->
        "S-expression (.cbl) programs cannot use an address"
          PP.<+> PP.parens (PP.pretty (showHex addr ""))
          PP.<+> "as an entrypoint"
      SyntaxProgWithTargetAddr addr ->
        "S-expression (.cbl) programs cannot use an address"
          PP.<+> PP.parens (PP.pretty (showHex addr ""))
          PP.<+> "as a target"
      SyntaxProgExterns ->
        "S-expression (.cbl) programs with `extern` declarations"
          PP.<+> "are not currently supported"
      InitialPreconditionParseError parseError ->
        PP.pretty parseError
      InitialPreconditionTypeMismatch typeMismatchError ->
        PP.pretty typeMismatchError
      TargetOverrideNoAddress _path ->
        "--target-override specified, but no address is available for the target"
      TargetAddressWithoutContainingFunction -> "Provided a target address but did not provide --target-containing-function-address"
      AvoidSymbolDidNotResolve nm ->
        PP.hsep
          ["An avoid location symbol should resolve to an address but", PP.squotes (PP.pretty nm), " did not"]

-- | The types of ELF binaries that @screach@ supports.
data ElfBinary w
  = -- | A \"raw\" binary (i.e., not an ECFS file).
    RawElfBinary (Elf.ElfHeaderInfo w)
  | -- | An ECFS file.
    EcfsBinary (Ecfs.Ecfs w)

-- | Extract the underlying 'ElF.ElfHeaderInfo' from an 'ElfBinary'.
elfBinaryHeaderInfo :: ElfBinary w -> Elf.ElfHeaderInfo w
elfBinaryHeaderInfo (RawElfBinary ehi) = ehi
elfBinaryHeaderInfo (EcfsBinary ecfs) = Ecfs.ecfsElfHeaderInfo ecfs

-- | Read an ELF binary and return its file permissions and the parsed
-- 'ElfBinary'.
readElfBinary ::
  forall m w.
  ( MonadIO m
  , MonadThrow m
  , CLM.HasPtrWidth w
  ) =>
  Conf.ProgramConfig ->
  m (Permissions, ElfBinary w)
readElfBinary conf =
  do
    perms <- liftIO $ getPermissions path
    bs <- liftIO $ BS.readFile path
    -- First, try to parse the ELF binary as an ECFS file.
    case Ecfs.decodeEcfs bs of
      -- It's an ECFS file, so proceed.
      Right (Elf.SomeElf ecfs) ->
        let ecfsHdr = Ecfs.ecfsElfHeaderInfo ecfs
         in assertHdrPointerWidth ecfsHdr $ pure (perms, EcfsBinary ecfs)
      -- It's not an ECFS file, as it lacks the ECFS magic bytes. Don't fail
      -- just yet, however, as the file might still be a valid ELF binary.
      Left (Ecfs.InvalidEcfsMagic{}) -> do
        Elf.SomeElf hdr <- decodeElfHeaderInfo bs
        assertHdrPointerWidth hdr $ pure (perms, RawElfBinary hdr)
      -- It's an ECFS file, but it failed to decode for some reason. Give up at
      -- this point, as we cannot proceed with a malformed ECFS binary.
      Left ecfsDecodeError -> throw ecfsDecodeError
 where
  path :: FilePath
  path = Conf.confProgram conf

  decodeElfHeaderInfo :: BS.ByteString -> m (Elf.SomeElf Elf.ElfHeaderInfo)
  decodeElfHeaderInfo bs =
    case Elf.decodeElfHeaderInfo bs of
      Left _ -> throw $ NotAnElf path
      Right hdr -> pure hdr

  -- Assert that the width of the supplied ElfHeaderInfo `w2` is pointer-sized
  -- (i.e., equal to `w`). If not, throw an exception.
  assertHdrPointerWidth ::
    forall w2 r.
    Elf.ElfHeaderInfo w2 ->
    ((w ~ w2) => m r) ->
    m r
  assertHdrPointerWidth hdr k =
    let hdrClass = Elf.headerClass (Elf.header hdr)
     in case ( hdrClass
             , CCC.testEquality ?ptrWidth (CCC.knownNat @32)
             , CCC.testEquality ?ptrWidth (CCC.knownNat @64)
             ) of
          (Elf.ELFCLASS32, Just CCC.Refl, Nothing) -> throw $ BadElf32Bit path
          (Elf.ELFCLASS64, Nothing, Just CCC.Refl) -> k
          _ -> throw $ UnsupportedElfPointerWidth $ CCC.natValue ?ptrWidth

-- TODO(internal#13): This is copy/pasted from GREASE

-- | Integer argument registers
interestingRegs :: Set.Set String
interestingRegs =
  Set.fromList
    [ -- x86_64
      "rdi"
    , "rsi"
    , "rdx"
    , "rcx"
    , "r8" -- also listed above in PPC32, but it's a set, so...
    , "r9"
    ]

-- TODO(internal#13): This is copy/pasted from GREASE

-- | Filter out \"uninteresting\" concretized shapes
--
-- They are interesting when:
--
-- * They have a non-default value, and
-- * The name is in a known \"interesting\" set ('interestingRegs')
--
-- This is clearly a bit of a hack, but leads to concise and readable output.
interestingConcretizedShapes ::
  forall ext wptr sym argTys tag.
  CLM.HasPtrWidth wptr =>
  (Shape.ExtShape ext ~ Shape.PtrShape ext wptr) =>
  -- | Argument names
  Ctx.Assignment ValueName argTys ->
  -- | Default/initial/minimal shapes
  Ctx.Assignment (Shape.Shape ext 'Shape.Precond tag) argTys ->
  ConcArgs sym ext argTys ->
  Ctx.Assignment (Const Bool) argTys
interestingConcretizedShapes names initArgs cArgs =
  let initArgsWithType = TFC.fmapFC Shape.tagWithType initArgs
      cShapes = TFC.fmapFC Shape.tagWithType (Conc.concArgsShapes cArgs)
   in Ctx.zipWith
        ( \vn (Const isDefault) ->
            let name = getValueName vn
             in Const (name `List.elem` interestingRegs && not isDefault)
        )
        names
        (Ctx.zipWith (\s s' -> Const (Maybe.isJust (testEquality s s'))) cShapes initArgsWithType)

-- | Log a message to 'stderr' along with the current time.
log :: MonadIO m => GD.Severity -> PP.Doc a -> m ()
log sev msg = do
  t <- liftIO Time.getCurrentTime
  let time = Time.formatTime Time.defaultTimeLocale "[%F %T]" t
  let severity = PP.brackets $ PP.viaShow sev
  liftIO $ PP.hPutDoc stderr $ PP.pretty time PP.<+> severity PP.<+> msg
  liftIO $ PP.hPutDoc stderr PP.line

defaultLogAction :: Conf.ProgramConfig -> ScreachLogAction
defaultLogAction conf =
  let sev = Conf.verbosity conf
   in LJ.LogAction $ \diag ->
        Monad.when (SD.severity diag <= sev) $
          log (SD.severity diag) (PP.pretty diag)

-- | Create a 'CLD.DataLayout' suitable for @macaw-symbolic@'s needs. Currently,
-- this simply overrides the 'CLD.defaultDataLayout' with a reasonable
-- endianness value based on the architecture.
macawDataLayout :: ArchContext arch -> CLD.DataLayout
macawDataLayout archCtx =
  CLD.defaultDataLayout
    & CLD.intLayout
      .~ (archCtx ^. Arch.archInfo . to (MSML.toCrucibleEndian . MAI.archEndianness))

loadInitialPreconditions ::
  Shape.ExtShape ext ~ Shape.PtrShape ext w =>
  CLM.HasPtrWidth w =>
  ScreachLogAction ->
  FilePath ->
  IO (Parse.ParsedShapes ext)
loadInitialPreconditions la path =
  Parse.fromFile path
    >>= \case
      Left err -> userError la err
      Right parsed -> pure parsed

initDebugger ::
  WI.IsExpr (WI.SymExpr sym) =>
  PP.Pretty cExt =>
  PP.Pretty (Dbg.ResponseExt cExt) =>
  ScreachLogAction ->
  GO.DebugOpts ->
  Dbg.CommandExt cExt ->
  CCC.TypeRepr t ->
  IO (Dbg.Context cExt sym ext t)
initDebugger la dbgOpts cExt t = do
  inps_ <- Dbg.defaultDebuggerInputs cExt
  stmts <-
    Monad.forM (GO.debugCmds dbgOpts) $ \cmdTxt ->
      case Dbg.parse cExt cmdTxt of
        Left err -> userError la (PP.pretty (Dbg.renderParseError err))
        Right stmt -> pure stmt
  inps <- Dbg.prepend stmts inps_
  let pp = (PP.<> "\n") . PP.pretty
  let outps = Dbg.Outputs (doLog la . Diag.DebuggerOutput . pp)
  Dbg.initCtx cExt prettyPtrFnMap inps outps t

-- | Convert a register-based CFG ('CCR.SomeCFG') to an SSA-based CFG
-- ('CCC.SomeCFG').
toSsaSomeCfg ::
  CCE.IsSyntaxExtension ext =>
  CCR.SomeCFG ext init ret ->
  CCC.SomeCFG ext init ret
toSsaSomeCfg (CCR.SomeCFG cfg) = CCS.toSSA cfg

-- | Inspect the user-supplied program's file extension to determine how to
-- analyze it.
analyzeFile ::
  ( arch ~ MX86.X86_64
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  Conf.Config ->
  ScreachLogAction ->
  GreaseLogAction ->
  CFH.HandleAllocator ->
  ArchContext arch ->
  IO ()
analyzeFile conf =
  let path = Conf.confProgram $ Conf.programConfig conf
   in if
        | ".cbl" `isExtensionOf` path -> analyzeSyntax conf
        | otherwise -> analyzeElf conf

overrideError :: ScreachLogAction -> FunctionName -> IO a
overrideError sla nm =
  userError sla $
    PP.hsep
      [ "Function declared but not defined:"
      , PP.pretty (WFN.functionName nm) <> "."
      , "Try specifying an override using --overrides."
      ]

overrideErrorCallback ::
  ScreachLogAction ->
  GMO.CantResolveOverrideCallback sym arch
overrideErrorCallback sla =
  GMO.CantResolveOverrideCallback $ \nm _hdl -> liftIO (overrideError sla nm)

-- | Aspects of setting up the simulator state that are shared for S-expression
-- programs and binaries. In particular, registering CFGs and forward
-- declarations that appear in overrides.
setupHookCommon ::
  ( CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , ToConc.HasToConcretize p
  , GP.HasMemVar p
  ) =>
  ScreachLogAction ->
  bak ->
  GMOA.AddressOverrides arch ->
  -- | The map of public function names to their overrides.
  Map.Map WFN.FunctionName (GMO.MacawSExpOverride p sym arch) ->
  Maybe (GE.StartupOv cfg) ->
  CS.OverrideSim p sym (MS.MacawExt arch) rtp args ret ()
setupHookCommon sla bak addrOvs funOvs mbEntryStartupOv = do
  let errCb = overrideErrorCallback sla

  -- Register forward declarations in startup overrides.
  Monad.forM_ @Maybe mbEntryStartupOv $ \entryStartupOv ->
    GMO.registerMacawOvForwardDeclarations bak funOvs errCb $
      GE.startupOvForwardDecs entryStartupOv

  GMS.registerOverrideHandles bak errCb funOvs
  GMOA.registerAddressOverrideHandles bak errCb funOvs addrOvs

-- | Analyze a Crucible S-expression program.
analyzeSyntax ::
  forall arch.
  ( arch ~ MX86.X86_64
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  Conf.Config ->
  ScreachLogAction ->
  GreaseLogAction ->
  CFH.HandleAllocator ->
  ArchContext arch ->
  IO ()
analyzeSyntax conf sla gla halloc archCtx = do
  let progConfg = Conf.programConfig conf
  prog <- doParse sla halloc $ Conf.confProgram progConfg
  Monad.unless (null (CSC.parsedProgExterns prog)) $
    throw SyntaxProgExterns

  let entryLoc = Conf.entryLoc progConfg
  entryNm <-
    getAnalysisLocSymbol SyntaxProgWithEntrypointAddr $ SAL.entryAnalysisLoc entryLoc

  targetNm <-
    getAnalysisLocSymbol SyntaxProgWithTargetAddr $ GAL.targetAnalysisLoc $ Conf.targetLoc conf
  let resolvedTargetLoc = GAL.ResolvedTargetLocSymbol targetNm Nothing

  entryRegCfg <-
    case Map.lookup (WFN.functionName entryNm) (Syntax.parsedProgramCfgMap prog) of
      Just cfg -> pure cfg
      Nothing -> throw $ SyntaxProgUnknownEntrypointSymbol entryNm
  mbEntryStartupOv <-
    traverse @Maybe
      ( \path -> do
          result <- GE.parseEntrypointStartupOv halloc path
          let usrErr = userError sla . PP.pretty
          case result of
            -- See Note [Explicitly listed errors]
            Left e@GE.StartupOvParseError{} -> usrErr e
            Left e@GE.StartupOvCFGNotFound{} -> usrErr e
            Right startupOv -> pure startupOv
      )
      (SAL.entryStartupOvPath entryLoc)
  let entryCfgs_ =
        GE.EntrypointCfgs
          { GE.entrypointStartupOv = mbEntryStartupOv
          , GE.entrypointCfg = entryRegCfg
          }
  let usrErr = userError sla . PP.pretty
  entryCfgs <-
    case GME.checkMacawEntrypointCfgsSignatures archCtx entryCfgs_ of
      -- See Note [Explicitly listed errors]
      Left e@GME.BadArgs{} -> usrErr e
      Left e@GME.BadRet{} -> usrErr e
      Right cfgs -> pure cfgs

  let emptyBinMd = GL.emptyBinMd
  let loadOpts = GL.binLoadOptions emptyBinMd
  let mem = MC.emptyMemory (archCtx ^. Arch.archInfo . to MAI.archAddrWidth)
  addrOvs <- loadAddrOvs archCtx conf halloc loadOpts mem resolvedTargetLoc
  let setupHook :: forall sym. GM.SetupHook sym arch
      setupHook = GM.SetupHook $ \bak funOvs -> do
        setupHookCommon sla bak addrOvs funOvs mbEntryStartupOv
        let dl = macawDataLayout archCtx
        let errCb = overrideErrorCallback sla
        GMS.registerSyntaxHandles bak gla errCb dl funOvs prog
  analyzeCfg
    conf
    sla
    gla
    halloc
    archCtx
    Nothing -- no LoadedELF for syntax programs
    setupHook
    resolvedTargetLoc
    (GMSH.ExecutingAddressAction (\_ -> pure ()))
    addrOvs
    entryCfgs
    defaultPrioritizationFunction
 where
  -- \| Retrieve the function name from an S-expression program's
  -- 'AnalysisLoc'. Throw an exception if an address was given.
  getAnalysisLocSymbol ::
    -- \| The exception to throw if an address was given.
    (Word64 -> UserError) ->
    GAL.AnalysisLoc ->
    IO WFN.FunctionName
  getAnalysisLocSymbol addrEx loc =
    case loc of
      GAL.AnalysisLocSymbol name -> pure name
      GAL.AnalysisLocAddress addr -> throw $ addrEx addr

loadElfFromConfig ::
  ( arch ~ MX86.X86_64
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  Conf.ProgramConfig ->
  ScreachLogAction ->
  GreaseLogAction ->
  ArchContext arch ->
  IO LoadedELF
loadElfFromConfig conf sla gla _archCtx = do
  let entryLoc = Conf.entryLoc conf
  let entrypointList = [SAL.entryLocToGreaseEntrypoint entryLoc]
  (perms, elfBinary) <- readElfBinary conf
  let elf = elfBinaryHeaderInfo elfBinary
  -- TODO: PLT stubs option support? (instead of [])
  loadResult <-
    GL.load @MX86.X86_64
      gla
      entrypointList
      perms
      elf
      x64RelocSupported
      (Just MX86.x86_64PLTStubInfo)
      (Conf.confProgram conf)
      []
  let usrErr = userError sla . PP.pretty
  let badElf = malformedElf sla . PP.pretty
  loadedProg <-
    case loadResult of
      -- See Note [Explicitly listed errors]
      -- User errors
      Left e@GL.UnsupportedObjectFile{} -> usrErr e
      Left e@GL.EntrypointNotFound{} -> usrErr e
      Left e@GL.InvalidEntrypointAddress{} -> usrErr e
      -- Malformed ELF
      Left e@GL.DynamicFunctionAddressUnresolvable{} -> badElf e
      Left e@GL.ElfParseError{} -> badElf e
      Left e@GL.CoreDumpClassMismatch{} -> badElf e
      Left e@GL.CoreDumpNotesError{} -> badElf e
      Left e@GL.CoreDumpPcError{} -> badElf e
      Left e@GL.CoreDumpAddressUnresolvable{} -> badElf e
      Left e@GL.CoreDumpNoEntrypoint{} -> badElf e
      Left e@GL.RelocationMapError{} -> badElf e
      Left e@GL.PltStubResolutionError{} -> badElf e
      Right prog -> pure prog
  let loadOpts = GL.binLoadOptions (GL.progBinMd loadedProg)
  let loadOffset = fromMaybe 0 (LC.loadOffset loadOpts)
  let mem = Loader.memoryImage (GL.progLoadedBinary loadedProg)
  let entrypointAddrs = GL.binEntrypointAddrs (GL.progBinMd loadedProg)

  -- For ECFS binaries, override PLT stubs with ECFS-specific resolution.
  pltStubs <-
    case elfBinary of
      RawElfBinary _ -> pure $ GL.binPltStubs (GL.progBinMd loadedProg)
      EcfsBinary ecfs -> do
        let ecfsPltStubs = SE.findEcfsPltStubs loadOpts ecfs
        resolvedEcfsPltStubs <-
          traverse
            ( \(GMP.PltStub addr name) ->
                case MBLE.resolveAbsoluteAddress mem (MC.memWord (addr + loadOffset)) of
                  Just so -> pure (so, functionNameFromText name)
                  Nothing -> throw $ UnknownEcfsPltStub addr name
            )
            ecfsPltStubs
        pure $ Map.fromList resolvedEcfsPltStubs

  let entry = SAL.entryLocToGreaseEntrypoint entryLoc
  entryAddr <-
    case Map.lookup entry entrypointAddrs of
      Just entryAddr -> pure entryAddr
      Nothing ->
        panic
          "main"
          [ "entrypoint not in progEntrypointAddrs"
          , show (PP.pretty entry)
          ]
  let isEcfs = case elfBinary of
        EcfsBinary{} -> True
        RawElfBinary{} -> False
  let loadedProg_ =
        loadedProg
          { GL.progBinMd = (GL.progBinMd loadedProg){GL.binPltStubs = pltStubs}
          }
  pure $
    LoadedELF
      { loadedElf = elfBinaryHeaderInfo elfBinary
      , loadedProgram = loadedProg_
      , entrypointAddr = entryAddr
      , isECFS = isEcfs
      }

containingFunctionFromRloc ::
  (MonadFail m) => GAL.ResolvedTargetLoc 64 -> Maybe Word64 -> m (MM.MemWord 64)
containingFunctionFromRloc rloc def =
  case (rloc, def) of
    (_, Just container) -> pure (MM.memWord @64 container)
    (GAL.ResolvedTargetLocSymbol _ (Just addr), _) -> pure addr
    _ -> throw TargetAddressWithoutContainingFunction

-- | Default prioritization function that assigns no priority.
-- Returns 'Nothing' for all paths, allowing the scheduler to use its default ordering.
defaultPrioritizationFunction :: Sched.PrioritizationFunction p sym ext rtp
defaultPrioritizationFunction _ _ = pure Nothing

withPrioritizationFunction ::
  (MonadFail m, MonadIO m) =>
  Conf.Config ->
  (WPL.ProgramLoc -> Bool) ->
  ArchContext MX86.X86_64 ->
  CG.CFGCache 64 ->
  ScreachLogAction ->
  GreaseLogAction ->
  LoadedELF ->
  CFH.HandleAllocator ->
  GAL.ResolvedTargetLoc 64 ->
  ( ( forall p sym rtp.
      ( SDSE.HasDistancesState p
      , RR.HasRecordState p p sym ext rtp
      , RR.HasReplayState p p sym ext rtp
      , WI.IsExprBuilder sym
      ) =>
      Sched.PrioritizationFunction p sym ext rtp
    ) ->
    m r
  ) ->
  m r
withPrioritizationFunction conf avoidList archCtx cfgCache sla gla elf halloc resolvedTargetLoc k =
  if not $ Conf.explore conf
    then
      k defaultPrioritizationFunction
    else do
      cgPath <- case Conf.callgraph conf of
        Just c -> pure c
        Nothing -> fail "user error: Must specify callgraph with \"--callgraph\" when exploring"
      container <- containingFunctionFromRloc resolvedTargetLoc (Conf.targetContainingFunction conf)
      mbCfg <- liftIO $ CG.loadCallGraph cgPath
      cfg <- case mbCfg of
        Left _ -> fail $ "Failed to load callgraph at: " ++ cgPath
        Right cfg -> pure cfg
      let
        pfuncFromTargetAddr ::
          MM.MemWord 64 ->
          forall p sym ext rtp.
          ( SDSE.HasDistancesState p
          , RR.HasRecordState p p sym ext rtp
          , RR.HasReplayState p p sym ext rtp
          , WI.IsExprBuilder sym
          ) =>
          Sched.PrioritizationFunction p sym ext rtp
        pfuncFromTargetAddr tAddr =
          let dCfg =
                Dist.DistanceConfig
                  { Dist.defaultRetDist = Conf.defaultReturnDistance conf
                  , Dist.isInfiniteDistLoc = avoidList
                  }
           in SDSE.sdsePrioritizationFunction
                tAddr
                container
                archCtx
                cfg
                cfgCache
                dCfg
                sla
                gla
                elf
                halloc
        cgPfunc ::
          forall p sym ext rtp.
          ( SDSE.HasDistancesState p
          , RR.HasRecordState p p sym ext rtp
          , RR.HasReplayState p p sym ext rtp
          , WI.IsExprBuilder sym
          ) =>
          Sched.PrioritizationFunction p sym ext rtp
        cgPfunc = case GAL.resolvedTargetAddr resolvedTargetLoc of
          Just targetAddr -> pfuncFromTargetAddr targetAddr
          _ -> defaultPrioritizationFunction
      k cgPfunc

loadAddrOvs ::
  forall arch.
  ( MS.SymArchConstraints arch
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  ArchContext arch ->
  Conf.Config ->
  CFH.HandleAllocator ->
  LC.LoadOptions ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  GAL.ResolvedTargetLoc 64 ->
  IO (GMOA.AddressOverrides arch)
loadAddrOvs archCtx conf halloc loadOpts mem rtLoc = do
  let regTys = archCtx ^. Arch.archRegStructType
  let word64ToInteger = fromIntegral @Word64 @Integer -- safe
  let loadOffset = word64ToInteger (fromMaybe 0 (LC.loadOffset loadOpts))
  let addrOvs_ =
        List.map
          (\(addr, path) -> (addr + loadOffset, path))
          (Conf.addrOverrides conf)
  allAddrOvs <-
    case Conf.targetOverride conf of
      Nothing -> pure addrOvs_
      Just path ->
        case GAL.resolvedTargetAddr rtLoc of
          Just addr_ ->
            let addr = word64ToInteger (MM.memWordValue addr_)
             in pure ((addr, path) : addrOvs_)
          Nothing -> throw (TargetOverrideNoAddress path)
  mbAddrOvs <- GMOA.loadAddressOverrides regTys halloc mem allAddrOvs
  case mbAddrOvs of
    Left err -> throw (AddressOverrideError (PP.pretty err))
    Right addrOvs -> pure addrOvs

analysisLocToAddr ::
  MM.Memory 64 ->
  GL.BinMd MX86.X86_64 ->
  GAL.AnalysisLoc ->
  IO (MM.MemWord 64)
analysisLocToAddr mem binMd loc = case GAL.resolveTargetLoc mem binMd loc of
  GAL.ResolvedTargetLocAddress addr _ -> pure addr
  GAL.ResolvedTargetLocSymbol _ (Just addr) -> pure addr
  GAL.ResolvedTargetLocSymbol nm Nothing -> throw $ AvoidSymbolDidNotResolve nm

-- The code that is invoked in the body of the refinement loop,
-- regardless of whether path-based exploration is used or not.
initCFG ::
  ( ext ~ MS.MacawExt arch
  , arch ~ MX86.X86_64
  , ret ~ MS.ArchRegStruct arch
  , args ~ (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch)
  , aty ~ MS.MacawCrucibleRegTypes arch
  , CB.IsSymBackend sym bak
  , sym ~ WEB.ExprBuilder t st fs
  , bak ~ CBO.OnlineBackend solver t st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth 64
  , ?lc :: CLTC.TypeContext
  , ?memOpts :: CLM.MemOptions
  , MC.RegAddrWidth (MC.ArchReg arch) ~ 64
  ) =>
  CCC.SomeCFG ext args ret ->
  -- | 'Just' the 'LoadedELF' if analyzing a binary, 'Nothing' for
  -- S-expression programs.
  Maybe LoadedELF ->
  bak ->
  GreaseLogAction ->
  ScreachLogAction ->
  CFH.HandleAllocator ->
  Conf.Config ->
  ArchContext arch ->
  -- | The target address to stop execution at if no
  -- goal override is present
  Maybe (GGE.TargetAddress 64) ->
  Maybe (CCC.SomeCFG ext args ret) ->
  -- | The target location of the goal
  -- for printing results
  GAL.ResolvedTargetLoc 64 ->
  CS.GlobalVar CLM.Mem ->
  GM.SetupHook sym arch ->
  GMSH.ExecutingAddressAction arch ->
  GMOA.AddressOverrides arch ->
  Shape.ArgShapes
    ext
    Shape.NoTag
    aty ->
  IO
    (CS.ExecState (SP.ScreachSimulatorState p sym bak ext arch t ret aty 64) sym ext (CS.RegEntry sym ret))
initCFG (CCC.SomeCFG entryRegSsaCfg) mbLoadedElf =
  let mbEntryAddr = entrypointAddr <$> mbLoadedElf
      isEcfs = maybe False isECFS mbLoadedElf
      discoveredHdls = Maybe.maybe Map.empty (`Map.singleton` CCC.cfgHandle entryRegSsaCfg) mbEntryAddr
   in \bak gla sla halloc conf archCtx mbTargetAddr mbStartupOvSomeSsaCfg rtLoc memVar setupHook execAction addrOvs argShapes -> do
        let (binMd, mem) =
              maybe
                ( GL.emptyBinMd
                , MC.emptyMemory (archCtx ^. Arch.archInfo . to MAI.archAddrWidth)
                )
                (\lelf -> let lp = loadedProgram lelf in (GL.progBinMd lp, Loader.memoryImage (GL.progLoadedBinary lp)))
                mbLoadedElf
        let dataLayout = macawDataLayout archCtx
        let rNames = archCtx ^. Arch.archRegNames
            argNames = archCtx ^. Arch.archValueNames
            regTypes = archCtx ^. Arch.archRegTypes
        let loadOpts = GL.binLoadOptions binMd
            relocs = GL.binRelocs binMd
        let sym = CB.backendGetSym bak
        GR.ErrorCallbacks
          { GR.errorMap = bbMapRef
          , GR.llvmErrCallback = recordLLVMAnnotation
          , GR.macawAssertionCallback = processMacawAssert
          } <-
          GR.buildErrMaps

        let addrWidth = MC.addrWidthRepr (Proxy @64)
        LJ.writeLog
          gla
          (GD.RefineDiagnostic (RDiag.RefinementUsingPrecondition addrWidth argNames argShapes))
        let ?recordLLVMAnnotation = recordLLVMAnnotation
        let ?processMacawAssert = processMacawAssert
        (initMem, memCfg0) <- do
          memModelContents <-
            case Conf.globals conf of
              GO.Initialized -> pure MSML.ConcreteMutable
              GO.Symbolic -> pure MSML.SymbolicMutable
              GO.Uninitialized ->
                unsupported sla "Macaw does not support uninitialized globals (macaw#372)"
          (mem0, ptrTable) <-
            GM.emptyMacawMem
              bak
              archCtx
              mem
              memModelContents
              relocs
          let skipRelocs = GO.SkipUnsupportedRelocs False
          let memCfg = GM.memConfigInitial bak archCtx ptrTable skipRelocs relocs
          pure (mem0, memCfg)
        (args, setupMem, setupAnns) <- GS.setup gla bak dataLayout argNames regTypes argShapes initMem
        -- TODO: When adding support for AArch32 and PPC, override the link
        -- registers like GREASE does, probably should refactor that in GREASE
        -- to make it easier to use here - part of ArchCtx?

        -- Initialize the file system
        GSIO.InitializedFs fileContents fs globals0 initFsOv <-
          liftIO $ GSIO.initialLlvmFileSystem halloc sym (Conf.fsOpts conf)

        let userOvPaths = Conf.overrides conf
        let ovs =
              reachabilityBuiltinOverrides gla
                <> builtinStubsOverrides
                  memVar
                  memCfg0
                  fs
                  (archCtx ^. Arch.archInfo . to MAI.archEndianness)
        fnOvsMap <- liftIO $ do
          result <- GMO.mkMacawOverrideMap bak ovs userOvPaths halloc archCtx
          let usrErr = userError sla . PP.pretty
          case result of
            -- See Note [Explicitly listed errors]
            Left (GMOS.MacawSExpOverrideParseError e@Syntax.SExpressionParseError{}) -> usrErr (GMOS.MacawSExpOverrideParseError e)
            Left (GMOS.MacawSExpOverrideParseError e@Syntax.SyntaxParseError{}) -> usrErr (GMOS.MacawSExpOverrideParseError e)
            Left e@GMOS.MacawSExpOverrideLoaderError{} -> usrErr e
            Right m -> pure m
        fnAddrOvs <-
          liftIO (Syntax.loadOverridesYaml loadOpts mem (Map.keysSet fnOvsMap) (Conf.overridesYaml conf))
            >>= \case
              -- See Note [Explicitly listed errors]
              Left e@Syntax.LoadOverridesYamlParseError{} -> userError sla (PP.pretty e)
              Left e@Syntax.LoadOverridesYamlResolveError{} -> userError sla (PP.pretty e)
              Right ok -> pure ok
        let cOpts = Conf.callOpts conf
        let defaultLfhd =
              ResolveCall.defaultLookupFunctionHandleDispatch
                bak
                gla
                halloc
                archCtx
                mem
                fnOvsMap
                (overrideError sla)
        let lfhd
              | isEcfs =
                  ecfsLookupFunctionHandleDispatch gla halloc archCtx binMd mem defaultLfhd
              | otherwise =
                  defaultLfhd
        let memCfg =
              ( GM.memConfigWithHandles
                  bak
                  gla
                  halloc
                  archCtx
                  binMd
                  mem
                  fnOvsMap
                  fnAddrOvs
                  builtinGenericSyscalls
                  cOpts
                  (overrideError sla)
                  memCfg0
              )
                { MS.lookupFunctionHandle =
                    ResolveCall.lookupFunctionHandle
                      bak
                      gla
                      halloc
                      archCtx
                      binMd
                      mem
                      fnOvsMap
                      fnAddrOvs
                      cOpts
                      lfhd
                }
        extImpl <- GM.buildMacawExtImpl (archCtx ^. Arch.archVals) sym memVar memCfg gla bak mem (Just rtLoc) (Conf.targetOverride conf)
        (toConcVar, globals1) <- liftIO $ ToConc.newToConcretize halloc globals0
        -- NB: We intentionally exclude `mustFailHeuristics` here. This is
        -- primarily because must-fail would incorrectly deem targets that are
        -- always reachable in all paths as refinement failures. Note that
        -- must-fail isn't needed for refinement to work properly (it is just
        -- a bug-finding mechanism), so it shouldn't impact screach's
        -- reachability results.
        let heuristics =
              GH.reachedTargetHeuristic
                : if Conf.noHeuristics conf
                  then []
                  else GH.macawHeuristics gla rNames
        let concInitState =
              Conc.InitialState
                { Conc.initStateArgs = args
                , Conc.initStateFs = fileContents
                , Conc.initStateMem = initMem
                }
        let boundsOpts = Conf.boundsOpts conf

        let refineData =
              GR.RefinementData
                { GR.refineAnns = setupAnns
                , GR.refineArgNames = argNames
                , GR.refineArgShapes = argShapes
                , GR.refineHeuristics = heuristics
                , GR.refineInitState = concInitState
                , GR.refineSolver = Conf.solver conf
                , GR.refineSolverTimeout = GO.simSolverTimeout boundsOpts
                , GR.refineErrMap = bbMapRef
                }
        let dbgOpts = Conf.debugOpts conf
        let dbgCmdExt = MDebug.macawCommandExt (archCtx ^. Arch.archVals)
        dbgCtx <- initDebugger sla dbgOpts dbgCmdExt (archCtx ^. Arch.archRegStructType)
        gssRecState <- RR.mkRecordState halloc
        gssEmpTrace <- RR.emptyRecordedTrace sym
        gssRepState <- RR.mkReplayState halloc gssEmpTrace
        let gssPers = GP.mkPersonality memVar dbgCtx toConcVar refineData
        let greaseSimState =
              GMSS.mkGreaseSimulatorState gssPers gssRecState gssRepState
                & GMSS.discoveredFnHandles .~ discoveredHdls
        personality <-
          SP.mkScreachSimulatorState
            sym
            halloc
            greaseSimState
        GM.initState
          bak
          gla
          extImpl
          execAction
          halloc
          setupMem
          globals1
          initFsOv
          archCtx
          setupHook
          addrOvs
          personality
          (GS.argVals args)
          fnOvsMap
          mbStartupOvSomeSsaCfg
          (CCC.SomeCFG entryRegSsaCfg)

withMaybeFile :: Maybe FilePath -> IOMode -> (Maybe Handle -> IO a) -> IO a
withMaybeFile Nothing _ f = f Nothing
withMaybeFile (Just pth) imd f = withFile pth imd (f . Just)

addressCallBack ::
  MM.MemWidth (MC.RegAddrWidth (MC.ArchReg arch)) =>
  Proxy arch ->
  Handle ->
  MM.MemSegmentOff (MC.RegAddrWidth (MC.ArchReg arch)) ->
  IO ()
addressCallBack _ hdl addr =
  let js = renderJSON $ GL.memSegOffToJson addr
   in Text.IO.hPutStrLn hdl js

-- | Analyze an ELF binary.
analyzeElf ::
  forall arch.
  ( arch ~ MX86.X86_64
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  Conf.Config ->
  ScreachLogAction ->
  GreaseLogAction ->
  CFH.HandleAllocator ->
  ArchContext arch ->
  IO ()
analyzeElf conf sla gla halloc archCtx = do
  let progConf = Conf.programConfig conf
  let entryLoc = Conf.entryLoc progConf
  elf <- loadElfFromConfig progConf sla gla archCtx
  let loadedProg_ = loadedProgram elf
  let entryAddr = entrypointAddr elf
  let mem = Loader.memoryImage (GL.progLoadedBinary loadedProg_)
  let loadOpts = GL.binLoadOptions (GL.progBinMd loadedProg_)
  withMaybeFile (Conf.simDumpCoverage conf) WriteMode $ \mbCovHandle -> do
    let secsJson = GL.dumpSections mem
    case mbCovHandle of
      Just covHandle -> Text.IO.hPutStrLn covHandle (renderJSON secsJson)
      Nothing -> pure ()
    mbEntryStartupOv <-
      traverse @Maybe
        ( \path -> do
            result <- GE.parseEntrypointStartupOv halloc path
            let usrErr = userError sla . PP.pretty
            case result of
              -- See Note [Explicitly listed errors]
              Left e@GE.StartupOvParseError{} -> usrErr e
              Left e@GE.StartupOvCFGNotFound{} -> usrErr e
              Right startupOv -> pure startupOv
        )
        (SAL.entryStartupOvPath entryLoc)
    let resolvedTargetLoc =
          GAL.resolveTargetLoc mem (GL.progBinMd loadedProg_) (GAL.targetAnalysisLoc $ Conf.targetLoc conf)
    addrOvs <- loadAddrOvs archCtx conf halloc loadOpts mem resolvedTargetLoc
    let setupHook :: forall sym. GM.SetupHook sym arch
        setupHook = GM.SetupHook $ \bak funOvs ->
          setupHookCommon sla bak addrOvs funOvs mbEntryStartupOv

    CCR.SomeCFG entryRegCfg <-
      GMD.discoverFunction gla halloc archCtx (GL.progBinMd loadedProg_) mem entryAddr
    let entryCfgs_ =
          GE.EntrypointCfgs
            { GE.entrypointStartupOv = mbEntryStartupOv
            , GE.entrypointCfg = CCR.AnyCFG entryRegCfg
            }
    entryCfgs <-
      let usrErr = userError sla . PP.pretty
       in case GME.checkMacawEntrypointCfgsSignatures archCtx entryCfgs_ of
            -- See Note [Explicitly listed errors]
            Left e@GME.BadArgs{} -> usrErr e
            Left e@GME.BadRet{} -> usrErr e
            Right cfgs -> pure cfgs
    let
      addressHandle :: GMSH.ExecutingAddressAction arch
      addressHandle =
        GMSH.ExecutingAddressAction $ Maybe.maybe (\_ -> pure ()) (addressCallBack @arch Proxy) mbCovHandle
    cfgCache <- IORef.newIORef Map.empty
    avoidAddrs <- Monad.forM (Conf.avoidedLocations conf) $ analysisLocToAddr mem (GL.progBinMd loadedProg_)
    let isLocAvoidAddr loc =
          let l = CG.locToAddressMaybe @64 loc
           in maybe False (`elem` avoidAddrs) l
    withPrioritizationFunction
      conf
      isLocAvoidAddr
      archCtx
      (CG.CFGCache cfgCache)
      sla
      gla
      elf
      halloc
      resolvedTargetLoc
      $ \pFunc ->
        analyzeCfg
          conf
          sla
          gla
          halloc
          archCtx
          (Just elf)
          setupHook
          resolvedTargetLoc
          addressHandle
          addrOvs
          entryCfgs
          pFunc

handleTarget ::
  forall p' p sym ext bak arch t ret aty wptr tag.
  ( p
      ~ SP.ScreachSimulatorState p' sym bak ext arch t ret aty wptr
  , ext ~ MS.MacawExt arch
  , aty ~ MS.CtxToCrucibleType (MS.ArchRegContext arch)
  , CLM.HasPtrWidth
      wptr
  , (Shape.ExtShape ext ~ Shape.PtrShape ext wptr)
  ) =>
  ArchContext arch ->
  Proxy (bak, t, p') ->
  ScreachLogAction ->
  -- | Default/initial/minimal shapes
  Ctx.Assignment (Shape.Shape ext 'Shape.Precond tag) aty ->
  CS.ExecResult p sym ext (CS.RegEntry sym ret) ->
  IO
    Bool
handleTarget archCtx _ sla initArgs st =
  let argNames = archCtx ^. Arch.archValueNames
      pers = CS.execResultContext st ^. CS.cruciblePersonality
   in case pers ^. SP.refineResult of
        Just (RFT.RefineResult bid cData) | Bug.bugType bid == Bug.ReachedTarget -> do
          let addrWidth = archCtx ^. Arch.archInfo . to MAI.archAddrWidth
          let interestingShapes = interestingConcretizedShapes argNames initArgs (Conc.concArgs cData)
          let prettyData = Conc.printConcData addrWidth argNames interestingShapes cData
          doLog sla $ Diag.RefinementReach prettyData
          pure True
        _ -> pure False

initialArgs ::
  ( ext ~ MS.MacawExt arch
  , args ~ MS.CtxToCrucibleType (MS.ArchRegContext arch)
  , ret ~ MS.ArchRegStruct arch
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , CB.IsSymBackend sym bak
  , MS.SymArchConstraints arch
  , Integral (Elf.ElfWordType (MC.ArchAddrWidth arch))
  , Show (ArchReloc arch)
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  ScreachLogAction ->
  GreaseLogAction ->
  Conf.Config ->
  MM.Memory (MC.ArchAddrWidth arch) ->
  Maybe (GGE.TargetAddress (MC.ArchAddrWidth arch)) ->
  bak ->
  ArchContext arch ->
  Maybe (Elf.ElfHeaderInfo (MC.ArchAddrWidth arch)) ->
  IO (Shape.ArgShapes (MS.MacawExt arch) Shape.NoTag (MS.CtxToCrucibleType (MS.ArchRegContext arch)))
initialArgs sla gla conf mem mbTargetAddr bak archCtx mbEhi = do
  let opts = Conf.initPrecondOpts conf
  parsed <-
    case GO.initPrecondPath opts of
      Nothing -> pure Nothing
      Just path -> Just <$> loadInitialPreconditions sla path
  let mbAddr = MM.resolveAbsoluteAddr mem . GGE.getTargetAddress =<< mbTargetAddr
  r <- GMShp.macawInitArgShapes gla bak archCtx opts parsed mbEhi mem mbAddr
  case r of
    Left err -> userError sla (PP.pretty err)
    Right shapes -> pure shapes

-- | Analyze a program's 'CCR.SomeCFG'.
analyzeCfg ::
  forall arch ext args ret.
  ( ext ~ MS.MacawExt arch
  , arch ~ MX86.X86_64
  , args ~ (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch)
  , ret ~ MS.ArchRegStruct arch
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , ?parserHooks :: CSC.ParserHooks (MS.MacawExt arch)
  ) =>
  Conf.Config ->
  ScreachLogAction ->
  GreaseLogAction ->
  CFH.HandleAllocator ->
  ArchContext arch ->
  -- | 'Just' the 'LoadedELF' if analyzing a binary, 'Nothing' for
  -- S-expression programs.
  Maybe LoadedELF ->
  (forall sym. GM.SetupHook sym arch) ->
  GAL.ResolvedTargetLoc (MC.ArchAddrWidth arch) ->
  GMSH.ExecutingAddressAction arch ->
  GMOA.AddressOverrides arch ->
  GE.EntrypointCfgs (CCR.SomeCFG ext args ret) ->
  ( forall p sym rtp.
    ( SDSE.HasDistancesState p
    , RR.HasRecordState p p sym ext rtp
    , RR.HasReplayState p p sym ext rtp
    , WI.IsExprBuilder sym
    ) =>
    Sched.PrioritizationFunction p sym ext rtp
  ) ->
  IO ()
analyzeCfg conf sla gla halloc archCtx mbLoadedElf setupHook rtLoc execAction addrOvs entryCfgs pFunc = do
  let mbEntryAddr = entrypointAddr <$> mbLoadedElf
  let isEcfs = maybe False isECFS mbLoadedElf
  let mbEhi = loadedElf <$> mbLoadedElf
  let mem =
        maybe
          (MC.emptyMemory (archCtx ^. Arch.archInfo . to MAI.archAddrWidth))
          (Loader.memoryImage . GL.progLoadedBinary . loadedProgram)
          mbLoadedElf
  doLog sla $ Diag.SearchingForTarget rtLoc

  GE.EntrypointCfgs
    { GE.entrypointStartupOv = startupOv
    , GE.entrypointCfg = CCR.SomeCFG entryRegCfgSome
    } <-
    pure entryCfgs
  let mbStartupOvCfg = GE.startupOvCfg <$> startupOv

  entryRegSomeSsaCfg <- pure $ toSSA entryRegCfgSome
  let mbStartupOvSomeSsaCfg = fmap toSsaSomeCfg mbStartupOvCfg

  -- Fire up SMT solver
  let fm = W4.FloatRealRepr
  let solv = Conf.solver conf
  let nonceGen = Nonce.globalNonceGenerator
  Solver.withSolverOnlineBackend solv fm nonceGen $ \(bak :: CBO.OnlineBackend solver scope st fs) -> do
    -- TODO: Log these errors
    let dataLayout = macawDataLayout archCtx
    let (_tyCtxErrs, tyCtx) = CLTC.mkTypeContext dataLayout IntMap.empty []
    let ?lc = tyCtx

    -- The target address and the target symbol, if they exist. At least one of
    -- these will be 'Just' no matter what, as the user must specify the target
    -- location as an address or as a symbol. Furthermore, note that:
    --
    -- - 'mbTargetSymbol' must always be 'Just' when analyzing an S-expression
    --   program, as that is the only valid form of target location for
    --   S-expression programs.
    --
    -- - It is possible for 'mbTargetAddr' to be 'Nothing' when analyzing a
    --   binary. Even if the target symbol does not resolve to a function in the
    --   binary, it is still possible for the symbol to correspond to a local
    --   function defined in an override, so we continue the analysis in case
    --   that local function is reached.
    --
    -- - It is possible for /both/ 'mbTargetAddr' and 'mbTargetSymbol' to be
    --   'Just' when analyzing a binary! This happens when the user specifies
    --   the target via --target-symbol and the symbol name resolves to a
    --   function in the binary. In this case, screach detects if either the
    --   address is reached (e.g., via a direct function call) or the symbol is
    --   reached (e.g., via calling a local function in an override file).
    --
    --   TODO(internal#80): It is not clear what exactly we want the semantics of
    --   screach to be in the case where the target symbol corresponds to both
    --   a function in the binary as well as a local function in an override
    --   file. We may want to consider throwing an error or adding some sort of
    --   disambiguation mechanism instead.
    let (mbTargetAddr, mbTargetSymbol) =
          case rtLoc of
            GAL.ResolvedTargetLocAddress addr mbResolvedAddrAndSymbolName ->
              ( Just $ GGE.TargetAddress addr
              , fmap (GGE.TargetFunctionName . snd) mbResolvedAddrAndSymbolName
              )
            GAL.ResolvedTargetLocSymbol name mbAddr ->
              ( GGE.TargetAddress <$> mbAddr
              , Just $ GGE.TargetFunctionName name
              )
    initArgs <-
      initialArgs sla gla conf mem mbTargetAddr bak archCtx mbEhi

    let mbGoalEvaluatorExecFeature =
          -- We omit the goal evaluator in the case that we have a target
          -- override because the target override determines when we have truly
          -- reached the goal (by calling the `@reached` built-in).
          case Conf.targetOverride conf of
            Just _ ->
              Nothing
            Nothing ->
              GGE.goalEvaluatorExecFeature gla bak rtLoc <$> mbTargetSymbol
    -- Call GREASE's refinement loop
    let boundsOpts = Conf.boundsOpts conf
    memVar <- liftIO (CLM.mkMemVar "screach:mem" halloc)
    let initShape =
          initCFG
            entryRegSomeSsaCfg
            mbLoadedElf
            bak
            gla
            sla
            halloc
            conf
            archCtx
            mbTargetAddr
            mbStartupOvSomeSsaCfg
            rtLoc
            memVar
            setupHook
            execAction
            addrOvs

    boundsFeats <- GEF.boundedExecFeats boundsOpts
    let genericExecFeats =
          concat
            [ Maybe.maybeToList mbGoalEvaluatorExecFeature
            , [LocLog.loggingFeature sla]
            , boundsFeats
            ]
    let mbElf = snd . Elf.getElf <$> mbEhi
    let macawDbgExtImpl =
          MDebug.macawExtImpl prettyPtrFnMap memVar (archCtx ^. Arch.archVals) mbElf
    -- We want to apply path splitting before we apply record replay, so that replay
    -- aborts the state that was path split that is not on
    -- the replayed path. The logging feature should be last so it observes items right before
    -- they reach the Crucible execution itself.
    (sdseFeats, savedRef) <-
      RFT.sdseExecFeatures
        bak
        sla
        gla
        (Conf.refineReplay conf)
        initShape
        pFunc
        (handleTarget archCtx Proxy sla (initArgs ^. Shape.argShapes))
        (Conf.allSolutions conf)
    let startFeats =
          List.concat
            [ sdseFeats
            , [SR.replayFeature False]
            , [SR.recordFeature]
            , List.map CS.genericToExecutionFeature genericExecFeats
            , if GO.debug (Conf.debugOpts conf)
                then [Dbg.debugger macawDbgExtImpl]
                else []
            ]

    setupAssertThenAssume bak
    firstState <- initShape initArgs
    _result <- CS.executeCrucible startFeats firstState
    refineResults <- IORef.readIORef savedRef
    doLog sla $ Diag.RefinementResultCount $ length refineResults
    GRV.verifyReachable gla
      (List.map CS.genericToExecutionFeature genericExecFeats)
      initShape
      (map RFT.refineResultConcData refineResults)
 where
  setupAssertThenAssume bak = do
    let sym = CB.backendGetSym bak
    let cfg = WI.getConfiguration sym
    assertThenAssume <- W4C.getOptionSetting CB.assertThenAssumeConfigOption cfg
    -- This can technically return warnings/errors, but seems unlikely in this
    -- case...
    warns <- W4C.setOpt assertThenAssume True
    case warns of
      [] -> pure ()
      _else -> panic "setupAssertThenAssume" (List.map show warns)

runScreach :: Conf.Config -> ScreachLogAction -> IO ()
runScreach conf sla = do
  let progConf = Conf.programConfig conf
  let sev = Conf.verbosity progConf
  let gla :: GreaseLogAction
      gla = LJ.LogAction $ \diag ->
        Monad.when (GD.severity diag <= sev) $
          LJ.writeLog sla (SD.GreaseDiagnostic diag)

  let ?ptrWidth = NatRepr.knownNat @64
  let ?memOpts = CLM.defaultMemOptions
  let ?parserHooks = MSS.machineCodeParserHooks (Proxy @MX86.X86_64) MX86Syn.x86ParserHooks
  halloc <- CFH.newHandleAllocator
  archCtx <- do
    let retAddr = Nothing -- TODO
    let slots = Conf.stackArgumentSlots progConf
    x86Ctx halloc retAddr slots

  analyzeFile conf sla gla halloc archCtx
