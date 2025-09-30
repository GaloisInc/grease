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
  Results (..),
) where

import Control.Applicative (pure)
import Control.Concurrent.Async (Async)
import Control.Exception.Safe (throw)
import Control.Lens (to, (.~), (^.))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function
import Data.Functor
import Data.Functor.Const (Const (..))
import Data.Functor.Const qualified as Const
import Data.List qualified as List
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Memory.Lazy qualified as Symbolic
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord ((<=))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.TraversableFC.WithIndex (imapFC)
import Data.Semigroup ((<>))
import Data.Sequence qualified as Seq
import Data.String (String)
import Data.Text qualified as Text
import Data.Traversable (traverse)
import Data.Tuple
import Data.Type.Equality (type (~))
import Grease.Bug qualified as Bug
import Grease.Concretize (ConcretizedData)
import Grease.Concretize.ToConcretize qualified as ToConc
import Grease.Diagnostic
import Grease.Entrypoint
import Grease.ExecutionFeatures (greaseExecFeats)
import Grease.Heuristic
import Grease.LLVM qualified as LLVM
import Grease.LLVM.Overrides.SExp qualified as GLOS
import Grease.LLVM.SetupHook qualified as LLVM (SetupHook)
import Grease.Macaw.Arch
import Grease.Macaw.Overrides.Address (AddressOverrides)
import Grease.Main.Diagnostic qualified as Diag
import Grease.Options
import Grease.Output
import Grease.Profiler.Feature (greaseProfilerFeature)
import Grease.Refine
import Grease.Setup
import Grease.Shape (ArgShapes (..), ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Simple qualified as Simple
import Grease.SymIO qualified as GSIO
import Grease.Time (time)
import Grease.Utility
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.DataLayout (DataLayout)
import Lang.Crucible.LLVM.DataLayout qualified as DataLayout
import Lang.Crucible.LLVM.Debug qualified as Debug
import Lang.Crucible.LLVM.Extension qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Translation qualified as Trans
import Lang.Crucible.LLVM.TypeContext qualified as TCtx
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lumberjack qualified as LJ
import System.FilePath (FilePath)
import System.IO (IO)
import Text.LLVM qualified as L
import Text.Show (Show (..))
import What4.Expr qualified as W4
import What4.FunctionName qualified as W4
import Prelude (Int, undefined)

-- | Results of analysis, one per given 'Entrypoint'
newtype Results = Results {getResults :: Map Entrypoint Batch}

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (MainDiagnostic diag)

-- | Define @?memOpts@ in a continuation, setting the 'Mem.laxLoadsAndStores'
-- option according to whether the user set the @--rust@ flag.
withMemOptions :: SimOpts -> ((?memOpts :: Mem.MemOptions) => r) -> r
withMemOptions _ _ = undefined

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

-- | Create a 'DataLayout' suitable for @macaw-symbolic@'s needs. Currently,
-- this simply overrides the 'DataLayout.defaultDataLayout' with a reasonable
-- endianness value based on the architecture.
macawDataLayout :: ArchContext arch -> DataLayout
macawDataLayout archCtx =
  DataLayout.defaultDataLayout
    & DataLayout.intLayout
      .~ (archCtx ^. archInfo . to (Symbolic.toCrucibleEndian . MI.archEndianness))

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
entrypointCfgMap la halloc prog entries = undefined

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
loadAddrOvs archCtx halloc memory simOpts = undefined

loadLLVMSExpOvs ::
  Mem.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Seq.Seq (W4.FunctionName, GLOS.LLVMSExpOverride))
loadLLVMSExpOvs sexpOvPaths halloc mvar = undefined

llvmInitArgShapes ::
  Mem.HasPtrWidth 64 =>
  GreaseLogAction ->
  InitialPreconditionOpts ->
  Maybe L.Module ->
  Ctx.Assignment (Const String) argTys ->
  -- | The CFG of the user-requested entrypoint function.
  C.CFG CLLVM.LLVM blocks argTys ret ->
  IO (ArgShapes CLLVM.LLVM NoTag argTys)
llvmInitArgShapes la opts llvmMod argNames cfg = undefined

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
            undefined
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

  pure undefined

main :: IO ()
main = pure ()
