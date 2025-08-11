{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- See "Target overrides" in @doc/overrides.md@ for a high-level overview.
module Grease.Macaw.Overrides.Target (
  targetOverrideParser,
  TargetOverrides,
  loadTargetOverrides,
  maybeRunTargetOverride,
) where

import Control.Applicative (empty)
import Control.Exception qualified as X
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Void (Void)
import Grease.Macaw.Arch (ArchContext, archVals)
import Grease.Panic (panic)
import Grease.Syntax (parseProgram)
import Grease.Utility (GreaseException (GreaseException), tshow)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Simple qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as LCCR
import Lang.Crucible.CFG.SSAConversion (toSSA)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Syntax (emptyParserHooks)
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.Expr qualified as W4
import What4.Interface qualified as W4

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> TM.Parsec Void Text Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: TM.Parsec Void Text ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse the address and path to a target override  in the format
-- @ADDRESS:PATH@, where @ADDRESS@ is a hexadecimal number and @PATH@ is a
-- file path.
targetOverrideParser :: TM.Parsec Void Text (Integer, FilePath)
targetOverrideParser = do
  addr <- symbol "0x" *> TMCL.hexadecimal
  _ <- TMC.char ':'
  path <- TM.takeWhileP (Just "path") (/= ':')
  pure (addr, Text.unpack path)

---------------------------------------------------------------------

newtype TargetOverrides arch
  = TargetOverrides (Map.Map (MC.ArchSegmentOff arch) (C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.BoolType))

-- | Parse and register target overrides in the Macaw S-expression syntax.
--
-- Target overrides cannot use @extern@.
loadTargetOverrides ::
  forall arch.
  Symbolic.SymArchConstraints arch =>
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  C.HandleAllocator ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  [(Integer, FilePath)] ->
  IO (TargetOverrides arch)
loadTargetOverrides archRegsType halloc memory paths =
  fmap (TargetOverrides . Map.fromList) $ Monad.forM paths $ \(intAddr, path) -> do
    segOff <-
      case MM.resolveAbsoluteAddr memory (fromIntegral intAddr) of
        -- TODO improve error
        Nothing -> X.throw (GreaseException ("Bad addresss: " <> tshow intAddr))
        Just segOff -> pure segOff
    let ?parserHooks = machineCodeParserHooks (Proxy @arch) emptyParserHooks
    prog <- parseProgram halloc path
    CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
    let userErr :: Text.Text -> IO a
        userErr msg = X.throw (GreaseException (msg <> " at " <> Text.pack path))
    case CSyn.parsedProgCFGs prog of
      [] -> userErr "No CFGs in target override"
      (LCCR.AnyCFG defCfg : []) -> do
        Refl <-
          case testEquality (LCCR.cfgArgTypes defCfg) (Ctx.Empty Ctx.:> archRegsType) of
            Nothing -> userErr "Bad target override args"
            Just r -> pure r
        Refl <-
          case testEquality (LCCR.cfgReturnType defCfg) C.BoolRepr of
            Nothing -> userErr "Bad target override return value"
            Just r -> pure r
        pure (segOff, toSSA defCfg)
      -- TODO(lb): Why does GHC 9.8.4 reject `userErr` here?
      (_ : _) -> X.throw (GreaseException ("Too many CFGs at " <> Text.pack path))

---------------------------------------------------------------------

withFreshBackend ::
  ( C.IsSymBackend sym bak
  , sym ~ W4.ExprBuilder t st fs
  ) =>
  bak ->
  (C.SomeBackend sym -> IO r) ->
  IO r
withFreshBackend bak k = do
  st <- C.saveAssumptionState bak
  let sym = C.backendGetSym bak
  bak' <- C.newSimpleBackend sym
  C.restoreAssumptionState bak st
  k (C.SomeBackend bak')

-- TODO: Upstream to Crucible?
toInitialState ::
  sym ~ W4.ExprBuilder t st fs =>
  C.CrucibleState p sym ext rtp blocks r args ->
  C.TypeRepr ret ->
  C.ExecCont p sym ext (C.RegEntry sym ret) (C.OverrideLang ret) ('Just Ctx.EmptyCtx) ->
  IO (C.ExecState p sym ext (C.RegEntry sym ret))
toInitialState crucState retTy action = do
  let ctx = crucState Lens.^. C.stateContext
  C.withBackend ctx $ \bak ->
    withFreshBackend bak $ \bak' -> do
      let ctx' = ctx{C._ctxBackend = bak'}
      pure $
        C.InitialState
          ctx'
          (crucState Lens.^. C.stateGlobals)
          C.defaultAbortHandler -- (crucState Lens.^. C.abortHandler)
          retTy
          action

-- When invoked, target overrides don't run in the current symbolic simulator.
-- Instead, they construct a new backend and simulator, and run there. This is
-- because Crucible can't just start running a CFG from an arbitrary state.
runTargetOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.BoolType ->
  IO (W4.Pred sym)
runTargetOverride crucState someCfg = do
  let regs = crucState Lens.^. C.stateCrucibleFrame . C.frameRegs
  C.SomeCFG cfg <- pure someCfg
  initState <-
    toInitialState crucState C.BoolRepr $
      C.runOverrideSim C.BoolRepr $
        C.regValue <$> C.callCFG cfg regs
  let ctx = C.execStateContext initState
  C.withBackend ctx $ \bak' -> do
    execResult <- C.executeCrucible [] initState
    mbGoals <- C.getProofObligations bak'
    case mbGoals of
      Just _ ->
        -- TODO: Lift this assumption someday by traversing and re-asserting goals
        X.throw (GreaseException "Target overrides should not generate proof goals")
      Nothing ->
        case execResult of
          C.FinishedResult _ (C.TotalRes gp) -> pure (C.regValue (gp Lens.^. C.gpValue))
          _ -> X.throw (GreaseException "Target override did not terminate successfully")

tryRunTargetOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.BoolType ->
  IO (W4.Pred sym)
tryRunTargetOverride archCtx crucState cfg = do
  let C.RegMap regMap = crucState Lens.^. C.stateCrucibleFrame . C.frameRegs
  case Ctx.viewAssign regMap of
    Ctx.AssignExtend Ctx.Empty regs ->
      case testEquality (C.regType regs) (regStructRepr archCtx) of
        Just Refl -> runTargetOverride crucState cfg
        Nothing ->
          panic
            "extensionExec"
            [ "Inside ill-typed Macaw CFG"
            , show (C.regType regs)
            ]
    _ -> panic "extensionExec" ["Inside Macaw CFG with bad arity"]

maybeRunTargetOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  -- | Current instruction pointer
  MC.ArchSegmentOff arch ->
  TargetOverrides arch ->
  IO ()
maybeRunTargetOverride archCtx crucState segOff (TargetOverrides tgtOvs) =
  case Map.lookup segOff tgtOvs of
    Nothing -> pure ()
    Just tgtOv ->
      C.withBackend (crucState Lens.^. C.stateContext) $ \bak -> do
        p <- tryRunTargetOverride archCtx crucState tgtOv
        C.assert bak p (C.GenericSimError "Assertion from target override")
        pure ()

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch =
  C.StructRepr . Symbolic.crucArchRegTypes $
    arch Lens.^. archVals . Lens.to Symbolic.archFunctions
