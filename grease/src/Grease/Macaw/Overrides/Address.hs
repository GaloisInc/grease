{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- See "Address overrides" in @doc/overrides.md@ for a high-level overview.
module Grease.Macaw.Overrides.Address (
  addressOverrideParser,
  AddressOverrides,
  loadAddressOverrides,
  maybeRunAddressOverride,
) where

import Control.Applicative (empty)
import Control.Exception qualified as X
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
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
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.Expr qualified as W4

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> TM.Parsec Void Text Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: TM.Parsec Void Text ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse the address and path to a address override  in the format
-- @ADDRESS:PATH@, where @ADDRESS@ is a hexadecimal number and @PATH@ is a
-- file path.
addressOverrideParser :: TM.Parsec Void Text (Integer, FilePath)
addressOverrideParser = do
  addr <- symbol "0x" *> TMCL.hexadecimal
  _ <- TMC.char ':'
  path <- TM.takeWhileP (Just "path") (/= ':')
  pure (addr, Text.unpack path)

---------------------------------------------------------------------

newtype AddressOverrides arch
  = AddressOverrides (Map.Map (MC.ArchSegmentOff arch) (C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.UnitType))

-- | Parse and register address overrides in the Macaw S-expression syntax.
--
-- Address overrides cannot use @extern@.
loadAddressOverrides ::
  forall arch.
  ( Symbolic.SymArchConstraints arch
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  C.HandleAllocator ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  [(Integer, FilePath)] ->
  IO (AddressOverrides arch)
loadAddressOverrides archRegsType halloc memory paths =
  fmap (AddressOverrides . Map.fromList) $ Monad.forM paths $ \(intAddr, path) -> do
    segOff <-
      case MM.resolveAbsoluteAddr memory (fromIntegral intAddr) of
        -- TODO improve error
        Nothing -> X.throw (GreaseException ("Bad address: " <> tshow intAddr))
        Just segOff -> pure segOff
    prog <- parseProgram halloc path
    CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
    let userErr :: Text.Text -> IO a
        userErr msg = X.throw (GreaseException (msg <> " at " <> Text.pack path))
    case CSyn.parsedProgCFGs prog of
      [] -> userErr "No CFGs in address override"
      (LCCR.AnyCFG defCfg : []) -> do
        Refl <-
          case testEquality (LCCR.cfgArgTypes defCfg) (Ctx.Empty Ctx.:> archRegsType) of
            Nothing -> userErr "Bad address override args"
            Just r -> pure r
        Refl <-
          case testEquality (LCCR.cfgReturnType defCfg) C.UnitRepr of
            Nothing -> userErr "Bad address override return value"
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

-- When invoked, address overrides don't run in the current symbolic simulator.
-- Instead, they construct a new backend and simulator, and run there. This is
-- because Crucible can't just start running a CFG from an arbitrary state.
runAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.UnitType ->
  IO ()
runAddressOverride crucState someCfg = do
  let regs = crucState Lens.^. C.stateCrucibleFrame . C.frameRegs
  C.SomeCFG cfg <- pure someCfg
  initState <-
    toInitialState crucState C.UnitRepr $
      C.runOverrideSim C.UnitRepr $
        C.regValue <$> C.callCFG cfg regs
  let ctx = C.execStateContext initState
  C.withBackend ctx $ \bak' -> do
    execResult <- C.executeCrucible [] initState
    mbGoals <- C.getProofObligations bak'
    case mbGoals of
      Just _ ->
        -- TODO: Lift this assumption someday by traversing and re-asserting goals
        X.throw (GreaseException "Address overrides should not generate proof goals")
      Nothing ->
        case execResult of
          C.FinishedResult _ (C.TotalRes{}) -> pure ()
          _ -> X.throw (GreaseException "Address override did not terminate successfully")

tryRunAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.UnitType ->
  IO ()
tryRunAddressOverride archCtx crucState cfg = do
  let C.RegMap regMap = crucState Lens.^. C.stateCrucibleFrame . C.frameRegs
  case Ctx.viewAssign regMap of
    Ctx.AssignExtend Ctx.Empty regs ->
      case testEquality (C.regType regs) (regStructRepr archCtx) of
        Just Refl -> runAddressOverride crucState cfg
        Nothing ->
          panic
            "extensionExec"
            [ "Inside ill-typed Macaw CFG"
            , show (C.regType regs)
            ]
    _ -> panic "extensionExec" ["Inside Macaw CFG with bad arity"]

maybeRunAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  -- | Current instruction pointer
  MC.ArchSegmentOff arch ->
  AddressOverrides arch ->
  IO ()
maybeRunAddressOverride archCtx crucState segOff (AddressOverrides tgtOvs) =
  case Map.lookup segOff tgtOvs of
    Nothing -> pure ()
    Just tgtOv -> tryRunAddressOverride archCtx crucState tgtOv

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch =
  C.StructRepr . Symbolic.crucArchRegTypes $
    arch Lens.^. archVals . Lens.to Symbolic.archFunctions
