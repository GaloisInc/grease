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
  registerAddressOverrideHandles,
  maybeRunAddressOverride,
) where

import Control.Applicative (empty)
import Control.Exception qualified as X
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Void (Void)
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Macaw.Arch (ArchContext, archVals)
import Grease.Macaw.Overrides qualified as GMO
import Grease.Macaw.Overrides.SExp (MacawSExpOverride)
import Grease.Panic (panic)
import Grease.Syntax (parseProgram)
import Grease.Utility (GreaseException (GreaseException), tshow)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as LCBO
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as LCCR
import Lang.Crucible.CFG.SSAConversion (toSSA)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as LCLM
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import System.FilePath qualified as FilePath
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.Expr qualified as W4
import What4.Expr.Builder qualified as WEB
import What4.FunctionName qualified as W4
import What4.Protocol.Online qualified as WPO

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

-- | An address override, corresponding to a single S-expression file.
data AddressOverride arch
  = AddressOverride
  { aoCfg ::
      C.SomeCFG
        (Symbolic.MacawExt arch)
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        C.UnitType
  -- ^ The override for the public function, whose name matches that of the
  -- S-expression file.
  , aoAuxiliaryOverrides :: [C.AnyCFG (Symbolic.MacawExt arch)]
  -- ^ Overrides for the auxiliary functions in the S-expression file.
  , aoForwardDeclarations :: Map.Map W4.FunctionName C.SomeHandle
  -- ^ The map of names of forward declarations in the S-expression file to
  -- their handles.
  }

newtype AddressOverrides arch
  = AddressOverrides (Map.Map (MC.ArchSegmentOff arch) (AddressOverride arch))

loadAddressOverride ::
  ( Symbolic.SymArchConstraints arch
  , ?parserHooks :: CSyn.ParserHooks (Symbolic.MacawExt arch)
  ) =>
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  C.HandleAllocator ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  -- | Absolute address to be overridden
  MM.MemWord (MC.ArchAddrWidth arch) ->
  FilePath ->
  IO (MC.ArchSegmentOff arch, AddressOverride arch)
loadAddressOverride archRegsType halloc memory addr path = do
  segOff <-
    case MM.resolveAbsoluteAddr memory addr of
      -- TODO improve error
      Nothing -> X.throw (GreaseException ("Bad address: " <> tshow addr))
      Just segOff -> pure segOff
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)

  let fnNameText =
        Text.pack $ FilePath.dropExtensions $ FilePath.takeBaseName path
  let fnName = W4.functionNameFromText fnNameText
  let (publicCfgs, auxRegCfgs) =
        List.partition (isPublicCblFun fnName) (CSyn.parsedProgCFGs prog)
  let userErr :: Text.Text -> IO a
      userErr msg = X.throw (GreaseException (msg <> " at " <> Text.pack path))
  case publicCfgs of
    [LCCR.AnyCFG defCfg] -> do
      Refl <-
        case testEquality (LCCR.cfgArgTypes defCfg) (Ctx.Empty Ctx.:> archRegsType) of
          Nothing -> userErr "Bad address override args"
          Just r -> pure r
      Refl <-
        case testEquality (LCCR.cfgReturnType defCfg) C.UnitRepr of
          Nothing -> userErr "Bad address override return value"
          Just r -> pure r
      let fwdDecs = CSyn.parsedProgForwardDecs prog
      auxCfgs <-
        Monad.forM auxRegCfgs $ \(LCCR.AnyCFG auxRegCfg) -> do
          C.SomeCFG auxCfg <- pure (toSSA auxRegCfg)
          pure (C.AnyCFG auxCfg)
      pure
        ( segOff
        , AddressOverride
            { aoCfg = toSSA defCfg
            , aoAuxiliaryOverrides = auxCfgs
            , aoForwardDeclarations = fwdDecs
            }
        )
    [] ->
      userErr ("Expected to find a function named `" <> fnNameText <> "`")
    _ : _ ->
      userErr ("Override contains multiple `" <> fnNameText <> "` functions")

-- | Does a function have the same name as the @.cbl@ file in which it is
-- defined? That is, is a function publicly visible from the @.cbl@ file?
isPublicCblFun :: W4.FunctionName -> LCCR.AnyCFG ext -> Bool
isPublicCblFun fnName (LCCR.AnyCFG cfg) =
  C.handleName (LCCR.cfgHandle cfg) == fnName

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
    let addr = fromIntegral intAddr
    loadAddressOverride archRegsType halloc memory addr path

---------------------------------------------------------------------

-- | Redirect function handles from forward declarations appearing in
-- 'AddressOverride's to their implementations.
registerAddressOverrideForwardDeclarations ::
  ( LCLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , C.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCBO.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  Map.Map W4.FunctionName (MacawSExpOverride p sym arch) ->
  AddressOverrides arch ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideForwardDeclarations bak funOvs addrOvs = do
  let AddressOverrides addrOvsMap = addrOvs
  Monad.forM_ (Map.elems addrOvsMap) $ \addrOv -> do
    let fwdDecs = aoForwardDeclarations addrOv
    GMO.registerMacawOvForwardDeclarations bak funOvs fwdDecs

-- | Register CFGs appearing an in 'AddressOverride'.
registerAddressOverrideCfgs ::
  LCLM.HasPtrWidth (MC.ArchAddrWidth arch) =>
  AddressOverrides arch ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideCfgs addrOvs = do
  let AddressOverrides addrOvsMap = addrOvs
  Monad.forM_ (Map.elems addrOvsMap) $ \addrOv -> do
    C.SomeCFG cfg <- pure (aoCfg addrOv)
    C.bindCFG cfg
    Monad.forM_ (aoAuxiliaryOverrides addrOv) $ \(C.AnyCFG auxCfg) ->
      C.bindCFG auxCfg

-- | Register all handles from 'AddressOverrides', including defined CFGs and
-- forwaAddressrd declarations.
registerAddressOverrideHandles ::
  ( LCLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , C.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCBO.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  Map.Map W4.FunctionName (MacawSExpOverride p sym arch) ->
  AddressOverrides arch ->
  C.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideHandles bak funOvs addrOvs = do
  registerAddressOverrideCfgs addrOvs
  registerAddressOverrideForwardDeclarations bak funOvs addrOvs

---------------------------------------------------------------------

toInitialState ::
  sym ~ W4.ExprBuilder t st fs =>
  C.CrucibleState p sym ext rtp blocks r args ->
  C.TypeRepr ret ->
  C.ExecCont p sym ext (C.RegEntry sym ret) (C.OverrideLang ret) ('Just Ctx.EmptyCtx) ->
  IO (C.ExecState p sym ext (C.RegEntry sym ret))
toInitialState crucState retTy action = do
  let ctx = crucState Lens.^. C.stateContext
  C.withBackend ctx $ \_bak ->
    pure $
      C.InitialState
        ctx
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
  C.withBackend (C.execStateContext initState) $ \_bak -> do
    execResult <- C.executeCrucible [] initState
    case execResult of
      C.FinishedResult _ (C.TotalRes{}) -> pure ()
      C.AbortedResult _ (C.AbortedExec reason _) -> C.abortExecBecause reason
      _ ->
        X.throw (GreaseException "Address override did not return a result nor abort")

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

-- | See if there is an address override corresponding to the current
-- instruction pointer value, and if so, run it.
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
    Just tgtOv -> do
      AddressOverride cfg _ _ <- pure tgtOv
      tryRunAddressOverride archCtx crucState cfg

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch =
  C.StructRepr . Symbolic.crucArchRegTypes $
    arch Lens.^. archVals . Lens.to Symbolic.archFunctions
