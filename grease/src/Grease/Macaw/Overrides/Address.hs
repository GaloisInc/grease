{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- See "Address overrides" in @doc/overrides.md@ for a high-level overview.
module Grease.Macaw.Overrides.Address (
  addressOverrideParser,
  AddressOverrides,
  AddressOverrideError (..),
  loadAddressOverrides,
  registerAddressOverrideHandles,
  maybeRunAddressOverride,
) where

import Control.Applicative (empty)
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Macaw.Arch (ArchContext, archVals)
import Grease.Macaw.Overrides qualified as GMO
import Grease.Macaw.Overrides.SExp (MacawSExpOverride)
import Grease.Overrides (CantResolveOverrideCallback (..), OverrideNameError (..), partitionCfgs)
import Grease.Syntax (ParseProgramError, parseProgram)
import Grease.Utility (tshow)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as LCBO
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as LCCR
import Lang.Crucible.CFG.SSAConversion (toSSA)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.CallFrame qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as C
import Lang.Crucible.Simulator.GlobalState qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Prettyprinter qualified as PP
import System.FilePath qualified as FilePath
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.Expr qualified as W4
import What4.Expr.Builder qualified as WEB
import What4.FunctionName qualified as WFN
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

-- | Error type for 'loadAddressOverrides'.
data AddressOverrideError arch
  = BadAddress Text FilePath
  | BadAddressOverrideArgs
      FilePath
      (C.TypeRepr (Symbolic.ArchRegStruct arch))
      (C.Some C.CtxRepr)
  | BadAddressOverrideReturn FilePath (C.Some C.TypeRepr)
  | OverrideNameError OverrideNameError
  | AddressOverrideParseError ParseProgramError

instance PP.Pretty (AddressOverrideError arch) where
  pretty =
    \case
      BadAddress addr path ->
        PP.nest 2 $
          "Bad address:" PP.<+> PP.pretty addr PP.<+> "at" PP.<+> PP.pretty path
      BadAddressOverrideArgs path expected (C.Some actual) ->
        PP.nest 2 $
          PP.vcat
            [ "Bad address override argument types at"
                PP.<+> PP.pretty path
            , "Expected:" PP.<+> PP.pretty expected
            , "Actual:" PP.<+> PP.list (TFC.toListFC PP.pretty actual)
            , "See https://galoisinc.github.io/grease/overrides.html#address-overrides"
            ]
      BadAddressOverrideReturn path (C.Some actual) ->
        PP.nest 2 $
          PP.vcat
            [ "Bad address override return type at"
                PP.<+> PP.pretty path
            , "Expected:" PP.<+> PP.pretty C.UnitRepr
            , "Actual:" PP.<+> PP.pretty actual
            , "See https://galoisinc.github.io/grease/overrides.html#address-overrides"
            ]
      OverrideNameError err -> PP.pretty err
      AddressOverrideParseError err -> PP.pretty err

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
  , aoForwardDeclarations :: Map.Map WFN.FunctionName C.SomeHandle
  -- ^ The map of names of forward declarations in the S-expression file to
  -- their handles.
  }

newtype AddressOverrides arch
  = AddressOverrides (Map.Map (MC.ArchSegmentOff arch) (AddressOverride arch))

typecheckAddressOverrideCfg ::
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  FilePath ->
  LCCR.CFG (Symbolic.MacawExt arch) blocks args ret ->
  Either
    (AddressOverrideError arch)
    ( LCCR.CFG
        (Symbolic.MacawExt arch)
        blocks
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        C.UnitType
    )
typecheckAddressOverrideCfg archRegsType path cfg = do
  let argTys = LCCR.cfgArgTypes cfg
  Refl <-
    case testEquality argTys (Ctx.Empty Ctx.:> archRegsType) of
      Just r -> Right r
      Nothing ->
        Left (BadAddressOverrideArgs path archRegsType (C.Some argTys))
  let retTy = LCCR.cfgReturnType cfg
  Refl <-
    case testEquality retTy C.UnitRepr of
      Just r -> Right r
      Nothing ->
        Left (BadAddressOverrideReturn path (C.Some retTy))
  Right cfg

parsedProgToAddressOverride ::
  Symbolic.SymArchConstraints arch =>
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  MM.MemWord (MC.ArchAddrWidth arch) ->
  FilePath ->
  CSyn.ParsedProgram (Symbolic.MacawExt arch) ->
  Either (AddressOverrideError arch) (MC.ArchSegmentOff arch, AddressOverride arch)
parsedProgToAddressOverride archRegsType memory addr path prog = do
  segOff <-
    case MM.resolveAbsoluteAddr memory addr of
      Nothing -> Left (BadAddress (tshow addr) path)
      Just segOff -> Right segOff

  let fnNameText =
        Text.pack $ FilePath.dropExtensions $ FilePath.takeBaseName path
  let fnName = WFN.functionNameFromText fnNameText

  (LCCR.AnyCFG mainCfg, auxRegCfgs) <-
    case partitionCfgs fnName path prog of
      Left err -> Left (OverrideNameError err)
      Right result -> Right result
  defCfg <- typecheckAddressOverrideCfg archRegsType path mainCfg

  let fwdDecs = CSyn.parsedProgForwardDecs prog
  auxCfgs <-
    Monad.forM auxRegCfgs $ \(LCCR.AnyCFG auxRegCfg) -> do
      C.SomeCFG auxCfg <- pure (toSSA auxRegCfg)
      pure (C.AnyCFG auxCfg)

  let ao =
        AddressOverride
          { aoCfg = toSSA defCfg
          , aoAuxiliaryOverrides = auxCfgs
          , aoForwardDeclarations = fwdDecs
          }
  Right (segOff, ao)

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
  IO (Either (AddressOverrideError arch) (MC.ArchSegmentOff arch, AddressOverride arch))
loadAddressOverride archRegsType halloc memory addr path = do
  progResult <- parseProgram halloc path
  case progResult of
    Left err -> pure $ Left $ AddressOverrideParseError err
    Right prog -> do
      CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
      pure $ parsedProgToAddressOverride archRegsType memory addr path prog

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
  IO (Either (AddressOverrideError arch) (AddressOverrides arch))
loadAddressOverrides archRegsType halloc memory paths = do
  results <-
    Monad.forM paths $ \(intAddr, path) -> do
      let addr = fromIntegral intAddr
      loadAddressOverride archRegsType halloc memory addr path

  case sequence results of
    Left err -> pure (Left err)
    Right rs -> pure (Right (AddressOverrides (Map.fromList rs)))

---------------------------------------------------------------------

-- | Redirect function handles from forward declarations appearing in
-- 'AddressOverride's to their implementations.
registerAddressOverrideForwardDeclarations ::
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , CB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCBO.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  -- | What to do when a forward declaration cannot be resolved.
  CantResolveOverrideCallback sym (Symbolic.MacawExt arch) ->
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  AddressOverrides arch ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideForwardDeclarations bak errCb funOvs addrOvs = do
  let AddressOverrides addrOvsMap = addrOvs
  Monad.forM_ (Map.elems addrOvsMap) $ \addrOv -> do
    let fwdDecs = aoForwardDeclarations addrOv
    GMO.registerMacawOvForwardDeclarations bak funOvs errCb fwdDecs

-- | Register CFGs appearing an in 'AddressOverride'.
registerAddressOverrideCfgs ::
  CLM.HasPtrWidth (MC.ArchAddrWidth arch) =>
  AddressOverrides arch ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideCfgs addrOvs = do
  let AddressOverrides addrOvsMap = addrOvs
  Monad.forM_ (Map.elems addrOvsMap) $ \addrOv -> do
    C.SomeCFG cfg <- pure (aoCfg addrOv)
    CS.bindCFG cfg
    Monad.forM_ (aoAuxiliaryOverrides addrOv) $ \(C.AnyCFG auxCfg) ->
      CS.bindCFG auxCfg

-- | Register all handles from 'AddressOverrides', including defined CFGs and
-- forwaAddressrd declarations.
registerAddressOverrideHandles ::
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , CB.IsSymBackend sym bak
  , WPO.OnlineSolver solver
  , sym ~ WEB.ExprBuilder scope st fs
  , bak ~ LCBO.OnlineBackend solver scope st fs
  , HasToConcretize p
  ) =>
  bak ->
  -- | What to do when a forward declaration cannot be resolved.
  CantResolveOverrideCallback sym (Symbolic.MacawExt arch) ->
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  AddressOverrides arch ->
  CS.OverrideSim p sym (Symbolic.MacawExt arch) rtp a r ()
registerAddressOverrideHandles bak errCb funOvs addrOvs = do
  registerAddressOverrideCfgs addrOvs
  registerAddressOverrideForwardDeclarations bak errCb funOvs addrOvs

---------------------------------------------------------------------

toInitialState ::
  sym ~ W4.ExprBuilder t st fs =>
  C.GlobalVar CLM.Mem ->
  C.CrucibleState p sym ext rtp blocks r args ->
  C.TypeRepr ret ->
  CS.ExecCont p sym ext (CS.RegEntry sym ret) (C.OverrideLang ret) ('Just Ctx.EmptyCtx) ->
  IO (CS.ExecState p sym ext (CS.RegEntry sym ret))
toInitialState memVar crucState retTy action = do
  let ctx = crucState Lens.^. C.stateContext
  let globals =
        case C.lookupGlobal memVar (crucState Lens.^. C.stateGlobals) of
          Nothing -> C.emptyGlobals
          Just mem -> C.insertGlobal memVar mem C.emptyGlobals
  C.ctxSolverProof ctx $
    pure $
      C.InitialState
        ctx
        globals
        CS.defaultAbortHandler -- (crucState Lens.^. CS.abortHandler)
        retTy
        action

-- When invoked, address overrides don't run in the current symbolic simulator.
-- Instead, they construct a new backend and simulator, and run there. This is
-- because Crucible can't just start running a CFG from an arbitrary state.
runAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  , HasCallStack
  ) =>
  C.GlobalVar CLM.Mem ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.UnitType ->
  CS.RegEntry sym (Symbolic.ArchRegStruct arch) ->
  IO ()
runAddressOverride memVar crucState someCfg regs = do
  C.SomeCFG cfg <- pure someCfg
  initState <-
    toInitialState memVar crucState C.UnitRepr $
      CS.runOverrideSim C.UnitRepr $
        CS.regValue <$> CS.callCFG cfg (CS.RegMap (Ctx.singleton regs))
  C.ctxSolverProof (C.execStateContext initState) $ do
    execResult <- CS.executeCrucible [] initState
    case execResult of
      C.FinishedResult _ (C.TotalRes{}) -> pure ()
      C.FinishedResult _ (C.PartialRes loc p _gp _aborted) -> do
        let ctx = crucState Lens.^. C.stateContext
        C.withBackend ctx $ \bak -> do
          CB.assert bak p (CS.GenericSimError ("Assertion from address override at " ++ show loc))
          pure ()
      C.AbortedResult _ (C.AbortedExec reason _) -> CB.abortExecBecause reason
      _ -> do
        let ctx = crucState Lens.^. C.stateContext
        C.withBackend ctx $ \bak -> do
          let sym = CB.backendGetSym bak
          let msg = "Address override did not return a result nor abort"
          CB.throwUnsupported sym msg

tryRunAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.GlobalVar CLM.Mem ->
  C.GlobalVar (Symbolic.ArchRegStruct arch) ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  C.SomeCFG (Symbolic.MacawExt arch) (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) C.UnitType ->
  IO ()
tryRunAddressOverride archCtx memVar archRegs crucState cfg = do
  let mbRegs = C.lookupGlobal archRegs (crucState Lens.^. C.stateGlobals)
  case mbRegs of
    Nothing -> pure ()
    Just regs ->
      let regsEntry = CS.RegEntry (regStructRepr archCtx) regs
       in runAddressOverride memVar crucState cfg regsEntry

-- | See if there is an address override corresponding to the current
-- instruction pointer value, and if so, run it.
maybeRunAddressOverride ::
  ( sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  C.GlobalVar CLM.Mem ->
  C.GlobalVar (Symbolic.ArchRegStruct arch) ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r args ->
  -- | Current instruction pointer
  MC.ArchSegmentOff arch ->
  AddressOverrides arch ->
  IO ()
maybeRunAddressOverride archCtx memVar archRegs crucState segOff (AddressOverrides tgtOvs) =
  case Map.lookup segOff tgtOvs of
    Nothing -> pure ()
    Just tgtOv -> do
      AddressOverride cfg _ _ <- pure tgtOv
      tryRunAddressOverride archCtx memVar archRegs crucState cfg

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch =
  C.StructRepr . Symbolic.crucArchRegTypes $
    arch Lens.^. archVals . Lens.to Symbolic.archFunctions
