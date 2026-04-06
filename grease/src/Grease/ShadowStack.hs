{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- Shadow stack execution feature for detecting return address
-- corruption. At each function call, the return address is saved using
-- 'archFunctionReturnAddr'. At each return, corruption is detected by reading
-- the instruction pointer from the return-time register state (which Macaw sets
-- to the return address as part of the terminator's effects) and comparing it
-- to the saved value.
--
-- Each shadow stack entry records the callee's function name.  At return time,
-- entries are popped until one matching the returning function is found.  This
-- tolerates unbalanced call\/return events that arise when SSE (Single-Shot
-- Execution) explores conditional function calls.
module Grease.ShadowStack (
  ShadowStack,
  newShadowStack,
  pushReturnAddr,
  shadowStackFeature,
  shadowStackCorruptionMsg,
) where

import Control.Lens ((^.))
import Data.BitVector.Sized qualified as BV
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Classes (testEquality)
import Data.Parameterized.Context qualified as Ctx
import Data.Type.Equality ((:~:)(Refl))
import Grease.Macaw.Arch (ArchContext, archFunctionReturnAddr, archGetIP, archRegStructType, archVals)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Types qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.CallFrame qualified as CF
import Lang.Crucible.Simulator.EvalStmt qualified as CS (ExecutionFeature (..), ExecutionFeatureResult (..))
import Lang.Crucible.Simulator.ExecutionTree qualified as CS
import Lang.Crucible.Simulator.GlobalState qualified as CS (lookupGlobal)
import What4.Expr qualified as WE
import What4.FunctionName (FunctionName)
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL
import What4.Protocol.Online qualified as WPO

-- | An entry on the shadow stack.
data ShadowEntry w
  = -- | A real (discovered) function call with the callee's function name
    -- and the saved return address.
    RealCall !FunctionName (Maybe (MM.MemWord w))
  | -- | An override/skipped function call; skip the check on return.
    OverrideEntry !FunctionName

-- | Get the function name from a shadow entry.
entryName :: ShadowEntry w -> FunctionName
entryName (RealCall n _) = n
entryName (OverrideEntry n) = n

-- | Mutable shadow stack storing saved return addresses.
newtype ShadowStack w = ShadowStack
  { ssStack :: IORef [ShadowEntry w]
  }

-- | Create a new, empty shadow stack.
newShadowStack :: IO (ShadowStack w)
newShadowStack = ShadowStack <$> newIORef []

-- | Push a return address onto the shadow stack (for the entry function push
-- from Main.hs where we don't have a baseline).
pushReturnAddr :: ShadowStack w -> FunctionName -> Maybe (MM.MemWord w) -> IO ()
pushReturnAddr ss name addr = modifyIORef' (ssStack ss) (RealCall name addr :)

-- | An 'CS.ExecutionFeature' that maintains a shadow stack to detect
-- return address corruption.
shadowStackFeature ::
  forall arch sym bak solver scope st fs p w.
  ( CB.IsSymBackend sym bak
  , bak ~ CBO.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , Symbolic.SymArchConstraints arch
  , w ~ MC.ArchAddrWidth arch
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  ArchContext arch ->
  CS.GlobalVar CLM.Mem ->
  ShadowStack w ->
  IO
    ( CS.ExecutionFeature
        p
        sym
        (Symbolic.MacawExt arch)
        (CS.RegEntry sym (Symbolic.ArchRegStruct arch))
    )
shadowStackFeature bak archCtx memVar ss = do
  let archRegStructTy = archCtx ^. archRegStructType
  pure $ CS.ExecutionFeature $ \case
    CS.CallState _rh resolvedCall simSt -> do
      handleCallState bak archCtx archRegStructTy memVar ss resolvedCall (simSt ^. CS.stateGlobals)
      pure CS.ExecutionFeatureNoChange
    CS.ReturnState retName _vfv retEntry _simSt -> do
      handleReturnState bak archCtx archRegStructTy ss retName retEntry
      pure CS.ExecutionFeatureNoChange
    _ -> pure CS.ExecutionFeatureNoChange

-- | Handle a 'CS.CallState': push the return address onto the shadow stack.
handleCallState ::
  forall arch sym bak solver scope st fs p ret w.
  ( CB.IsSymBackend sym bak
  , bak ~ CBO.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , Symbolic.SymArchConstraints arch
  , w ~ MC.ArchAddrWidth arch
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  ArchContext arch ->
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  CS.GlobalVar CLM.Mem ->
  ShadowStack w ->
  CS.ResolvedCall p sym (Symbolic.MacawExt arch) ret ->
  CS.SymGlobalState sym ->
  IO ()
handleCallState bak archCtx archRegStructTy memVar ss resolvedCall globals =
  case resolvedCall of
    CS.CrucibleCall _bid callFrame ->
      case testEquality (CF.frameReturnType callFrame) archRegStructTy of
        Just Refl ->
          case extractRegs archRegStructTy (CF._frameRegs callFrame) of
            Just regs ->
              case CS.lookupGlobal memVar globals of
                Just mem -> do
                  let calleeName = case CF.frameHandle callFrame of
                        C.SomeHandle h -> C.handleName h
                  mbAddr <- readRetAddrAtCall bak archCtx regs mem
                  modifyIORef' (ssStack ss) (RealCall calleeName mbAddr :)
                Nothing -> pure ()
            Nothing -> pure ()
        Nothing -> pure ()
    CS.OverrideCall _ov ovFrame ->
      case extractRegs archRegStructTy (ovFrame ^. CF.overrideRegMap) of
        Just _regs -> do
          let calleeName = ovFrame ^. CF.override
          modifyIORef' (ssStack ss) (OverrideEntry calleeName :)
        Nothing -> pure ()

-- | Handle a 'CS.ReturnState': pop the matching entry and check that the
-- return-time IP matches the saved return address.
handleReturnState ::
  forall arch sym bak solver scope st fs ret w.
  ( CB.IsSymBackend sym bak
  , bak ~ CBO.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , Symbolic.SymArchConstraints arch
  , w ~ MC.ArchAddrWidth arch
  ) =>
  bak ->
  ArchContext arch ->
  C.TypeRepr (Symbolic.ArchRegStruct arch) ->
  ShadowStack w ->
  FunctionName ->
  CS.RegEntry sym ret ->
  IO ()
handleReturnState bak archCtx archRegStructTy ss retName retEntry =
  case testEquality (CS.regType retEntry) archRegStructTy of
    Just Refl -> do
      mbMatch <- popMatchingEntry ss retName
      case mbMatch of
        Just (RealCall _name mbSaved) -> do
          mbCurrent <- readIP archCtx (CS.regValue retEntry)
          case (mbSaved, mbCurrent) of
            (Just saved, Just current) | saved /= current -> assertCorruption bak
            _ -> pure ()
        Just (OverrideEntry {}) -> pure ()
        Nothing -> pure ()
    Nothing -> pure ()

-- | Pop entries from the shadow stack until finding one whose callee name
-- matches the given function name, or the stack is exhausted.
popMatchingEntry ::
  ShadowStack w ->
  FunctionName ->
  IO (Maybe (ShadowEntry w))
popMatchingEntry ss name = do
  stack <- readIORef (ssStack ss)
  case stack of
    [] -> pure Nothing
    (entry : rest) -> do
      writeIORef (ssStack ss) rest
      if entryName entry == name
        then pure (Just entry)
        else popMatchingEntry ss name

-- | Add a proof obligation asserting that stack corruption was detected.
assertCorruption ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  ) =>
  bak ->
  IO ()
assertCorruption bak = do
  let sym = CB.backendGetSym bak
  let loc = WPL.initializationLoc
  let msg = CS.GenericSimError shadowStackCorruptionMsg
  let err = CS.SimError loc msg
  CB.addDurableProofObligation bak (CB.LabeledPred (WI.falsePred sym) err)

-- | The error message used by shadow stack corruption proof obligations.
-- Exported so that the shadow stack heuristic can recognize these obligations.
shadowStackCorruptionMsg :: String
shadowStackCorruptionMsg = "Stack corruption detected: return address was modified"

-- | Extract the value of the last entry in a 'CS.RegMap' whose type matches
-- the given 'C.TypeRepr'.
extractRegs ::
  C.TypeRepr tp ->
  CS.RegMap sym args ->
  Maybe (CS.RegValue sym tp)
extractRegs ty (CS.RegMap assigns) =
  case Ctx.viewAssign assigns of
    Ctx.AssignExtend _base entry
      | Just Refl <- testEquality (CS.regType entry) ty
      -> Just (CS.regValue entry)
    _ -> Nothing

-- | Read the return address at call time using 'archFunctionReturnAddr'.
readRetAddrAtCall ::
  forall arch sym bak solver scope st fs w.
  ( CB.IsSymBackend sym bak
  , bak ~ CBO.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , Symbolic.SymArchConstraints arch
  , w ~ MC.ArchAddrWidth arch
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  ArchContext arch ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
  CLM.MemImpl sym ->
  IO (Maybe (MM.MemWord w))
readRetAddrAtCall bak archCtx regs mem =
  (archCtx ^. archFunctionReturnAddr) bak (archCtx ^. archVals) regs mem

-- | Read the instruction pointer from a register struct and concretize it.
-- Macaw sets IP to the return address as part of the return terminator's
-- effects, so at 'CS.ReturnState' this gives the (possibly corrupted) return
-- address on all architectures.
readIP ::
  forall arch sym w.
  ( CB.IsSymInterface sym
  , Symbolic.SymArchConstraints arch
  , w ~ MC.ArchAddrWidth arch
  ) =>
  ArchContext arch ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
  IO (Maybe (MM.MemWord w))
readIP archCtx regs = do
  ipVal <- (archCtx ^. archGetIP) regs
  case WI.asBV ipVal of
    Just bv -> pure (Just (fromIntegral (BV.asUnsigned bv)))
    Nothing -> pure Nothing
