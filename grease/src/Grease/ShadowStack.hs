{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- tolerates unbalanced call\/return events that arise when SSE (Static Symbolic
-- Execution) explores conditional function calls.
module Grease.ShadowStack (
  ShadowStack,
  newShadowStack,
  setInitialReturnAddr,
  shadowStackFeature,
  shadowStackCorruptionMsg,
) where

import Control.Exception.Safe qualified as X
import Control.Monad (void)
import Data.BitVector.Sized qualified as BV
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Classes (testEquality)
import Data.Parameterized.Context qualified as Ctx
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Equality ((:~:) (Refl))
import Grease.Macaw.Arch (ArchContext, archFunctionReturnAddr, archGetIP, archRegStructType, archVals)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.CallFrame qualified as CF
import Lang.Crucible.Simulator.EvalStmt qualified as CS (ExecutionFeature (..), ExecutionFeatureResult (..))
import Lang.Crucible.Simulator.ExecutionTree qualified as CS
import Lang.Crucible.Simulator.GlobalState qualified as CS (lookupGlobal)
import Lang.Crucible.Types qualified as C
import Lens.Micro ((^.))
import What4.Expr qualified as WE
import What4.FunctionName (FunctionName)
import What4.Interface qualified as WI
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
entryName = \case
  RealCall n _ -> n
  OverrideEntry n -> n

-- | Mutable shadow stack storing saved return addresses.
data ShadowStack w = ShadowStack
  { ssStack :: !(IORef [ShadowEntry w])
  -- ^ The live shadow stack.
  , ssInitial :: !(IORef [ShadowEntry w])
  -- ^ The entries that 'ssStack' is reset to at each 'CS.InitialState' (i.e.
  -- the start of each fresh simulation). This holds the entry function's saved
  -- return address; see 'setInitialReturnAddr'.
  }

-- | Create a new, empty shadow stack.
newShadowStack :: IO (ShadowStack w)
newShadowStack = ShadowStack <$> newIORef [] <*> newIORef []

-- | Reset the live shadow stack to its initial entries (see 'ssInitial'). Run
-- at each 'CS.InitialState' so that the shadow stack starts each simulation
-- seeded with only the entry function's return address.
resetToInitial :: ShadowStack w -> IO ()
resetToInitial ss = writeIORef (ssStack ss) =<< readIORef (ssInitial ss)

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
  -- | Absolute addresses of PLT stub jump targets (the dynamically-resolved
  -- functions reached by a @jmp \*GOT@). When a discovered PLT stub tail-calls
  -- one of these, Macaw models it as a return whose IP is the jump target
  -- rather than the saved return address, so checking it would produce a false
  -- positive. We skip the check when returning to such an address. See
  -- @Note [PLT stubs and the shadow stack]@.
  Set (MM.MemWord w) ->
  ShadowStack w ->
  IO
    ( CS.ExecutionFeature
        p
        sym
        (Symbolic.MacawExt arch)
        (CS.RegEntry sym (Symbolic.ArchRegStruct arch))
    )
shadowStackFeature bak archCtx memVar pltTargets ss = do
  let archRegStructTy = archCtx ^. archRegStructType
  pure $ CS.ExecutionFeature $ \case
    CS.InitialState{} -> do
      -- Each simulation begins at an 'CS.InitialState', so reset the live
      -- shadow stack to its seed (the entry function's return address) here.
      resetToInitial ss
      pure CS.ExecutionFeatureNoChange
    CS.CallState _rh resolvedCall simSt -> do
      handleCallState bak archCtx archRegStructTy memVar ss resolvedCall (simSt ^. CS.stateGlobals)
      pure CS.ExecutionFeatureNoChange
    CS.ReturnState retName _vfv retEntry _simSt -> do
      handleReturnState bak archCtx archRegStructTy pltTargets ss retName retEntry
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
      let mbCall = do
            -- Maybe
            Refl <- testEquality (CF.frameReturnType callFrame) archRegStructTy
            regs <- extractRegs archRegStructTy (CF._frameRegs callFrame)
            mem <- CS.lookupGlobal memVar globals
            C.SomeHandle h <- pure (CF.frameHandle callFrame)
            pure (C.handleName h, regs, mem)
       in case mbCall of
            Just (calleeName, regs, mem) -> do
              mbAddr <- tryReadReturnAddr bak archCtx regs mem
              modifyIORef' (ssStack ss) (RealCall calleeName mbAddr :)
            Nothing -> pure ()
    CS.OverrideCall _ov ovFrame ->
      let calleeName = ovFrame ^. CF.override
       in modifyIORef' (ssStack ss) (OverrideEntry calleeName :)

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
  Set (MM.MemWord w) ->
  ShadowStack w ->
  FunctionName ->
  CS.RegEntry sym ret ->
  IO ()
handleReturnState bak archCtx archRegStructTy pltTargets ss retName retEntry =
  case testEquality (CS.regType retEntry) archRegStructTy of
    Just Refl -> do
      mbMatch <- popMatchingEntry ss retName
      case mbMatch of
        Just (RealCall _name (Just saved)) -> do
          -- Macaw sets IP to the return address as part of the return
          -- terminator's effects, so at 'CS.ReturnState' this is the
          -- (possibly corrupted) return address on all architectures.
          ipVal <- (archCtx ^. archGetIP) (CS.regValue retEntry)
          -- A return whose IP is a PLT stub's jump target is a tail call into
          -- the resolved function, not a real return; skip it. See
          -- @Note [PLT stubs and the shadow stack]@.
          let isPltTarget =
                case WI.asBV ipVal of
                  Just bv -> Set.member (fromIntegral (BV.asUnsigned bv)) pltTargets
                  Nothing -> False
          if isPltTarget
            then pure ()
            else assertReturnAddr bak ipVal saved
        -- Either there is no saved address to compare against (the entry
        -- function's return address was unreadable; see 'setInitialReturnAddr'),
        -- the entry is an override, or the return matched no call. In all cases
        -- there is nothing to check.
        _ -> pure ()
    Nothing -> pure ()

-- | Search the shadow stack for the topmost entry whose callee name matches the
-- given function name. If one is found, pop it along with the entries above it
-- (which correspond to calls whose returns SSE skipped) and return it. If no
-- entry matches, leave the stack untouched and return 'Nothing'.
popMatchingEntry ::
  ShadowStack w ->
  FunctionName ->
  IO (Maybe (ShadowEntry w))
popMatchingEntry ss name = do
  stack <- readIORef (ssStack ss)
  case break ((== name) . entryName) stack of
    (_skipped, entry : rest) -> do
      writeIORef (ssStack ss) rest
      pure (Just entry)
    (_, []) -> pure Nothing

-- | Add a proof obligation asserting that the return-time instruction pointer
-- equals the saved return address. The solver disproves this exactly when the
-- return address may have been corrupted, including the case where corruption
-- redirects the return to a symbolic address (which a concrete comparison could
-- not detect).
assertReturnAddr ::
  forall sym bak solver scope st fs w.
  ( CB.IsSymBackend sym bak
  , bak ~ CBO.OnlineBackend solver scope st fs
  , sym ~ WE.ExprBuilder scope st fs
  , 1 C.<= w
  ) =>
  bak ->
  -- | The return-time instruction pointer.
  WI.SymBV sym w ->
  -- | The saved return address.
  MM.MemWord w ->
  IO ()
assertReturnAddr bak ipVal saved = do
  let sym = CB.backendGetSym bak
  let w = WI.bvWidth ipVal
  savedBV <- WI.bvLit sym w (BV.mkBV w (fromIntegral (MM.memWordValue saved)))
  matches <- WI.isEq sym ipVal savedBV
  loc <- WI.getCurrentProgramLoc sym
  let msg = CS.GenericSimError shadowStackCorruptionMsg
  let err = CS.SimError loc msg
  CB.addDurableProofObligation bak (CB.LabeledPred matches err)

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
      | Just Refl <- testEquality (CS.regType entry) ty ->
          Just (CS.regValue entry)
    _ -> Nothing

-- | Read the return address from the current register state using
-- 'archFunctionReturnAddr', returning 'Nothing' if it cannot be read.
--
-- 'archFunctionReturnAddr' loads from @[RSP]@ (or the architecture's
-- equivalent), which may not be readable: the entry function's saved return
-- address need not be backed by initialized memory, and a tail call can leave
-- the stack pointer somewhere unmapped. A failed 'CLM.doLoad' calls
-- 'CB.abortExecBecause', which throws 'CB.AbortExecReason'. We do not want such
-- a failure to kill the path, so we catch it and return 'Nothing'.
--
-- The catch alone is not enough, though: before throwing, the load already
-- registered a (failing) proof obligation as a side effect on the backend, and
-- catching the exception does not undo that 'Data.IORef.modifyIORef''. We
-- therefore run the read inside a fresh assumption frame and pop it with
-- 'CB.popAssumptionFrameAndObligations', which discards any obligations (and
-- assumptions) incurred by the speculative read. Note that
-- 'CB.saveAssumptionState' is /not/ suitable here: restoring it deliberately
-- preserves accumulated obligations.
tryReadReturnAddr ::
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
tryReadReturnAddr bak archCtx regs mem = do
  frm <- CB.pushAssumptionFrame bak
  let read_ = (archCtx ^. archFunctionReturnAddr) bak (archCtx ^. archVals) regs mem
  -- Pop the frame (discarding the speculative read's assumptions and
  -- obligations) regardless of whether the read succeeded, was aborted, or
  -- raised some other exception, to keep pushes and pops well-bracketed.
  X.finally
    ( X.try read_ >>= \case
        Left (_ :: CB.AbortExecReason) -> pure Nothing
        Right mbAddr -> pure mbAddr
    )
    (void (CB.popAssumptionFrameAndObligations bak frm))

-- | Read the entry function's return address with 'tryReadReturnAddr' and
-- record it as the shadow stack's initial entry (see 'ssInitial'). The feature
-- resets the live stack to this entry at each 'CS.InitialState', so this seeds
-- the entrypoint's return address, for which there is no preceding
-- 'CS.CallState' to record it.
setInitialReturnAddr ::
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
  ShadowStack w ->
  FunctionName ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
  CLM.MemImpl sym ->
  IO ()
setInitialReturnAddr bak archCtx ss name regs mem = do
  mbAddr <- tryReadReturnAddr bak archCtx regs mem
  writeIORef (ssInitial ss) [RealCall name mbAddr]

{-
Note [PLT stubs and the shadow stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A PLT stub is a short trampoline that jumps to a dynamically-resolved function
(e.g. @malloc\@plt@ jumps to the real @malloc@). When grease discovers and
simulates a PLT stub as an ordinary function (see @Note [Mark PLT stubs as
trusted function entry points]@ in "Grease.Macaw.Discovery"), Macaw lifts the
stub's terminal @jmp \*GOT@ as a return whose instruction pointer is the jump
\*target* (the resolved callee's entry point), not the return address saved by
the original call.

The shadow stack detects corruption by comparing the saved return address to
the return-time IP, so a PLT stub would always appear corrupted: the IP is the
jump target rather than the saved return address. To avoid this false positive,
'handleReturnState' skips the check whenever the return-time IP is one of the
known PLT stub jump targets (the dynamically-resolved function addresses). These
targets are computed in "Grease.Main" from the binary's PLT stub map.

This only affects architectures where PLT stubs are discovered and simulated
(e.g. x86-64). On architectures where PLT stubs are handled as overrides (e.g.
AArch32, PPC32), they already flow through the 'CS.OverrideCall' path and are
recorded as 'OverrideEntry'.
-}
