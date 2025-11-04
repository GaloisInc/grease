{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.SimulatorHooks (
  greaseMacawExtImpl,
) where

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad.IO.Class (MonadIO)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Backend qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Grease.Diagnostic (Diagnostic (SimulatorHooksDiagnostic), GreaseLogAction)
import Grease.Macaw.Arch (ArchContext, archVals)
import Grease.Macaw.Overrides.Address (AddressOverrides, maybeRunAddressOverride)
import Grease.Macaw.SimulatorHooks.Diagnostic qualified as Diag
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.Evaluation qualified as C
import Lang.Crucible.Simulator.ExecutionTree qualified as CS
import Lang.Crucible.Simulator.GlobalState qualified as CS
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import What4.Expr qualified as W4
import What4.Interface qualified as WI

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (SimulatorHooksDiagnostic diag)

-- | A 'CS.ExtensionImpl' with overrides for the semantics of some macaw-symbolic
-- operations.
--
-- Wraps around a base ExtensionImpl that is invoked in cases where
-- greaseMacawExtImpl does not override the default behavior.
--
-- We override several pointer operations because their implementations
-- in "Data.Macaw.Symbolic.MemOps" invent fresh symbolic constants via
-- 'WI.freshConstant'. These fresh constants are unsuitable for use with
-- concretization, because they are unconstrained. Thus, when asked for a
-- model, the solver can choose arbitrary values for pointers that would result
-- in such constants being created, then choose arbitrary values for the
-- constants themselves.
--
-- We also override 'Symbolic.PtrToBits', because the default implementation
-- incurs a proof obligation stating that the pointer was indeed just a raw
-- bitvector (via 'Mem.projectLLVM_bv'). However, 'Grease.Setup.setupPtr'
-- currently uses symbolic block numbers, which causes this assertion to fail.
greaseMacawExtImpl ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  ) =>
  ArchContext arch ->
  bak ->
  GreaseLogAction ->
  AddressOverrides arch ->
  CS.GlobalVar CLM.Mem ->
  CS.GlobalVar (MC.ArchRegStruct arch) ->
  CS.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  CS.ExtensionImpl p sym (Symbolic.MacawExt arch)
greaseMacawExtImpl archCtx bak la tgtOvs memVar archStruct macawExtImpl =
  macawExtImpl
    { CS.extensionEval = extensionEval macawExtImpl
    , CS.extensionExec = extensionExec archCtx bak la tgtOvs memVar archStruct macawExtImpl
    }

-- | This evaluates a Macaw statement extension in the simulator.
extensionEval ::
  CB.IsSymBackend sym bak =>
  CS.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  bak ->
  CS.IntrinsicTypes sym ->
  (Int -> String -> IO ()) ->
  CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
  C.EvalAppFunc sym (C.ExprExtension (Symbolic.MacawExt arch))
extensionEval baseExt bak iTypes logFn cst evalFn =
  \case
    Symbolic.PtrToBits _w ptr -> do
      CLM.LLVMPointer _blk off <- evalFn ptr
      pure off
    e -> defaultExec e
 where
  defaultExec = CS.extensionEval baseExt bak iTypes logFn cst evalFn

-- | Perform and AND of two pointers.
--
-- In contrast to 'Data.Macaw.Symbolic.MemOps.doPtrAnd', this simply asserts
-- that one of the pointers is actually a bitvector, OR the pointers point to
-- the same allocation.
ptrAnd ::
  CB.IsSymBackend sym bak =>
  (1 C.<= w) =>
  bak ->
  CLM.LLVMPtr sym w ->
  CLM.LLVMPtr sym w ->
  IO (CLM.LLVMPtr sym w)
ptrAnd bak x y = do
  let sym = CB.backendGetSym bak
  CLM.LLVMPointer xblk xoff <- pure x
  CLM.LLVMPointer yblk yoff <- pure y
  natZero <- WI.natLit sym 0
  xbv <- WI.natEq sym natZero xblk
  ybv <- WI.natEq sym natZero yblk
  sameBlk <- WI.natEq sym xblk yblk
  oneBv <- WI.orPred sym xbv ybv
  ok <- WI.orPred sym sameBlk oneBv
  let msg = "Invalid AND of two pointers"
  CB.assert bak ok (CS.AssertFailureSimError msg "")
  xptr <- WI.notPred sym xbv
  blk <- WI.natIte sym xptr xblk yblk
  off <- WI.bvAndBits sym xoff yoff
  pure (CLM.LLVMPointer blk off)

updateStruct ::
  (C.OrdF (MC.ArchReg arch), sym ~ W4.ExprBuilder t st fs) =>
  Ctx.Assignment (MC.ArchReg arch) ctx ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.CtxToCrucibleType ctx) ->
  MapF.MapF (MC.ArchReg arch) (Symbolic.MacawCrucibleValue (CS.RegEntry (W4.ExprBuilder t st fs))) ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.CtxToCrucibleType ctx)
updateStruct Ctx.Empty Ctx.Empty _ =
  Ctx.Empty
updateStruct (rstRegs Ctx.:> currReg) (rstVals Ctx.:> currVal) regToNewVal =
  let newVal =
        case MapF.lookup currReg regToNewVal of
          Nothing -> currVal
          Just (Symbolic.MacawCrucibleValue (CS.RegEntry _ val)) -> CS.RV val
   in updateStruct rstRegs rstVals regToNewVal Ctx.:> newVal

extensionExec ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , sym ~ W4.ExprBuilder t st fs
  , Symbolic.SymArchConstraints arch
  , 16 C.<= MC.ArchAddrWidth arch
  ) =>
  ArchContext arch ->
  bak ->
  GreaseLogAction ->
  AddressOverrides arch ->
  CS.GlobalVar CLM.Mem ->
  CS.GlobalVar (MC.ArchRegStruct arch) ->
  CS.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  Symbolic.MacawEvalStmtFunc (C.StmtExtension (Symbolic.MacawExt arch)) p sym (Symbolic.MacawExt arch)
extensionExec archCtx bak la tgtOvs memVar archStruct baseExt stmt crucState = do
  let sym = CB.backendGetSym bak
  case stmt of
    Symbolic.PtrAnd _w (CS.RegEntry _ x) (CS.RegEntry _ y) -> do
      p <- ptrAnd bak x y
      pure (p, crucState)
    Symbolic.PtrEq _w (CS.RegEntry _ x) (CS.RegEntry _ y) -> do
      p <- CLM.ptrEq sym (CLM.ptrWidth x) x y
      pure (p, crucState)
    Symbolic.PtrLeq _w (CS.RegEntry _ x) (CS.RegEntry _ y) -> do
      CLM.LLVMPointer _xblk xoff <- pure x
      CLM.LLVMPointer _yblk yoff <- pure y
      p <- WI.bvUle sym xoff yoff
      pure (p, crucState)
    Symbolic.PtrLt _w (CS.RegEntry _ x) (CS.RegEntry _ y) -> do
      CLM.LLVMPointer _xblk xoff <- pure x
      CLM.LLVMPointer _yblk yoff <- pure y
      p <- WI.bvUlt sym xoff yoff
      pure (p, crucState)
    Symbolic.PtrMux _w (CS.RegEntry _ cond) (CS.RegEntry _ x) (CS.RegEntry _ y) -> do
      p <- CLM.muxLLVMPtr sym cond x y
      pure (p, crucState)
    Symbolic.MacawInstructionStart baddr iaddr dis ->
      -- Lifted from PATE...
      -- https://github.com/GaloisInc/pate/blob/198f84e64bf9b772c01e7972e99f26f1f03b68f2/src/Pate/Memory/MemTrace.hs#L918-L919
      case MM.incSegmentOff baddr (MM.memWordToUnsigned iaddr) of
        Just segOff -> do
          doLog la (Diag.ExecutingInstruction (PP.pretty segOff) dis)
          maybeRunAddressOverride archCtx memVar archStruct crucState segOff tgtOvs
          pure ((), crucState)
        Nothing ->
          panic
            "extensionExec"
            [ "MemorySegmentOff out of range"
            , show baddr
            , show iaddr
            ]
    Symbolic.MacawArchStateUpdate _ updates ->
      let nstate =
            crucState
              Lens.& CS.stateGlobals
                Lens.%~ ( \globs ->
                            let regs = CS.lookupGlobal archStruct globs
                                regAssign = Symbolic.crucGenRegAssignment $ Symbolic.archFunctions (archCtx ^. archVals)
                             in CS.insertGlobal
                                  archStruct
                                  ( case regs of
                                      Just rs -> updateStruct regAssign rs updates
                                      Nothing -> panic "extensionExec" ["The register struct global should be initialized during initState."]
                                  )
                                  globs
                        )
       in pure ((), nstate)
    _ -> defaultExec
 where
  defaultExec = CS.extensionExec baseExt stmt crucState
