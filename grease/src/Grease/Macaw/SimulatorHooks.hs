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

import Control.Monad.IO.Class (MonadIO)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Backend qualified as Symbolic
import Grease.Diagnostic (Diagnostic (SimulatorHooksDiagnostic), GreaseLogAction)
import Grease.Macaw.Arch (ArchContext)
import Grease.Macaw.Overrides.Address (AddressOverrides, maybeRunAddressOverride)
import Grease.Macaw.SimulatorHooks.Diagnostic qualified as Diag
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Simulator.Evaluation qualified as C
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import What4.Expr qualified as W4
import What4.Interface qualified as W4

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (SimulatorHooksDiagnostic diag)

-- | A 'C.ExtensionImpl' with overrides for the semantics of some macaw-symbolic
-- operations.
--
-- Wraps around a base ExtensionImpl that is invoked in cases where
-- greaseMacawExtImpl does not override the default behavior.
--
-- We override several pointer operations because their implementations
-- in "Data.Macaw.Symbolic.MemOps" invent fresh symbolic constants via
-- 'W4.freshConstant'. These fresh constants are unsuitable for use with
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
  C.GlobalVar CLM.Mem ->
  C.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  C.ExtensionImpl p sym (Symbolic.MacawExt arch)
greaseMacawExtImpl archCtx bak la tgtOvs memVar macawExtImpl =
  macawExtImpl
    { C.extensionEval = extensionEval macawExtImpl
    , C.extensionExec = extensionExec archCtx bak la tgtOvs memVar macawExtImpl
    }

-- | This evaluates a Macaw statement extension in the simulator.
extensionEval ::
  CB.IsSymBackend sym bak =>
  C.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  bak ->
  C.IntrinsicTypes sym ->
  (Int -> String -> IO ()) ->
  C.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
  C.EvalAppFunc sym (C.ExprExtension (Symbolic.MacawExt arch))
extensionEval baseExt bak iTypes logFn cst evalFn =
  \case
    Symbolic.PtrToBits _w ptr -> do
      CLM.LLVMPointer _blk off <- evalFn ptr
      pure off
    e -> defaultExec e
 where
  defaultExec = C.extensionEval baseExt bak iTypes logFn cst evalFn

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
  natZero <- W4.natLit sym 0
  xbv <- W4.natEq sym natZero xblk
  ybv <- W4.natEq sym natZero yblk
  sameBlk <- W4.natEq sym xblk yblk
  oneBv <- W4.orPred sym xbv ybv
  ok <- W4.orPred sym sameBlk oneBv
  let msg = "Invalid AND of two pointers"
  CB.assert bak ok (C.AssertFailureSimError msg "")
  xptr <- W4.notPred sym xbv
  blk <- W4.natIte sym xptr xblk yblk
  off <- W4.bvAndBits sym xoff yoff
  pure (CLM.LLVMPointer blk off)

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
  C.GlobalVar CLM.Mem ->
  C.ExtensionImpl p sym (Symbolic.MacawExt arch) ->
  Symbolic.MacawEvalStmtFunc (C.StmtExtension (Symbolic.MacawExt arch)) p sym (Symbolic.MacawExt arch)
extensionExec archCtx bak la tgtOvs memVar baseExt stmt crucState = do
  let sym = CB.backendGetSym bak
  case stmt of
    Symbolic.PtrAnd _w (C.RegEntry _ x) (C.RegEntry _ y) -> do
      p <- ptrAnd bak x y
      pure (p, crucState)
    Symbolic.PtrEq _w (C.RegEntry _ x) (C.RegEntry _ y) -> do
      p <- CLM.ptrEq sym (CLM.ptrWidth x) x y
      pure (p, crucState)
    Symbolic.PtrLeq _w (C.RegEntry _ x) (C.RegEntry _ y) -> do
      CLM.LLVMPointer _xblk xoff <- pure x
      CLM.LLVMPointer _yblk yoff <- pure y
      p <- W4.bvUle sym xoff yoff
      pure (p, crucState)
    Symbolic.PtrLt _w (C.RegEntry _ x) (C.RegEntry _ y) -> do
      CLM.LLVMPointer _xblk xoff <- pure x
      CLM.LLVMPointer _yblk yoff <- pure y
      p <- W4.bvUlt sym xoff yoff
      pure (p, crucState)
    Symbolic.PtrMux _w (C.RegEntry _ cond) (C.RegEntry _ x) (C.RegEntry _ y) -> do
      p <- CLM.muxLLVMPtr sym cond x y
      pure (p, crucState)
    Symbolic.MacawInstructionStart baddr iaddr dis ->
      -- Lifted from PATE...
      -- https://github.com/GaloisInc/pate/blob/198f84e64bf9b772c01e7972e99f26f1f03b68f2/src/Pate/Memory/MemTrace.hs#L918-L919
      case MM.incSegmentOff baddr (MM.memWordToUnsigned iaddr) of
        Just segOff -> do
          doLog la (Diag.ExecutingInstruction (PP.pretty segOff) dis)
          maybeRunAddressOverride archCtx memVar crucState segOff tgtOvs
          pure ((), crucState)
        Nothing ->
          panic
            "extensionExec"
            [ "MemorySegmentOff out of range"
            , show baddr
            , show iaddr
            ]
    _ -> defaultExec
 where
  defaultExec = C.extensionExec baseExt stmt crucState
