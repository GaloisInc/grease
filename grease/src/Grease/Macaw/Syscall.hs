{-# LANGUAGE ExplicitNamespaces #-}

-- \| Functionality for converting 'Stubs.Syscall's into functions
-- that can be simulated within @macaw-symbolic@.

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Syscall (
  macawSyscallOverride,
) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (fmapFC)
import Grease.Macaw.Arch
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.Simulator qualified as C
import Stubs.Syscall qualified as Stubs
import What4.Expr qualified as W4
import What4.Protocol.Online qualified as W4

-- | Convert a 'Stubs.Syscall' to a 'MacawOverride'. Really, this functionality
-- ought to be exposed from @stubs-common@. See
-- <https://github.com/GaloisInc/stubs/issues/16>.
macawSyscallOverride ::
  forall sym bak p arch syscallArgs syscallRet regArgs regRets solver scope st fs.
  ( C.IsSymInterface sym
  , -- For silly reasons, `stubs` requires the use of an online SMT solver
    -- connection in order to call `syscallOverride`. See
    -- https://github.com/GaloisInc/stubs/issues/28.
    W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  ArchContext arch ->
  C.CtxRepr regArgs ->
  C.CtxRepr regRets ->
  Stubs.Syscall p sym syscallArgs (Symbolic.MacawExt arch) syscallRet ->
  C.Override p sym (Symbolic.MacawExt arch) regArgs (C.StructType regRets)
macawSyscallOverride bak archCtx regArgTps regRetTps syscallOv =
  C.mkOverride' (Stubs.syscallName syscallOv) (C.StructRepr regRetTps) ov
 where
  ov ::
    forall r'.
    C.OverrideSim
      p
      sym
      (Symbolic.MacawExt arch)
      r'
      regArgs
      (C.StructType regRets)
      (Ctx.Assignment (C.RegValue' sym) regRets)
  ov = do
    -- Construct the arguments
    argMap <- C.getOverrideArgs
    let argReg = massageRegAssignment $ C.regMap argMap
    args <-
      liftIO $
        (archCtx ^. archSyscallArgumentRegisters)
          bak
          regArgTps
          argReg
          (Stubs.syscallArgTypes syscallOv)

    -- Invoke the override and put the return value(s) into the appropriate
    -- register(s)
    (archCtx ^. archSyscallReturnRegisters)
      (Stubs.syscallReturnType syscallOv)
      (Stubs.syscallOverride syscallOv bak args)
      regArgTps
      argReg
      regRetTps

-- | Massage the 'C.RegEntry' 'Ctx.Assignment' that 'C.getOverrideArgs'
-- provides into the form that 'archSyscallArgumentRegisters' expects.
massageRegAssignment ::
  Ctx.Assignment (C.RegEntry sym) ctx ->
  C.RegEntry sym (C.StructType ctx)
massageRegAssignment assn =
  C.RegEntry
    (C.StructRepr (fmapFC C.regType assn))
    (fmapFC (C.RV . C.regValue) assn)
