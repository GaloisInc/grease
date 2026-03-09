{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Overrides for network-related functions (Macaw backend).
--
-- See @Note [The networking story]@ in "Grease.Overrides.Networking" for an
-- overview of how GREASE models network I/O.
module Grease.Macaw.Overrides.Networking (
  networkOverrides,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State qualified as State
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as MS
import Data.Parameterized.Context qualified as Ctx
import Data.Sequence qualified as Seq
import Grease.Macaw.Memory qualified as GMM
import Grease.Macaw.SimulatorState qualified as GMSS
import Grease.Overrides.Networking qualified as GON
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.Simulator qualified as CS
import Stubs.FunctionOverride qualified as StubsF
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

-- | All of the socket I/O–related overrides.
networkOverrides ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  Seq.Seq (StubsF.SomeFunctionOverride p sym arch)
networkOverrides fs memVar mmConf =
  Seq.fromList
    [ StubsF.SomeFunctionOverride (buildAcceptOverride fs)
    , StubsF.SomeFunctionOverride (buildBindOverride memVar mmConf)
    , StubsF.SomeFunctionOverride buildConnectOverride
    , StubsF.SomeFunctionOverride buildListenOverride
    , StubsF.SomeFunctionOverride (buildRecvOverride fs memVar)
    , StubsF.SomeFunctionOverride (buildSendOverride fs memVar)
    , StubsF.SomeFunctionOverride (buildSocketOverride fs)
    ]

buildAcceptOverride ::
  ( CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  CLSIO.LLVMFileSystem w ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildAcceptOverride fs =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "accept" $ \bak args ->
      Ctx.uncurryAssignment (GON.callAccept bak fs) args

buildBindOverride ::
  ( CLM.HasPtrWidth w
  , CLM.HasLLVMAnn sym
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildBindOverride memVar mmConf =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "bind" $ \bak args ->
      Ctx.uncurryAssignment (GON.callBind bak memVar (loadSockaddrUnPath bak memVar mmConf)) args

buildConnectOverride ::
  ( CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildConnectOverride =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "connect" $ \bak args ->
      Ctx.uncurryAssignment (GON.callConnect bak) args

buildListenOverride ::
  ( CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildListenOverride =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "listen" $ \bak args ->
      Ctx.uncurryAssignment (GON.callListen bak) args

buildRecvOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildRecvOverride fs memVar =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "recv" $ \bak args ->
      Ctx.uncurryAssignment (GON.callRecv bak fs memVar) args

buildSendOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildSendOverride fs memVar =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "send" $ \bak args ->
      Ctx.uncurryAssignment (GON.callSend bak fs memVar) args

buildSocketOverride ::
  ( CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  CLSIO.LLVMFileSystem w ->
  StubsF.FunctionOverride
    p
    sym
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
        Ctx.::> CLM.LLVMPointerType w
    )
    arch
    (CLM.LLVMPointerType w)
buildSocketOverride fs =
  WI.withKnownNat ?ptrWidth $
    StubsF.mkFunctionOverride "socket" $ \bak args ->
      Ctx.uncurryAssignment (GON.callSocket bak fs) args

-----
-- Macaw-specific helpers
-----

-- | This function digs through the memory in a pointer to a @sockaddr_un@
-- struct and loads the @sun_path@ out of it, which is the only information
-- that we care about.
loadSockaddrUnPath ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  CLM.LLVMPtr sym w ->
  CS.OverrideSim p sym (MS.MacawExt arch) r args ret BS.ByteString
loadSockaddrUnPath bak memVar mmConf sockaddrUnPtr = do
  let sym = CB.backendGetSym bak
  st <- State.get
  mem <- CS.readGlobal memVar
  let
    -- See the comments above @saFamilyTLenBytes@ in
    -- 'GON.loadSockaddrInPort' for why we use 2 here.
    saFamilyTLenBytes = BV.mkBV ?ptrWidth 2
  bvSaFamilyTLenBytes <- liftIO $ WI.bvLit sym ?ptrWidth saFamilyTLenBytes
  sockaddrUnPathPtr <- liftIO $ CLM.doPtrAddOffset bak mem sockaddrUnPtr bvSaFamilyTLenBytes
  let sockaddrUnPathReg = CS.RegEntry CLM.PtrRepr sockaddrUnPathPtr
  -- Note that the maximum size of @sun_path@ is 108 characters, which is why
  -- we pass @Just 108@ here.
  (unPathBytes, _) <-
    liftIO $
      GMM.loadConcreteString bak memVar mmConf sockaddrUnPathReg (Just 108) st
  pure unPathBytes
