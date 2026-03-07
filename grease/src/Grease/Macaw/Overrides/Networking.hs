{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Overrides for network-related functions (Macaw backend).
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

{-
Note [The networking story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
GREASE has limited support for socket I/O. We define enough overrides (see
`networkOverrides`) to simulate basic socket server and client programs. These
overrides emulate socket connections by opening specific files on the symbolic
filesystem.

When a program invokes socket(domain, type, protocol), the following file will
be opened:

  /network/<domain_macro>/<type_macro>/socket

Where:

\* <domain_macro> is the name of the C macro that corresponds to the `domain`
  value passed to socket(). For example, a `domain` value of 2 corresponds to
  the AF_INET macro, so that would result in /network/AF_INET/...

\* <type_macro> is the name of the C macro that corresponds to the `type` value
  passed to socket(). For example, a `type` value of 1 corresponds to the
  SOCK_STREAM macro, so that would reult in /network/.../SOCK_STREAM/...

For servers, the contents of this .../socket file don't matter that much. For
clients, this file is used for network communication, so
`read`ing/`recv`ing from this file simulates receiving a message from the
server, and `send`ing/`write`ing to this file simulates sending a message to
the server.

When a program invokes accept(sockfd, addr, addrlen), a file will be opened at
a path depending on the socket domain:

\* If it is an AF_UNIX socket, the following file will be opened:

    <sun_path>/<seq_num>

  Where:

  * <sun_path> is the value in the `sun_path` field of the `sockaddr_un` struct
    passed to bind() when binding the socket.

  * <seq_num> indicates the order in which each call to accept() was made. For
    example, the first accept() call will be given a <seq_num> of 0, the second
    accept() call will be given a <seq_num> of 1, etc.

\* If it is an AF_INET or AF_INET6 socket, the  following file will be opened:

    /network/<domain_macro>/<type_macro>/<port>/<seq_num>

  Where:

  * <domain_macro> and <type_macro> are the same values as in the socket() case.
    <port> is the same value that was passed to bind() when assigning a name to
    the socket. (We'll describe how GREASE looks up these values in a bit.)

  * <seq_num> indicates the order in which each call to accept() was made.

These overrides use a crucible-symio LLVMFileSystem under the hood to track
these files. One limitation of an LLVMFileSystem is that there isn't a simple
way to associate extra metadata with each file. That is to say, after socket()
returns a file descriptor, there isn't an easy way to use that file descriptor
to look up its file path, port number, the number of connections etc. One might
be tempted to pass around a GlobalVar containing this metadata, but it's not
obvious what CrucibleType such a GlobalVar would have, since we would need some
kind of map-like structure.

Our solution to this problem is to add a `serverSocketFDs` field to the
`GreaseSimulatorState`, which is passed around across all overrides. This
contains a map of file descriptors to ServerSocketInfo, which is a collection
of metadata that gradually gets filled in after calls to socket() and bind().
This is not a perfect solution (more on this in a bit), but it is sufficient to
handle the kinds of network programs we are targeting.

Note that this metadata is only needed in service of figuring out the name of
the filepath that accept() opens, which is only needed for servers. Clients do
not need to invoke accept(), which is why there is not a corresponding
ClientSocketInfo data type.

There are a variety of limitations surrounding how this works to be aware of:

\* GreaseSimulatorState is passed around in the `personality` of each
  OverrideSim, which does not participate in symbolic branching and merging.
  One could imagine a program for which this is problematic (e.g., a socket
  that conditionally gets created depeneding on the value of symbolic data),
  but we do not anticipate this being an issue in practice.

\* Because the names of crucible-symio filepaths must be concrete, a socket's
  domain, type, and path (for AF_UNIX sockets) or port number (for AF_INET{6}
  sockets) must all be concrete, since they all appear in filepath names.
  Again, we do not anticipate many programs in the wild will try to make this
  information symbolic. The file descriptor for each socket must also be
  concrete, but since crucible-symio always allocates concrete file descriptors
  anyway, this requirement is easily satisfied.

\* The domain value passed to socket() must be listed in the `socketDomainMap`.
  We could, in theory, support other forms of communication, but we do not do so
  at present.

\* The type value passed to socket() must be listed in the `socketTypeMap`.
  We could, in theory, support other forms of communication semantics, but we
  do not do so at present.

\* Because crucible-symio's open() implementation does not support creating new
  files (see https://github.com/GaloisInc/crucible/issues/803), all of the
  files under the /network directory must be specified ahead of time in the
  initial symbolic filesystem.

\* The accept() override, unlike the syscall it models, completely ignores the
  addr and addrlen arguments. To model accept() more faithfully, it should fill
  in these arguments with information about the peer socket address.
-}

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
