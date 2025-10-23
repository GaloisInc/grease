{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- TODO(#438): Remove calls to `error`
{- HLINT ignore "Use panic" -}

-- | Overrides for network-related functions.
module Grease.Macaw.Overrides.Networking (
  networkOverrides,
) where

import Control.Lens (use, (%=))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State qualified as State
import Data.BitVector.Sized as BV
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as MS
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context as Ctx
import Data.Parameterized.Some (Some (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Word (Word16)
import GHC.Stack (HasCallStack, callStack)
import Grease.Macaw.Memory qualified as GMM
import Grease.Macaw.SimulatorState qualified as GMSS
import Grease.Macaw.SimulatorState.Networking qualified as GMSSN
import Grease.Panic (panic)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.DataLayout qualified as CLD
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSymIo
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.SymIO qualified as CSymIo
import Lang.Crucible.Types qualified as CT
import Prettyprinter qualified as PP
import Stubs.FunctionOverride qualified as StubsF
import Stubs.Override qualified as StubsO
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WP
import What4.Protocol.Online qualified as WPO
import What4.Utils.ResolveBounds.BV qualified as WURB

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
  CLSymIo.LLVMFileSystem w ->
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
  CLSymIo.LLVMFileSystem w ->
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
      Ctx.uncurryAssignment (callAccept bak fs) args

-- | Override for the @accept(2)@ function. This function looks up the metadata
-- associated with the socket file descriptor argument, allocates a new socket
-- file with a unique name, and records this information in the
-- 'GMSS.GreaseSimulatorState'. See Note @[The networking story]@ for the full
-- details.
callAccept ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  CLSymIo.LLVMFileSystem w ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callAccept bak fs sockfd _addr _addrlen = do
  sockfdInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "accept" FdArgument $
        CS.regValue sockfd
  serverSocketFDs <- use (CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds)
  fd <-
    case Map.lookup sockfdInt serverSocketFDs of
      Just (Some ssi@(GMSSN.ServerSocketInfo{GMSSN.serverSocketAddress = Just sockAddr})) -> do
        let connectionFilePath :: FilePath
            connectionFilePath =
              case GMSSN.serverSocketDomain ssi of
                GMSSN.AfUnixRepr -> GMSSN.acceptAfUnixFilePath sockAddr ssi
                GMSSN.AfInetRepr -> GMSSN.acceptAfInetFilePath sockAddr ssi
                GMSSN.AfInet6Repr -> GMSSN.acceptAfInetFilePath sockAddr ssi
        connectionFileLit <- liftIO $ WI.stringLit sym $ WI.Char8Literal $ BSC.pack connectionFilePath
        CSymIo.openFile (CLSymIo.llvmFileSystem fs) connectionFileLit $ \res -> do
          case res of
            Left CSymIo.FileNotFound -> returnIOError
            Right fileHandle -> do
              let sockNextConn = GMSSN.serverSocketNextConnection ssi
              CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds
                %= Map.insert sockfdInt (Some (ssi{GMSSN.serverSocketNextConnection = sockNextConn + 1}))
              CLSymIo.allocateFileDescriptor fs fileHandle
      _ -> returnIOError
  fdPtr <- liftIO $ CLM.llvmPointer_bv sym fd
  liftIO $ StubsO.adjustPointerSize sym fdPtr ?ptrWidth
 where
  sym = CB.backendGetSym bak

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
      Ctx.uncurryAssignment (callBind bak memVar mmConf) args

-- | Override for the @bind(2)@ syscall. This function reads the port number from
-- the @addr@ struct, ensures that it is concrete, and records it for later
-- calls to @accept()@. See Note @[The networking story]@ for the full details.
callBind ::
  forall sym bak arch w p solver scope st fs ext r args ret cExt.
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , MC.MemWidth w
  , w ~ MC.ArchAddrWidth arch
  , ext ~ MS.MacawExt arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  MS.MemModelConfig p sym arch CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callBind bak memVar mmConf sockfd addr _addrlen = do
  sockfdInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "bind" FdArgument $
        CS.regValue sockfd
  serverSocketFds <- use (CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds)
  ec <-
    case Map.lookup sockfdInt serverSocketFds of
      Just (Some ssi) -> do
        () <- case GMSSN.serverSocketDomain ssi of
          GMSSN.AfUnixRepr -> bindUnix sockfdInt ssi
          GMSSN.AfInetRepr -> bindInet sockfdInt ssi
          GMSSN.AfInet6Repr -> bindInet sockfdInt ssi
        returnIOSuccess
      Nothing ->
        returnIOError
  ecPtr <- liftIO $ CLM.llvmPointer_bv sym ec
  liftIO $ StubsO.adjustPointerSize sym ecPtr ?ptrWidth
 where
  sym = CB.backendGetSym bak

  -- For AF_UNIX sockets, we bind the socket to a path name.
  bindUnix ::
    Integer ->
    GMSSN.ServerSocketInfo 'GMSSN.AfUnix ->
    CS.OverrideSim p sym ext r args ret ()
  bindUnix sockfdInt ssi = do
    portPath <- loadSockaddrUnPath bak memVar mmConf $ CS.regValue addr
    CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds
      %= Map.insert sockfdInt (Some (ssi{GMSSN.serverSocketAddress = Just portPath}))

  -- For AF_INET(6) sockets, we bind the socket to a port number.
  bindInet ::
    forall domain.
    (GMSSN.SocketAddress domain ~ Word16) =>
    Integer ->
    GMSSN.ServerSocketInfo domain ->
    CS.OverrideSim p sym ext r args ret ()
  bindInet sockfdInt ssi = do
    mem <- CS.readGlobal memVar
    portBV <- liftIO $ loadSockaddrInPort bak mem $ CS.regValue addr
    portInt <-
      liftIO $
        fmap BV.asUnsigned $
          networkConstantBv bak "bind" PortArgument (WI.knownNat @16) portBV
    CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds
      %= Map.insert sockfdInt (Some (ssi{GMSSN.serverSocketAddress = Just (fromInteger portInt)}))

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
      Ctx.uncurryAssignment (callConnect bak) args

-- | Override for the @connect(2)@ function. This implementation is very simple,
-- as it only checks to see if the socket file descriptor argument has
-- previously been registered. If it has, it will return 0, indicating success.
-- If not, it will return -1, indicating failure.
--
-- Like @bind@, the @connect@ function also takes an @addr@ argument. However,
-- we never need to examine what port it uses, since this override always
-- assumes that the connection was successfully initiated (FD registration
-- notwithstanding).
callConnect ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callConnect bak sockfd _addr _addrlen = checkSocketFdInUse bak "connect" sockfd

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
      Ctx.uncurryAssignment (callListen bak) args

-- | Override for the @listen(2)@ function. This implementation is very simple,
-- as it only checks to see if the socket file descriptor argument has
-- previously been registered. If it has, it will return 0, indicating success.
-- If not, it will return -1, indicating failure.
callListen ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callListen bak sockfd _backlog = checkSocketFdInUse bak "listen" sockfd

buildRecvOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  ) =>
  CLSymIo.LLVMFileSystem w ->
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
      Ctx.uncurryAssignment (callRecv bak fs memVar) args

-- | Override for the @recv(2)@ function. For now, we treat it identically to
-- @read@, ignoring the @flags@ argument entirely.
callRecv ::
  ( CLM.HasLLVMAnn sym
  , CB.IsSymBackend sym bak
  , CLM.HasPtrWidth w
  ) =>
  bak ->
  CLSymIo.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callRecv bak fs memVar sockfd buf len _flags = do
  let sym = CB.backendGetSym bak
  -- Drop upper 32 bits from `sockfd` to create a 32 bit file descriptor
  let w32 = knownNat @32
  sockfd32 <- liftIO $ StubsO.adjustPointerSize sym (CS.regValue sockfd) w32
  let sockfdPtrErr =
        CS.AssertFailureSimError
          "Found pointer instead of bitvector in socket file descriptor passed to 'recv'"
          ""
  sockfd32Bv <- liftIO $ CLM.ptrToBv bak sockfdPtrErr sockfd32
  let sockfd32Reg = CS.RegEntry (CT.BVRepr w32) sockfd32Bv

  -- Convert `len` to a bitvector
  let lenPtrErr =
        CS.AssertFailureSimError
          "Found pointer instead of bitvector in length passed to 'recv'"
          ""
  lenBv <- liftIO $ CLM.ptrToBv bak lenPtrErr $ CS.regValue len
  let lenReg = CS.RegEntry (CT.BVRepr ?ptrWidth) lenBv

  -- Use crucible-llvm override for `read`
  resBv <- CLSymIo.callReadFileHandle memVar fs sockfd32Reg buf lenReg

  liftIO $ CLM.llvmPointer_bv sym resBv

buildSendOverride ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  CLSymIo.LLVMFileSystem w ->
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
      Ctx.uncurryAssignment (callSend bak fs memVar) args

-- | Override for the @send(2)@ function. For now, we treat it identically to
-- @write@, ignoring the @flags@ argument entirely.
callSend ::
  ( CLM.HasLLVMAnn sym
  , CB.IsSymBackend sym bak
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  CLSymIo.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callSend bak fs memVar sockfd buf len _flags = do
  let sym = CB.backendGetSym bak
  -- Drop upper 32 bits from `sockfd` to create a 32 bit file descriptor
  let w32 = knownNat @32
  sockfd32 <- liftIO $ StubsO.adjustPointerSize sym (CS.regValue sockfd) w32
  let sockfdPtrErr =
        CS.AssertFailureSimError
          "Found pointer instead of bitvector in socket file descriptor passed to 'send'"
          ""
  sockfd32Bv <- liftIO $ CLM.ptrToBv bak sockfdPtrErr sockfd32
  let sockfd32Reg = CS.RegEntry (CT.BVRepr w32) sockfd32Bv

  -- Convert `len` to a bitvector
  let lenPtrErr =
        CS.AssertFailureSimError
          "Found pointer instead of bitvector in length passed to 'send'"
          ""
  lenBv <- liftIO $ CLM.ptrToBv bak lenPtrErr $ CS.regValue len
  let lenReg = CS.RegEntry (CT.BVRepr ?ptrWidth) lenBv

  -- Use the crucible-llvm override for `write`
  resBv <- CLSymIo.callWriteFileHandle memVar fs sockfd32Reg buf lenReg

  liftIO $ CLM.llvmPointer_bv sym resBv

buildSocketOverride ::
  ( CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  CLSymIo.LLVMFileSystem w ->
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
      Ctx.uncurryAssignment (callSocket bak fs) args

-- | Override for the @socket(2)@ function. This checks to see if the
-- appropriate arguments are concrete, and if so, open a file with the
-- appropriate name representing the socket. See Note @[The networking story]@
-- for the full details.
callSocket ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  CLSymIo.LLVMFileSystem w ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callSocket bak fs domainReg typeReg _protocol = do
  let sym = CB.backendGetSym bak
  domainInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "socket" DomainArgument $
        CS.regValue domainReg
  domain <-
    case Map.lookup domainInt socketDomainMap of
      Just d -> pure d
      Nothing -> liftIO $ do
        loc <- WI.getCurrentProgramLoc sym
        unsupportedSocketArgument loc DomainArgument domainInt
  Some domainRepr <- pure $ GMSSN.toSocketDomainRepr domain
  typeInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "socket" TypeArgument $
        CS.regValue typeReg
  typ <-
    case Map.lookup typeInt socketTypeMap of
      Just t -> pure t
      Nothing -> liftIO $ do
        loc <- WI.getCurrentProgramLoc sym
        unsupportedSocketArgument loc TypeArgument typeInt
  let ssi =
        GMSSN.ServerSocketInfo
          { GMSSN.serverSocketDomain = domainRepr
          , GMSSN.serverSocketType = typ
          , GMSSN.serverSocketAddress = Nothing
          , GMSSN.serverSocketNextConnection = 0
          }
  socketFileLit <-
    liftIO $
      WI.stringLit sym $
        WI.Char8Literal $
          BSC.pack $
            GMSSN.socketFilePath ssi
  fd <- CSymIo.openFile (CLSymIo.llvmFileSystem fs) socketFileLit $ \res -> do
    case res of
      Left CSymIo.FileNotFound -> returnIOError
      Right fileHandle -> do
        fd <- CLSymIo.allocateFileDescriptor fs fileHandle
        fdBV <- case WI.asBV fd of
          Just fdBV -> pure fdBV
          Nothing ->
            panic
              "callSocket"
              ["allocateFileDescriptor should return a concrete FD"]
        CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds
          %= Map.insert (BV.asUnsigned fdBV) (Some ssi)
        pure fd
  fdPtr <- liftIO $ CLM.llvmPointer_bv sym fd
  liftIO $ StubsO.adjustPointerSize sym fdPtr ?ptrWidth

-----
-- Exceptions
-----

networkConcretizationFailedSymbolic ::
  HasCallStack =>
  WP.ProgramLoc ->
  -- | The function being invoked.
  Text.Text ->
  -- | The argument to the function for which concretization was attempted.
  NetworkFunctionArgument ->
  IO a
networkConcretizationFailedSymbolic loc nm arg = do
  let msg =
        "Attempted to make a call to the"
          PP.<+> PP.squotes (PP.pretty nm)
          PP.<+> "function with non-concrete"
          PP.<+> networkFunctionArgumentDescription arg
  let reason = CS.Unsupported callStack (show msg)
  let simErr = CS.SimError loc reason
  CB.abortExecBecause (CB.AssertionFailure simErr)

unsupportedSocketArgument ::
  HasCallStack =>
  WP.ProgramLoc ->
  -- | The type of argument to the @socket@ function.
  NetworkFunctionArgument ->
  -- | The unsupported argument value.
  Integer ->
  IO a
unsupportedSocketArgument loc arg value = do
  let msg =
        "Attempted to call the 'socket' function with an unsupported"
          PP.<+> networkFunctionArgumentDescription arg
          <> PP.colon
          PP.<+> PP.viaShow value
  let reason = CS.Unsupported callStack (show msg)
  let simErr = CS.SimError loc reason
  CB.abortExecBecause (CB.AssertionFailure simErr)

networkFunctionArgumentDescription :: NetworkFunctionArgument -> PP.Doc a
networkFunctionArgumentDescription =
  \case
    FdArgument -> "file descriptor argument"
    DomainArgument -> "domain argument"
    TypeArgument -> "type argument"
    PortArgument -> "port argument"

-- | Which argument to a networking-related override did a solver try to
-- resolve as concrete?
data NetworkFunctionArgument
  = FdArgument
  | DomainArgument
  | TypeArgument
  | PortArgument

-----
-- Helpers, not exported
-----

-- | Check if the socket file descriptor argument has previously been
-- registered. If so, return 0, indicating success. If not, return -1,
-- indicating failure.
--
-- This function is the workhorse for the @connect@ and @listen@ overrides.
checkSocketFdInUse ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , w ~ MC.ArchAddrWidth arch
  , GMSS.HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  Text.Text ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
checkSocketFdInUse bak fnName sockfd = do
  let sym = CB.backendGetSym bak
  sockfdInt <- liftIO $ networkConstantBvPtrToInteger bak fnName FdArgument $ CS.regValue sockfd
  serverSocketFDs <- use (CS.stateContext . CS.cruciblePersonality . GMSS.greaseSimulatorState . GMSS.serverSocketFds)
  ec <-
    if Map.member sockfdInt serverSocketFDs
      then returnIOSuccess
      else returnIOError
  ecPtr <- liftIO $ CLM.llvmPointer_bv sym ec
  liftIO $ StubsO.adjustPointerSize sym ecPtr ?ptrWidth

-- | This function digs through the memory in a pointer to a @sockaddr_in@
-- struct (for @AF_INET@ connections) or a @sockaddr_in6@ struct (for @AF_INET6@
-- connections) and loads the port number out of it, which is the only
-- information that we care about. Note that the port number's bytes will be
-- represented in network order, not host order, so you may want to byteswap it
-- for debugging purposes.
loadSockaddrInPort ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  CLM.MemImpl sym ->
  CLM.LLVMPtr sym w ->
  IO (WI.SymBV sym 16)
loadSockaddrInPort bak mem sockaddrInPtr = do
  let sym = CB.backendGetSym bak
  let
    -- The size of the @sa_family_t@ type in bytes. While
    -- https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_socket.h.html
    -- requires @sa_family_t@ to be an unsigned integer, it does not specify
    -- what size it should have. As a result, the size could potentially be
    -- different across different OS/architecture configurations.
    --
    -- @sa_family_t@ is two bytes in x86-64 Linux, so we have opted to
    -- hard-code this information for the time being.
    saFamilyTLenBytes = BV.mkBV ?ptrWidth 2
  bvSaFamilyTLenBytes <- WI.bvLit sym ?ptrWidth saFamilyTLenBytes
  -- At what offset into the struct should we load to get the port number?
  -- Here are the definitions of @sockaddr_in@ and @sockaddr_in6@,
  -- respectively, for reference:
  --
  -- @
  -- struct sockaddr_in {
  --     sa_family_t    sin_family;
  --     in_port_t      sin_port;
  --     struct in_addr sin_addr;
  -- };
  --
  -- struct sockaddr_in6 {
  --     sa_family_t     sin6_family;
  --     in_port_t       sin6_port;
  --     uint32_t        sin6_flowinfo;
  --     struct in6_addr sin6_addr;
  --     uint32_t        sin6_scope_id;
  -- };
  -- @
  --
  -- Conveniently, the field of type @in_port_t@ is always the second field,
  -- and it always follows a field of type @sa_family_t@. Therefore, we use the
  -- size of @sa_family_t@ to compute the offset.
  sockaddrInPortPtr <- CLM.doPtrAddOffset bak mem sockaddrInPtr bvSaFamilyTLenBytes
  let
    -- The size of the @in_port_t@ type in bits. The size of this type
    -- appears to be standardized across all implementations. See, e.g.,.
    -- https://pubs.opengroup.org/onlinepubs/009695399/basedefs/netinet/in.h.html,
    -- which says that @in_port_t@ should always be equivalent to @uint16_t@.
    inPortTLenBits = WI.knownNat @16
    inPortTLenBytes = CLB.bitsToBytes $ WI.intValue $ inPortTLenBits
  v <-
    CLM.doLoad
      bak
      mem
      sockaddrInPortPtr
      (CLM.bitvectorType inPortTLenBytes)
      (CLM.LLVMPointerRepr inPortTLenBits)
      CLD.noAlignment
  let err =
        CS.AssertFailureSimError
          "Found pointer instead of bitvector in 'sockaddr_in' port number loaded in 'bind'"
          ""
  CLM.ptrToBv bak err v

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
    -- See the comments above @saFamilyTLenBytes@ in 'loadSockaddrInPort'
    -- for why we use 2 here.
    saFamilyTLenBytes = BV.mkBV ?ptrWidth 2
  bvSaFamilyTLenBytes <- liftIO $ WI.bvLit sym ?ptrWidth saFamilyTLenBytes
  -- Here is the definition of @sockaddr_un@:
  --
  -- @
  -- struct sockaddr_un {
  --     sa_family_t sun_family;               /* AF_UNIX */
  --     char        sun_path[108];            /* Pathname */
  -- };
  -- @
  --
  -- Just as with @sockaddr_in@, the information we care about is the second
  -- field, which follows a field of type @sa_family_t@. Therefore, we use the
  -- size of @sa_family_t@ to compute the offset.
  sockaddrUnPathPtr <- liftIO $ CLM.doPtrAddOffset bak mem sockaddrUnPtr bvSaFamilyTLenBytes
  let sockaddrUnPathReg = CS.RegEntry CLM.PtrRepr sockaddrUnPathPtr
  -- Note that the maximum size of @sun_path@ is 108 characters, which is why
  -- we pass @Just 108@ here.
  (unPathBytes, _) <-
    liftIO $
      GMM.loadConcreteString bak memVar mmConf sockaddrUnPathReg (Just 108) st
  pure unPathBytes

-- | Concretize a symbolic bitvector representing the argument to a
-- networking-related function override. If the bitvector is truly symbolic,
-- throw an exception.
networkConstantBv ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , 1 WI.<= w
  ) =>
  bak ->
  -- | The name of the function being overriden. Only used for exception message
  -- purposes.
  Text.Text ->
  -- | The sort of argument the 'WI.SymBV' represents. Only used for exception
  -- message purposes.
  NetworkFunctionArgument ->
  -- | The width of the bitvector.
  WI.NatRepr w ->
  -- | The symbolic bitvector.
  WI.SymBV sym w ->
  IO (BV.BV w)
networkConstantBv bak fnName fnArg w symBV =
  CBO.withSolverProcess bak onlinePanic $ \sp -> do
    let sym = CB.backendGetSym bak
    resBV <- WURB.resolveSymBV sym WURB.ExponentialSearch w sp symBV
    case resBV of
      WURB.BVConcrete bv ->
        pure bv
      WURB.BVSymbolic{} -> do
        loc <- WI.getCurrentProgramLoc sym
        networkConcretizationFailedSymbolic loc fnName fnArg
 where
  onlinePanic =
    error "networkConstantBv: Online solver support is not enabled"

-- | Like 'networkConstantBv', but where:
--
-- * The argument is an 'CLM.LLVMPtr' instead of a 'WI.SymBV'.
--
-- * The returned value is converted directly to an 'Integer'.
networkConstantBvPtrToInteger ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , 1 WI.<= w
  ) =>
  bak ->
  Text.Text ->
  NetworkFunctionArgument ->
  CLM.LLVMPtr sym w ->
  IO Integer
networkConstantBvPtrToInteger bak fnName fnArg ptr = do
  let err =
        CS.AssertFailureSimError
          ( "Passed a pointer to '"
              ++ Text.unpack fnName
              ++ "' where a bitvector was expected"
          )
          ""
  ptrBV <- CLM.ptrToBv bak err ptr
  bv <- networkConstantBv bak fnName fnArg (CLM.ptrWidth ptr) ptrBV
  pure $ BV.asUnsigned bv

-- | Return -1, indiciating failure.
returnIOError ::
  CB.IsSymInterface sym =>
  CS.OverrideSim p sym ext r args ret (WI.SymBV sym 32)
returnIOError = do
  sym <- CS.getSymInterface
  liftIO $ WI.bvLit sym (WI.knownNat @32) (BV.mkBV (WI.knownNat @32) (-1))

-- | Return 0, indicating success.
returnIOSuccess ::
  CB.IsSymInterface sym =>
  CS.OverrideSim p sym ext r args ret (WI.SymBV sym 32)
returnIOSuccess = do
  sym <- CS.getSymInterface
  liftIO $ WI.bvLit sym (WI.knownNat @32) (BV.zero (WI.knownNat @32))

-- | A map of supported socket domains. The keys are the values of the
-- corresponding C macros, with each key mapping to the name of the macro.
--
-- Note that there is no guarantee that the values of these macros will be the
-- same in every OS/architecture configuration. At the moment, we hard-code the
-- values that arise in x86-64 Linux.
socketDomainMap :: Map.Map Integer GMSSN.SocketDomain
socketDomainMap =
  Map.fromList
    [ (1, GMSSN.AfUnix)
    , (2, GMSSN.AfInet)
    , (10, GMSSN.AfInet6)
    ]

-- | A map of supported socket types. The keys are the values of the
-- corresponding C macros, with each key mapping to the name of the macro.
--
-- Note that there is no guarantee that the values of these macros will be the
-- same in every OS/architecture configuration. At the moment, we hard-code the
-- values that arise in x86-64 Linux.
socketTypeMap :: Map.Map Integer GMSSN.SocketType
socketTypeMap =
  Map.fromList
    [ (1, GMSSN.SockStream)
    , (2, GMSSN.SockDgram)
    , (5, GMSSN.SockSeqpacket)
    ]
