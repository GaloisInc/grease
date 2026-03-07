{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- TODO(#438): Remove calls to `error`
{- HLINT ignore "Use panic" -}

-- | Shared networking override logic, used by both Macaw and LLVM backends.
module Grease.Overrides.Networking (
  -- * Core call implementations
  callSocket,
  callAccept,
  callBind,
  callConnect,
  callListen,
  callRecv,
  callSend,

  -- * Helpers
  loadSockaddrInPort,
  networkConstantBv,
  networkConstantBvPtrToInteger,
  checkSocketFdInUse,
  returnIOError,
  returnIOSuccess,
  socketDomainMap,
  socketTypeMap,
  NetworkFunctionArgument (..),
) where

import Control.Lens (use, (%=))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Parameterized.Some (Some (Some))
import Data.Text qualified as Text
import Data.Word (Word16)
import GHC.Stack (HasCallStack, callStack)
import Grease.Panic (panic)
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.DataLayout qualified as CLD
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.SymIO qualified as CSymIo
import Lang.Crucible.Types qualified as CT
import Prettyprinter qualified as PP
import Stubs.Override qualified as StubsO
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WPL
import What4.Protocol.Online qualified as WPO
import What4.Utils.ResolveBounds.BV qualified as WURB

-- | Override for the @accept(2)@ function. This function looks up the metadata
-- associated with the socket file descriptor argument, allocates a new socket
-- file with a unique name, and records this information in the personality
-- state.
callAccept ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem w ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callAccept bak fs sockfd _addr _addrlen = do
  sockfdInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "accept" FdArgument $
        CS.regValue sockfd
  serverSocketFDs <- use (CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL)
  fd <-
    case Map.lookup sockfdInt serverSocketFDs of
      Just (Some ssi@(GSN.ServerSocketInfo{GSN.serverSocketAddress = Just sockAddr})) -> do
        let connectionFilePath :: FilePath
            connectionFilePath =
              case GSN.serverSocketDomain ssi of
                GSN.AfUnixRepr -> GSN.acceptAfUnixFilePath sockAddr ssi
                GSN.AfInetRepr -> GSN.acceptAfInetFilePath sockAddr ssi
                GSN.AfInet6Repr -> GSN.acceptAfInetFilePath sockAddr ssi
        connectionFileLit <- liftIO $ WI.stringLit sym $ WI.Char8Literal $ BSC.pack connectionFilePath
        CSymIo.openFile (CLSIO.llvmFileSystem fs) connectionFileLit $ \res -> do
          case res of
            Left CSymIo.FileNotFound -> returnIOError
            Right fileHandle -> do
              let sockNextConn = GSN.serverSocketNextConnection ssi
              CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL
                %= Map.insert sockfdInt (Some (ssi{GSN.serverSocketNextConnection = sockNextConn + 1}))
              CLSIO.allocateFileDescriptor fs fileHandle
      _ -> returnIOError
  fdPtr <- liftIO $ CLM.llvmPointer_bv sym fd
  liftIO $ StubsO.adjustPointerSize sym fdPtr ?ptrWidth
 where
  sym = CB.backendGetSym bak

-- | Type alias for the callback that loads a Unix socket path from memory.
-- Different backends (Macaw, LLVM) implement this differently.
type LoadUnixPath sym ext p w r args ret =
  CS.OverrideSim p sym ext r args ret BS.ByteString

-- | Override for the @bind(2)@ syscall. This function reads the port number from
-- the @addr@ struct, ensures that it is concrete, and records it for later
-- calls to @accept()@.
callBind ::
  forall sym bak w p solver scope st fs ext r args ret.
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  -- | Callback to load the @sun_path@ from a @sockaddr_un@ struct
  (CLM.LLVMPtr sym w -> LoadUnixPath sym ext p w r args ret) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callBind bak memVar loadUnixPath sockfd addr _addrlen = do
  sockfdInt <-
    liftIO $
      networkConstantBvPtrToInteger bak "bind" FdArgument $
        CS.regValue sockfd
  serverSocketFds <- use (CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL)
  ec <-
    case Map.lookup sockfdInt serverSocketFds of
      Just (Some ssi) -> do
        () <- case GSN.serverSocketDomain ssi of
          GSN.AfUnixRepr -> bindUnix sockfdInt ssi
          GSN.AfInetRepr -> bindInet sockfdInt ssi
          GSN.AfInet6Repr -> bindInet sockfdInt ssi
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
    GSN.ServerSocketInfo 'GSN.AfUnix ->
    CS.OverrideSim p sym ext r args ret ()
  bindUnix sockfdInt ssi = do
    portPath <- loadUnixPath $ CS.regValue addr
    CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL
      %= Map.insert sockfdInt (Some (ssi{GSN.serverSocketAddress = Just portPath}))

  -- For AF_INET(6) sockets, we bind the socket to a port number.
  bindInet ::
    forall domain.
    (GSN.SocketAddress domain ~ Word16) =>
    Integer ->
    GSN.ServerSocketInfo domain ->
    CS.OverrideSim p sym ext r args ret ()
  bindInet sockfdInt ssi = do
    mem <- CS.readGlobal memVar
    portBV <- liftIO $ loadSockaddrInPort bak mem $ CS.regValue addr
    portInt <-
      liftIO $
        fmap BV.asUnsigned $
          networkConstantBv bak "bind" PortArgument (WI.knownNat @16) portBV
    CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL
      %= Map.insert sockfdInt (Some (ssi{GSN.serverSocketAddress = Just (fromInteger portInt)}))

-- | Override for the @connect(2)@ function.
callConnect ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callConnect bak sockfd _addr _addrlen = checkSocketFdInUse bak "connect" sockfd

-- | Override for the @listen(2)@ function.
callListen ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callListen bak sockfd _backlog = checkSocketFdInUse bak "listen" sockfd

-- | Override for the @recv(2)@ function.
callRecv ::
  ( CLM.HasLLVMAnn sym
  , CB.IsSymBackend sym bak
  , CLM.HasPtrWidth w
  ) =>
  bak ->
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callRecv bak fs memVar sockfd buf len _flags = do
  let sym = CB.backendGetSym bak
  -- Drop upper 32 bits from `sockfd` to create a 32 bit file descriptor
  let w32 = WI.knownNat @32
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
  resBv <- CLSIO.callReadFileHandle memVar fs sockfd32Reg buf lenReg

  liftIO $ CLM.llvmPointer_bv sym resBv

-- | Override for the @send(2)@ function.
callSend ::
  ( CLM.HasLLVMAnn sym
  , CB.IsSymBackend sym bak
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  CLSIO.LLVMFileSystem w ->
  CS.GlobalVar CLM.Mem ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
callSend bak fs memVar sockfd buf len _flags = do
  let sym = CB.backendGetSym bak
  -- Drop upper 32 bits from `sockfd` to create a 32 bit file descriptor
  let w32 = WI.knownNat @32
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
  resBv <- CLSIO.callWriteFileHandle memVar fs sockfd32Reg buf lenReg

  liftIO $ CLM.llvmPointer_bv sym resBv

-- | Override for the @socket(2)@ function.
callSocket ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem w ->
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
  Some domainRepr <- pure $ GSN.toSocketDomainRepr domain
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
        GSN.ServerSocketInfo
          { GSN.serverSocketDomain = domainRepr
          , GSN.serverSocketType = typ
          , GSN.serverSocketAddress = Nothing
          , GSN.serverSocketNextConnection = 0
          }
  socketFileLit <-
    liftIO $
      WI.stringLit sym $
        WI.Char8Literal $
          BSC.pack $
            GSN.socketFilePath ssi
  fd <- CSymIo.openFile (CLSIO.llvmFileSystem fs) socketFileLit $ \res -> do
    case res of
      Left CSymIo.FileNotFound -> returnIOError
      Right fileHandle -> do
        fd <- CLSIO.allocateFileDescriptor fs fileHandle
        fdBV <- case WI.asBV fd of
          Just fdBV -> pure fdBV
          Nothing ->
            panic
              "callSocket"
              ["allocateFileDescriptor should return a concrete FD"]
        CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL
          %= Map.insert (BV.asUnsigned fdBV) (Some ssi)
        pure fd
  fdPtr <- liftIO $ CLM.llvmPointer_bv sym fd
  liftIO $ StubsO.adjustPointerSize sym fdPtr ?ptrWidth

-----
-- Exceptions
-----

networkConcretizationFailedSymbolic ::
  HasCallStack =>
  WPL.ProgramLoc ->
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
  WPL.ProgramLoc ->
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
-- Helpers
-----

-- | Check if the socket file descriptor argument has previously been
-- registered. If so, return 0, indicating success. If not, return -1,
-- indicating failure.
checkSocketFdInUse ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth w
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  Text.Text ->
  CS.RegEntry sym (CLM.LLVMPointerType w) ->
  CS.OverrideSim p sym ext r args ret (CLM.LLVMPtr sym w)
checkSocketFdInUse bak fnName sockfd = do
  let sym = CB.backendGetSym bak
  sockfdInt <- liftIO $ networkConstantBvPtrToInteger bak fnName FdArgument $ CS.regValue sockfd
  serverSocketFDs <- use (CS.stateContext . CS.cruciblePersonality . GSN.serverSocketFdsL)
  ec <-
    if Map.member sockfdInt serverSocketFDs
      then returnIOSuccess
      else returnIOError
  ecPtr <- liftIO $ CLM.llvmPointer_bv sym ec
  liftIO $ StubsO.adjustPointerSize sym ecPtr ?ptrWidth

-- | This function digs through the memory in a pointer to a @sockaddr_in@
-- struct (for @AF_INET@ connections) or a @sockaddr_in6@ struct (for @AF_INET6@
-- connections) and loads the port number out of it.
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
    saFamilyTLenBytes = BV.mkBV ?ptrWidth 2
  bvSaFamilyTLenBytes <- WI.bvLit sym ?ptrWidth saFamilyTLenBytes
  sockaddrInPortPtr <- CLM.doPtrAddOffset bak mem sockaddrInPtr bvSaFamilyTLenBytes
  let
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

-- | Concretize a symbolic bitvector representing the argument to a
-- networking-related function override.
networkConstantBv ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , 1 WI.<= w
  ) =>
  bak ->
  Text.Text ->
  NetworkFunctionArgument ->
  WI.NatRepr w ->
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

-- | Like 'networkConstantBv', but for an 'CLM.LLVMPtr' returning an 'Integer'.
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

-- | Return -1, indicating failure.
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

-- | A map of supported socket domains.
socketDomainMap :: Map.Map Integer GSN.SocketDomain
socketDomainMap =
  Map.fromList
    [ (1, GSN.AfUnix)
    , (2, GSN.AfInet)
    , (10, GSN.AfInet6)
    ]

-- | A map of supported socket types.
socketTypeMap :: Map.Map Integer GSN.SocketType
socketTypeMap =
  Map.fromList
    [ (1, GSN.SockStream)
    , (2, GSN.SockDgram)
    , (5, GSN.SockSeqpacket)
    ]
