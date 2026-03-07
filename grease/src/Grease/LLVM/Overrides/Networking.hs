{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- TODO(#438): Remove calls to `error`
{- HLINT ignore "Use panic" -}

-- | Overrides for network-related functions (LLVM backend).
module Grease.LLVM.Overrides.Networking (
  networkLLVMOverrides,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.BitVector.Sized qualified as BV
import Data.ByteString qualified as BS
import Data.Parameterized.Context qualified as Ctx
import Grease.LLVM.Overrides.Declare (mkDeclare)
import Grease.Overrides.Networking qualified as GON
import Grease.SimulatorState.Networking qualified as GSN
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as CBO
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.SymIO qualified as CLSIO
import Lang.Crucible.Simulator qualified as CS
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

-- | All of the socket I/O–related LLVM overrides.
--
-- Note that @bak@ is captured in the override closures so that the online
-- solver is available at override invocation time.
networkLLVMOverrides ::
  ( CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CS.GlobalVar CLM.Mem ->
  [CLI.SomeLLVMOverride p sym ext]
networkLLVMOverrides bak fs memVar =
  [ CLI.SomeLLVMOverride (socketOverride bak fs)
  , CLI.SomeLLVMOverride (bindOverride bak fs memVar)
  , CLI.SomeLLVMOverride (connectOverride bak)
  , CLI.SomeLLVMOverride (listenOverride bak)
  , CLI.SomeLLVMOverride (acceptOverride bak fs)
  , CLI.SomeLLVMOverride (recvOverride bak fs memVar)
  , CLI.SomeLLVMOverride (sendOverride bak fs memVar)
  ]

-----
-- Individual LLVM overrides
-----

socketOverride ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth 64
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
socketOverride bak fs =
  case mkDeclare "socket" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callSocket bak fs) args
        }
    Left err -> error ("socketOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

bindOverride ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CS.GlobalVar CLM.Mem ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
bindOverride bak _fs memVar =
  case mkDeclare "bind" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment
              (GON.callBind bak memVar (loadSockaddrUnPath bak memVar))
              args
        }
    Left err -> error ("bindOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

connectOverride ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth 64
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
connectOverride bak =
  case mkDeclare "connect" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callConnect bak) args
        }
    Left err -> error ("connectOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

listenOverride ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth 64
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
listenOverride bak =
  case mkDeclare "listen" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callListen bak) args
        }
    Left err -> error ("listenOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

acceptOverride ::
  ( CB.IsSymBackend sym bak
  , sym ~ WE.ExprBuilder scope st fs
  , bak ~ CBO.OnlineBackend solver scope st fs
  , WPO.OnlineSolver solver
  , CLM.HasPtrWidth 64
  , GSN.HasServerSocketFds p
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
acceptOverride bak fs =
  case mkDeclare "accept" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callAccept bak fs) args
        }
    Left err -> error ("acceptOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

recvOverride ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth 64
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CS.GlobalVar CLM.Mem ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
recvOverride bak fs memVar =
  case mkDeclare "recv" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callRecv bak fs memVar) args
        }
    Left err -> error ("recvOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

sendOverride ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth 64
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  CLSIO.LLVMFileSystem 64 ->
  CS.GlobalVar CLM.Mem ->
  CLI.LLVMOverride
    p
    sym
    ext
    ( Ctx.EmptyCtx
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
        Ctx.::> CLM.LLVMPointerType 64
    )
    (CLM.LLVMPointerType 64)
sendOverride bak fs memVar =
  case mkDeclare "send" argTys retTy of
    Right decl ->
      CLI.LLVMOverride
        { CLI.llvmOverride_declare = decl
        , CLI.llvmOverride_args = argTys
        , CLI.llvmOverride_ret = retTy
        , CLI.llvmOverride_def = \_mvar args ->
            Ctx.uncurryAssignment (GON.callSend bak fs memVar) args
        }
    Left err -> error ("sendOverride: " ++ show err)
 where
  argTys = Ctx.Empty Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr Ctx.:> CLM.PtrRepr
  retTy = CLM.PtrRepr

-----
-- LLVM-specific helpers
-----

-- | Load the @sun_path@ from a @sockaddr_un@ struct using @crucible-llvm@'s
-- 'CLM.loadString'.
loadSockaddrUnPath ::
  ( CB.IsSymBackend sym bak
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth w
  , ?memOpts :: CLM.MemOptions
  ) =>
  bak ->
  CS.GlobalVar CLM.Mem ->
  CLM.LLVMPtr sym w ->
  CS.OverrideSim p sym ext r args ret BS.ByteString
loadSockaddrUnPath bak memVar sockaddrUnPtr = do
  let sym = CB.backendGetSym bak
  mem <- CS.readGlobal memVar
  let
    saFamilyTLenBytes = BV.mkBV ?ptrWidth 2
  bvSaFamilyTLenBytes <- liftIO $ WI.bvLit sym ?ptrWidth saFamilyTLenBytes
  sockaddrUnPathPtr <- liftIO $ CLM.doPtrAddOffset bak mem sockaddrUnPtr bvSaFamilyTLenBytes
  -- Note that the maximum size of @sun_path@ is 108 characters, which is why
  -- we pass @Just 108@ here.
  bytes <- liftIO $ CLM.loadString bak mem sockaddrUnPathPtr (Just 108)
  pure $ BS.pack bytes
