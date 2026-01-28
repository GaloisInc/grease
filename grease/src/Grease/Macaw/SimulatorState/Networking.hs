{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Grease.Macaw.SimulatorState.Networking (
  -- * @ServerSocketInfo@ and friends
  ServerSocketInfo (..),
  SocketDomain (..),
  SocketDomainRepr (..),
  fromSocketDomainRepr,
  toSocketDomainRepr,
  SocketAddress,
  SocketType (..),

  -- * Socket file path conventions
  socketFilePath,
  acceptAfUnixFilePath,
  acceptAfInetFilePath,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Kind (Type)
import Data.Parameterized.Some (Some (Some))
import Data.Word (Word16)
import Prettyprinter qualified as PP
import System.FilePath qualified as FilePath

-- See Note [The networking story] in Grease.Macaw.Overrides.Networking for a
-- high-level overview of how these data types are used to model network IO.

-- | A collection of metadata about sockets that is useful for server programs
-- (i.e., programs that call @accept()@).
type ServerSocketInfo :: SocketDomain -> Type
data ServerSocketInfo domain = ServerSocketInfo
  { serverSocketDomain :: SocketDomainRepr domain
  -- ^ The socket's domain as specified by the first argument to the
  -- @socket()@ syscall.
  , serverSocketType :: SocketType
  -- ^ The socket's type as specified by the second argument to the
  -- @socket()@ syscall.
  , serverSocketAddress :: Maybe (SocketAddress domain)
  -- ^ If this socket has been assigned via @bind()@, this is
  -- @'Just' addr@. If not, this is 'Nothing'. The type of @addr@ depends on
  -- the socket domain—see the Haddocks for 'SocketAddress'.
  , serverSocketNextConnection :: Word
  -- ^ The number to use for the socket file allocated by the next call to
  -- @accept()@.
  }

deriving instance Show (SocketAddress domain) => Show (ServerSocketInfo domain)

-- | All of the socket domains that @grease@ currently supports. In addition to
-- being used at the value level, this is also used at the type level to compute
-- the type of the 'SocketAddress', which depends on the domain.
data SocketDomain
  = -- | @AF_UNIX@
    AfUnix
  | -- | @AF_INET@
    AfInet
  | -- | @AF_INET6@
    AfInet6
  deriving Show

instance PP.Pretty SocketDomain where
  pretty AfUnix = "AF_UNIX"
  pretty AfInet = "AF_INET"
  pretty AfInet6 = "AF_INET6"

-- | A singleton type for 'SocketDomain'.
type SocketDomainRepr :: SocketDomain -> Type
data SocketDomainRepr domain where
  AfUnixRepr :: SocketDomainRepr 'AfUnix
  AfInetRepr :: SocketDomainRepr 'AfInet
  AfInet6Repr :: SocketDomainRepr 'AfInet6

deriving instance Show (SocketDomainRepr domain)

-- | Obtain a 'SocketDomain' from its corresponding singleton.
fromSocketDomainRepr :: SocketDomainRepr domain -> SocketDomain
fromSocketDomainRepr AfUnixRepr = AfUnix
fromSocketDomainRepr AfInetRepr = AfInet
fromSocketDomainRepr AfInet6Repr = AfInet6

-- | Convert a singleton to its corresponding singleton.
toSocketDomainRepr :: SocketDomain -> Some SocketDomainRepr
toSocketDomainRepr AfUnix = Some AfUnixRepr
toSocketDomainRepr AfInet = Some AfInetRepr
toSocketDomainRepr AfInet6 = Some AfInet6Repr

-- | The type of address corresponding to a particular socket domain.
type SocketAddress :: SocketDomain -> Type
type family SocketAddress domain where
  SocketAddress 'AfUnix = BS.ByteString -- A file path (`sun_path`)
  SocketAddress 'AfInet = Word16 -- A port number (`sin_addr`)
  SocketAddress 'AfInet6 = Word16 -- A port number (`sin6_addr`)

-- | All of the supported socket types.
data SocketType
  = -- | @SOCK_STREAM@
    SockStream
  | -- | @SOCK_DGRAM@
    SockDgram
  | -- | @SOCK_SEQPACKET@
    SockSeqpacket
  deriving Show

instance PP.Pretty SocketType where
  pretty SockStream = "SOCK_STREAM"
  pretty SockDgram = "SOCK_DGRAM"
  pretty SockSeqpacket = "SOCK_SEQPACKET"

-- These functions implement the file path conventions described in
-- Note [The networking story] in Grease.Macaw.Overrides.Networking.

-- | In the @socket()@ override, the returned socket file descriptor is modeled
-- with a file located at @/network/<domain_macro>/<type_macro>/socket@.
socketFilePath :: ServerSocketInfo domain -> FilePath
socketFilePath ssi = mkSocketPathPrefix ssi FilePath.</> "socket"

-- | In our @accept()@ override, @AF_UNIX@ sockets are modeled with files
-- located at @<sun_path>/<seq_num>@.
acceptAfUnixFilePath ::
  BS.ByteString ->
  ServerSocketInfo 'AfUnix ->
  FilePath
acceptAfUnixFilePath
  sockPath
  ( ServerSocketInfo
      { serverSocketNextConnection = sockNextConn
      }
    ) =
    BSC.unpack sockPath FilePath.</> show sockNextConn

-- | In our @accept()@ override, @AF_INET(6)@ sockets are modeled with files
-- located at @/network/<domain_macro>/<type_macro>/<port>/<seq_num>@.
--
-- NB: while @domain@ is universally quantified here, it only makes sense
-- to instantiate it at @AF_INET@ or @AF_INET6@.
acceptAfInetFilePath ::
  Word16 ->
  ServerSocketInfo domain ->
  FilePath
acceptAfInetFilePath
  sockPort
  ssi@( ServerSocketInfo
          { serverSocketNextConnection = sockNextConn
          }
        ) =
    mkSocketPathPrefix ssi
      FilePath.</> show sockPort
      FilePath.</> show sockNextConn

-- | Construct a @/network/<domain_macro>/<type_macro>@ file path for a socket.
-- This is shared in common between 'socketFilePath' and 'acceptAfInetPath'.
mkSocketPathPrefix :: ServerSocketInfo domain -> FilePath
mkSocketPathPrefix
  ( ServerSocketInfo
      { serverSocketDomain = domainRepr
      , serverSocketType = typ
      }
    ) =
    "/network"
      FilePath.</> show (PP.pretty (fromSocketDomainRepr domainRepr))
      FilePath.</> show (PP.pretty typ)
