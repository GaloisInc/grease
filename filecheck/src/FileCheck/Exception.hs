{-# LANGUAGE LambdaCase #-}

module FileCheck.Exception
  ( Exception(..)
  , NoMatch
  , noMatch
  , throwNoMatch
  ) where

import Control.Exception qualified as X
import Control.Monad.Catch qualified as Catch
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import FileCheck.Result qualified as FCR
import Foreign.Marshal qualified as Foreign
import Foreign.Ptr as Foreign
import Foreign.StablePtr (StablePtr)
import Foreign.StablePtr qualified as Foreign
import Foreign.Storable as Foreign
import HsLua qualified as Lua
import HsLua.Core.Utf8 qualified as Lua.Utf8

-- | Exceptions that may be thrown from Lua code.
--
-- Must be storable on the Lua stack. Uses opaque 'Ptr's for data that cannot be
-- inspected by Lua.
data Exception
  = LuaException Lua.Exception
    -- | @fail@ was called.
  | Failure NoMatch

-- | Wrapper for 'FCR.Failure'
newtype NoMatch = NoMatch (Ptr (StablePtr FCR.Failure))

instance Show NoMatch where
  -- can't do IO here, but this Show instance won't be used anyway
  show (NoMatch {}) = "filecheck: no match"

instance Show Exception where
  show =
    \case
      LuaException e -> show e
      Failure f -> show f

instance X.Exception Exception

noMatch :: NoMatch -> IO FCR.Failure
noMatch (NoMatch ptr) = do
  sp <- Foreign.peek ptr
  Foreign.deRefStablePtr sp

throwNoMatch :: FCR.Failure -> Lua.LuaE Exception a
throwNoMatch failure = do
  sp <- liftIO (Foreign.newStablePtr failure)
  ptr <- liftIO Foreign.malloc
  liftIO (Foreign.poke ptr sp)
  Catch.throwM (Failure (NoMatch ptr))

instance Lua.LuaError Exception where
  popException = do
    top <- Lua.tostring Lua.top
    case top of
      Just str -> do
        Lua.pop 1
        pure (LuaException (Lua.Exception (Text.unpack (Text.decodeUtf8Lenient str))))
      Nothing -> do
        top' <- Lua.touserdata Lua.top
        case top' of
          Just ptr -> do
            Lua.pop 1
            pure (Failure (NoMatch ptr))
          Nothing -> Lua.failLua "Bad exception!"

  pushException =
    \case
      LuaException (Lua.Exception msg) -> Lua.pushstring (Lua.Utf8.fromString msg)
      Failure (NoMatch ptr) -> Lua.pushlightuserdata ptr

  luaException s = LuaException (Lua.luaException s)
