{-# LANGUAGE OverloadedStrings #-}

-- | The Oughta Lua API
module Oughta.LuaApi
  ( check
  ) where

import Control.Exception qualified as X
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Oughta.Exception (Exception)
import Oughta.Exception qualified as OE
import Oughta.Extract (LuaProgram, SourceMap, lookupSourceMap, programText, sourceMap, sourceMapFile)
import Oughta.Lua qualified as OL
import Oughta.Pos qualified as OP
import Oughta.Result (Progress, Result)
import Oughta.Result qualified as OR
import Oughta.Traceback qualified as OT
import HsLua qualified as Lua

-- | Name of the @text@ global variable. Not exported.
text :: Lua.Name
text = Lua.Name "text"

-- | Set the @text@ global. Not exported.
setText :: ByteString -> Lua.LuaE Exception ()
setText txt = do
  Lua.pushstring txt
  Lua.setglobal text

-- | Helper, not exported.
withProgress :: IORef Progress -> (Progress -> Lua.LuaE Exception Progress) -> Lua.LuaE Exception ()
withProgress stateRef f = do
  p <- liftIO (IORef.readIORef stateRef)
  p' <- f p
  setText (OR.progressRemainder p')
  liftIO (IORef.writeIORef stateRef p')
  pure ()

-- | Implementation of @col@. Not exported.
col :: IORef Progress -> Lua.LuaE Exception Int
col stateRef = do
  p <- liftIO (IORef.readIORef stateRef)
  pure (OP.col (OP.pos (OR.progressLoc p)))

-- | Implementation of @fail@. Not exported.
fail_ :: SourceMap -> IORef Progress -> Lua.LuaE Exception ()
fail_ sm stateRef =
  withProgress stateRef $ \p -> do
    tb <- OT.getTraceback sm
    OE.throwNoMatch (OR.Failure p tb)

-- | Implementation of @file@. Not exported.
file :: SourceMap -> Lua.LuaE Exception Text
file sm = pure (sourceMapFile sm)

-- | Implementation of @line@. Not exported.
line :: IORef Progress -> Lua.LuaE Exception Int
line stateRef = do
  p <- liftIO (IORef.readIORef stateRef)
  pure (OP.line (OP.pos (OR.progressLoc p)))

-- | Implementation of @match@. Not exported.
match :: SourceMap -> IORef Progress -> Int -> Lua.LuaE Exception ()
match sm stateRef n =
  withProgress stateRef $ \p -> do
    tb <- OT.getTraceback sm
    let txt = OR.progressRemainder p
    let (matched, remainder) = BS.splitAt n txt
    let loc = OR.progressLoc p
    let start = OP.pos loc
    let end = OP.incPos (OP.pos loc) (Text.decodeUtf8Lenient matched)
    let m =
          OR.Match
          { OR.matchRemainder = remainder
          , OR.matchSpan = OP.Span (OP.path loc) start end
          , OR.matchText = matched
          , OR.matchTraceback = tb
          }
    pure (OR.updateProgress m p)

-- | Implementation of @seek@. Not exported.
seek :: IORef Progress -> Int -> Lua.LuaE Exception ()
seek stateRef chars =
  withProgress stateRef $ \p -> do
    let loc = OR.progressLoc p
    let txt = OR.progressRemainder p
    let (before, after) = BS.splitAt chars txt
    let pos' = OP.incPos (OP.pos loc) (Text.decodeUtf8Lenient before)
    let p' =
          p
          { OR.progressLoc = loc { OP.pos = pos' }
          , OR.progressRemainder = after
          }
    pure p'

-- | Implementation of @src_line@. Not exported.
srcLine :: SourceMap -> Int -> Lua.LuaE Exception Int
srcLine sm level = do
  Lua.getglobal' "debug.getinfo"
  -- Empirically, there are 3 levels of functions on the Lua stack between this
  -- function and user Lua code.
  Lua.pushinteger (Lua.Integer (fromIntegral level + 3))
  Lua.pushstring "lnS"
  Lua.call 2 1

  _ty <- Lua.getfield Lua.top "currentline"
  l0 <- Lua.peek @Int Lua.top
  Lua.pop 1

  _ty <- Lua.getfield Lua.top "short_src"
  src0 <- Lua.peek @Text Lua.top
  Lua.pop 1
  let src = Text.drop (Text.length "[string \"") (Text.dropEnd (Text.length "\"]") src0)

  pure (lookupSourceMap src l0 sm)

-- | Load user and Oughta Lua code. Helper, not exported.
luaSetup ::
  IORef Progress ->
  -- | User code
  LuaProgram ->
  -- | Initial content of @text@ global
  ByteString ->
  Lua.LuaE Exception ()
luaSetup stateRef prog txt = do
  Lua.openlibs
  setText txt

  let sm = sourceMap prog

  Lua.pushHaskellFunction (Lua.toHaskellFunction (col stateRef))
  Lua.setglobal (Lua.Name "col_no")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (fail_ sm stateRef))
  Lua.setglobal (Lua.Name "fail")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (file sm))
  Lua.setglobal (Lua.Name "file")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (line stateRef))
  Lua.setglobal (Lua.Name "line")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (match sm stateRef))
  Lua.setglobal (Lua.Name "match")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (seek stateRef))
  Lua.setglobal (Lua.Name "seek")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (srcLine sm))
  Lua.setglobal (Lua.Name "src_line")

  _ <- Lua.loadbuffer OL.luaCode (Lua.Name "oughta.lua")
  Lua.call 0 0

  let nm = Lua.Name (Text.encodeUtf8 (sourceMapFile sm))
  _ <- Lua.loadbuffer (Text.encodeUtf8 (programText prog)) nm
  Lua.call 0 0

-- | Check some text against a Lua program using the API.
check ::
  LuaProgram ->
  -- | Text to check
  ByteString ->
  IO Result
check prog txt = do
  let p0 = OR.newProgress "<out>" txt
  stateRef <- IORef.newIORef p0
  result <- Lua.run (Lua.try (luaSetup stateRef prog txt))
  case result of
    Left (OE.LuaException e) -> X.throwIO e
    Left (OE.Failure noMatch) ->
      OR.Result . Left <$> OE.noMatch noMatch
    Right () -> do
      state <- IORef.readIORef stateRef
      pure (OR.Result (Right (OR.progressToSuccess state)))
