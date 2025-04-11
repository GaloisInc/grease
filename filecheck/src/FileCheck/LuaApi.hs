{-# LANGUAGE OverloadedStrings #-}

-- | The FileCheck Lua API
module FileCheck.LuaApi
  ( check
  ) where

import Control.Exception qualified as X
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text.Encoding qualified as Text
import FileCheck.Exception (Exception)
import FileCheck.Exception qualified as FCE
import FileCheck.Extract (LuaProgram, SourceMap, programText, sourceMap, sourceMapFile)
import FileCheck.Lua qualified as FCL
import FileCheck.Pos qualified as FCP
import FileCheck.Result (Progress, Result)
import FileCheck.Result qualified as FCR
import FileCheck.Traceback qualified as FCT
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
  setText (FCR.progressRemainder p')
  liftIO (IORef.writeIORef stateRef p')
  pure ()

-- | Implementation of @fail@. Not exported.
fail_ :: SourceMap -> IORef Progress -> Lua.LuaE Exception ()
fail_ sm stateRef =
  withProgress stateRef $ \p -> do
    tb <- FCT.getTraceback sm
    FCE.throwNoMatch (FCR.Failure p tb)

-- | Implementation of @match@. Not exported.
match :: SourceMap -> IORef Progress -> Int -> Lua.LuaE Exception ()
match sm stateRef n =
  withProgress stateRef $ \p -> do
    tb <- FCT.getTraceback sm
    let txt = FCR.progressRemainder p
    let (matched, remainder) = BS.splitAt n txt
    let loc = FCR.progressLoc p
    let start = FCP.pos loc
    let end = FCP.incPos (FCP.pos loc) (Text.decodeUtf8Lenient matched)
    let m =
          FCR.Match
          { FCR.matchRemainder = remainder
          , FCR.matchSpan = FCP.Span (FCP.path loc) start end
          , FCR.matchText = matched
          , FCR.matchTraceback = tb
          }
    pure (FCR.updateProgress m p)

-- | Implementation of @seek@. Not exported.
seek :: IORef Progress -> Int -> Lua.LuaE Exception ()
seek stateRef chars =
  withProgress stateRef $ \p -> do
    let loc = FCR.progressLoc p
    let txt = FCR.progressRemainder p
    let (before, after) = BS.splitAt chars txt
    let pos' = FCP.incPos (FCP.pos loc) (Text.decodeUtf8Lenient before)
    let p' =
          p
          { FCR.progressLoc = loc { FCP.pos = pos' }
          , FCR.progressRemainder = after
          }
    pure p'

-- | Load user and FileCheck Lua code. Helper, not exported.
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

  Lua.pushHaskellFunction (Lua.toHaskellFunction (fail_ sm stateRef))
  Lua.setglobal (Lua.Name "fail")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (match sm stateRef))
  Lua.setglobal (Lua.Name "match")

  Lua.pushHaskellFunction (Lua.toHaskellFunction (seek stateRef))
  Lua.setglobal (Lua.Name "seek")

  _ <- Lua.loadbuffer FCL.luaCode (Lua.Name "filecheck.lua")
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
  let p0 = FCR.newProgress "<out>" txt
  stateRef <- IORef.newIORef p0
  result <- Lua.run (Lua.try (luaSetup stateRef prog txt))
  case result of
    Left (FCE.LuaException e) -> X.throwIO e
    Left (FCE.Failure noMatch) ->
      FCR.Result . Left <$> FCE.noMatch noMatch
    Right () -> do
      state <- IORef.readIORef stateRef
      pure (FCR.Result (Right (FCR.progressToSuccess state)))
