{-# LANGUAGE OverloadedStrings #-}

-- | The FileCheck Lua API
module FileCheck.LuaApi
  ( check
  ) where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import FileCheck.Command qualified as FCC
import FileCheck.Directive qualified as FCD
import FileCheck.Pos qualified as FCP
import FileCheck.LuaApi.Func (LuaFunc)
import FileCheck.LuaApi.Func qualified as FCLAF
import FileCheck.Result (Failure, Progress, Result)
import FileCheck.Result qualified as FCR
import HsLua qualified as Lua

-- | Current state of the Lua API. Not exported.
newtype State = State (Either Failure Progress)

-- | Helper, not exported
newState :: FilePath -> Text -> State
newState path txt =
  let p0 = FCR.newProgress path txt in
  State (Right p0)

-- | Helper, not exported
finalizeState :: State -> FCR.Result
finalizeState (State s) = FCR.Result (FCR.progressToSuccess <$> s)

-- | The implementation of a function in the Lua API
funcImpl :: LuaFunc -> IORef State -> Lua.HaskellFunction Lua.Exception
funcImpl f stateRef =
  case f of
    FCLAF.Check -> Lua.toHaskellFunction (checkImpl stateRef )

-- | Get Lua source line number. Helper, not exported.
getLineNo :: Lua.Lua FCP.LuaLine
getLineNo = do
  Lua.getglobal' "debug.getinfo"
  Lua.pushinteger 3
  Lua.call 1 1
  _ <- Lua.getfield Lua.top "currentline"
  line <- Lua.peek Lua.top
  Lua.pop 1
  pure (FCP.LuaLine line)

-- | Helper, not exported.
withState :: IORef State -> (State -> Lua.Lua State) -> Lua.Lua ()
withState stateRef f = do
  state <- liftIO (IORef.readIORef stateRef)
  state' <- f state
  liftIO (IORef.writeIORef stateRef state')

-- | Implementation of 'Check'. Not exported.
checkImpl :: IORef State -> Text -> Lua.Lua ()
checkImpl stateRef str =
  withState stateRef $ \s@(State state) -> do
    line <- getLineNo
    case state of
      Left {} -> pure s
      Right p@(FCR.Progress { FCR.progressLoc = loc, FCR.progressMatches = ms, FCR.progressRemainder = txt }) -> do
        let start = FCP.Pos (FCP.getLuaLine line) 1
        let cmd = FCC.Command FCD.Check str (Just (FCP.Span (Just "<lua>") start start))
        case FCC.match cmd loc txt of
          Nothing -> pure (State (Left (FCR.Failure FCLAF.Check line p)))
          Just m ->
            let m' =
                  FCR.Match
                  { FCR.matchFunc = FCLAF.Check
                  , FCR.matchLuaLine = line
                  , FCR.matchRemainder = FCC.matchRemainder m
                  , FCR.matchSpan = FCC.matchSpan m
                  , FCR.matchText = FCC.matchText m
                  } in
            let ms' = ms Seq.:|> m' in
            pure (State (Right p { FCR.progressMatches = ms' }))

-- | Check some text against a Lua program using the API.
check ::
  -- | Lua code
  Text ->
  -- | Text to check
  Text ->
  IO Result
check luaCode txt = do
  stateRef <- IORef.newIORef (newState "<out>" txt)
  Lua.run @Lua.Exception $ do
    Lua.openlibs
    Monad.forM_ [minBound @LuaFunc .. maxBound] $ \func -> do
      Lua.pushHaskellFunction (funcImpl func stateRef)
      Lua.setglobal (FCLAF.funcLuaName func)
    _ <- Lua.loadbuffer (Text.encodeUtf8 luaCode) "test"
    Lua.call 0 0
  state <- IORef.readIORef stateRef
  pure (finalizeState state)
