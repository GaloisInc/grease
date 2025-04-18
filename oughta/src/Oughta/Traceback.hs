{-# LANGUAGE OverloadedStrings #-}

-- | Get Lua stack traces
module Oughta.Traceback
  ( Traceback
  , getTraceback
  , printTraceback
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Oughta.Extract (SourceMap)
import Oughta.Extract qualified as OE
import HsLua qualified as Lua
import qualified Data.Text as Text

-- | Data about a single stack frame
data Frame
  = Frame
    { -- | Source line number (NOT Lua line number)
      _frameLine :: {-# UNPACK #-} !Int
    , _frameName :: !Text
    , frameSource :: !Text
    }

printFrame :: Frame -> Text
printFrame (Frame line name src) =
  src <> ":" <> Text.pack (show line) <> " in " <> name

-- | The stack trace from the Lua interpreter.
--
-- Excludes C internals.
newtype Traceback = Traceback { _getTraceback :: [Frame] }

getFrame ::
  Lua.LuaError e =>
  SourceMap ->
  -- | Lua stack level
  Int64 ->
  Lua.LuaE e (Maybe Frame)
getFrame sm level = do
  Lua.getglobal' "debug.getinfo"
  Lua.pushinteger (Lua.Integer level)
  Lua.pushstring "lnS"
  Lua.call 2 1

  nil <- Lua.isnil Lua.top
  if nil
  then do
    Lua.pop 1
    pure Nothing
  else do
    _ty <- Lua.getfield Lua.top "what"
    what <- Lua.peek @Text Lua.top
    Lua.pop 1

    ty <- Lua.getfield Lua.top "name"
    name <-
      case ty of
        Lua.TypeString -> do
          name <- Lua.peek @Text Lua.top
          pure name
        _ -> pure "<main>"
    Lua.pop 1

    _ty <- Lua.getfield Lua.top "short_src"
    src0 <- Lua.peek @Text Lua.top
    Lua.pop 1

    let src = Text.drop (Text.length "[string \"") (Text.dropEnd (Text.length "\"]") src0)
    let src' =
          if what == "C"
          then "C"
          else src

    _ty <- Lua.getfield Lua.top "currentline"
    l0 <- Lua.peek @Int Lua.top
    Lua.pop 1
    let l = OE.lookupSourceMap src' l0 sm

    pure (Just (Frame l name src'))

-- | Get a Lua stack trace.
getTraceback ::
  Lua.LuaError e =>
  SourceMap ->
  Lua.LuaE e Traceback
getTraceback sm =
  Traceback . filter ((== OE.sourceMapFile sm) . frameSource) . reverse <$> go 3 []
  where
    go level frames = do
      mf <- getFrame sm level
      case mf of
        Nothing -> pure frames
        Just f -> go (level + 1) (f : frames)

printTraceback :: Traceback -> Text
printTraceback (Traceback tb) =
  Text.unlines ("stack trace:" : map (("  " <>) . printFrame) tb)
