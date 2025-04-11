{-# LANGUAGE OverloadedStrings #-}

-- | Extract Lua programs embedded in other files
module FileCheck.Extract
  ( SourceMap
  , sourceMapFile
  , lookupSourceMap
  , LuaProgram
  , programText
  , sourceMap
  , addPrefix
  , plainLuaProgram
  , fromLines
  , fromLineComments
  ) where

import Data.Foldable qualified as Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text

-- | Map from Lua line numbers to source line numbers
data SourceMap
  = SourceMap
    { sourceMapFile :: !Text
    , sourceMapLines :: !(IntMap Int)
    }

empty :: FilePath -> SourceMap
empty path = SourceMap (Text.pack path) IntMap.empty

lookupSourceMap ::
  -- | File path
  Text ->
  -- | Lua line number
  Int ->
  SourceMap ->
  Int
lookupSourceMap path luaLine (SourceMap f m) =
  if path == f
  then Maybe.fromMaybe luaLine (IntMap.lookup luaLine m)
  else luaLine

-- | Lua program paired with a 'SourceMap'
data LuaProgram
  = LuaProgram
    { programText :: !Text
    , sourceMap :: !SourceMap
    }

-- | Add a prefix (i.e., prelude).
--
-- Bumps all subsequent line numbers
addPrefix :: Text -> LuaProgram -> LuaProgram
addPrefix pfx prog =
  let pfx' = pfx <> "\n" in
  prog
  { programText = pfx' <> programText prog
  , sourceMap =
      let sm = sourceMap prog in
      sm { sourceMapLines = IntMap.mapKeys (+ Text.count "\n" pfx') (sourceMapLines sm) }
  }
    
-- | A standalone Lua program
plainLuaProgram :: FilePath -> Text -> LuaProgram
plainLuaProgram path txt = LuaProgram txt (empty path)

-- | Extract a Lua program embedded in certain lines of another file
fromLines :: FilePath -> (Text -> Maybe Text) -> Text -> LuaProgram
fromLines path filt txt =
  -- luaLineNo starts at 2 becaus we add a newline before each line
  let (_, t, m) = Foldable.foldl' go (2, "", empty path) (zip [1..] (Text.lines txt)) in
  LuaProgram t m
  where
    go (luaLineNo, t, sm) (sourceLineNo, line) =
      case filt line of
        Just line' ->
          -- trace ("LINE '" ++ Text.unpack line' ++ " at " ++ show luaLineNo ++ " is " ++ show sourceLineNo) $
          ( luaLineNo + 1
          , t <> "\n" <> line'
          , sm { sourceMapLines = IntMap.insert luaLineNo sourceLineNo (sourceMapLines sm) }
          )
        Nothing ->
          ( luaLineNo
          , t
          , sm
          )

-- | Extract a Lua program embedded in the line comments of another file
fromLineComments ::
  FilePath ->
  -- | Start of comment marker, e.g., @"# "@
  Text ->
  -- | Whole file
  Text ->
  LuaProgram
fromLineComments path c = fromLines path (Text.stripPrefix c)
