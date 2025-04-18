{-# LANGUAGE OverloadedStrings #-}

module Oughta.Pos
  ( Pos(..)
  , incPos
  , Loc(..)
  , printLoc
  , Span(..)
  , startLoc
  , endLoc
  , printSpan
  ) where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text (Text)
import Prelude hiding (last, lines, span)

-- | Helper, not exported
tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | A source position
data Pos
  = Pos
    { line :: {-# UNPACK #-} !Int
    , col :: {-# UNPACK #-} !Int
    }

-- | Move a position forward past a chunk of text.
incPos :: Pos -> Text -> Pos
incPos (Pos l c) t =
  let newlines = Text.count "\n" t in
  Pos (l + newlines) $
    if "\n" `Text.isSuffixOf` t
    then 1
    else
      case List.reverse (Text.lines t) of
        [] -> c
        (only : []) -> c + Text.length only
        (last : _) -> Text.length last

printPos :: Pos -> Text
printPos p = tshow (line p) <> ":" <> tshow (col p)

-- | A source location
data Loc
  = Loc
    { path :: !(Maybe FilePath)
    , pos :: {-# UNPACK #-} !Pos
    }

printLoc :: Loc -> Text
printLoc l =
  let p = Maybe.fromMaybe "<unknown file>" (path l) in
  Text.pack p <> ":" <> printPos (pos l)

-- | A source span
data Span
  = Span
  { spanPath :: !(Maybe FilePath)
  , spanStart :: {-# UNPACK #-} !Pos
  , spanEnd :: {-# UNPACK #-} !Pos
  }

startLoc :: Span -> Loc
startLoc s = Loc (spanPath s) (spanStart s)

endLoc :: Span -> Loc
endLoc s = Loc (spanPath s) (spanEnd s)

printSpan :: Span -> Text
printSpan s =
  let p = Maybe.fromMaybe "<unknown file>" (spanPath s) in
  Text.pack p <> ":" <> printPos (spanStart s) <> "-" <> printPos (spanEnd s)
