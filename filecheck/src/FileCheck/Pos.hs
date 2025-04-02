{-# LANGUAGE OverloadedStrings #-}

module FileCheck.Pos
  ( Pos(..)
  , Loc(..)
  , printLoc
  ) where

import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text (Text)

-- | A source position
data Pos
  = Pos
    { line :: {-# UNPACK #-} !Int
    , col :: {-# UNPACK #-} !Int
    }

data Loc
  = Loc
    { path :: !(Maybe FilePath)
    , pos :: {-# UNPACK #-} !Pos
    }

printLoc :: Loc -> Text
printLoc l =
  let p = Maybe.fromMaybe "<unknown file>" (path l) in
  let tshow = Text.pack . show in
  mconcat [Text.pack p, ":", tshow (line (pos l)), ":", tshow (col (pos l))]

