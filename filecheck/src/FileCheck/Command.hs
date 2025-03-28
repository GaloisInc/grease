{-# LANGUAGE OverloadedStrings #-}

module FileCheck.Command
  ( Command(..)
  , parse
  , match
  ) where

import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text (Text)
import FileCheck.Directive (Directive, Prefix)
import FileCheck.Directive qualified as FCD

data Command
  = Command
    { cmdDirective :: Directive
    , cmdContent :: Text
    }

splitOnOne :: Text -> Text -> (Text, Text)
splitOnOne needle haystack =
  let (hd, tl) = Text.breakOn needle haystack in
  (hd, Text.drop (Text.length needle) tl)


parse :: Maybe Prefix -> Text -> Maybe Command
parse pfx t = do
  let split = ":"
  let (hd, tl) = splitOnOne split t
  d <- FCD.parse pfx hd
  let tl' = Maybe.fromMaybe tl (Text.stripPrefix " " tl)
  pure (Command d tl')

-- | Match a 'Command' against a chunk of 'Text'.
--
-- Returns 'Just' the remaining text after the match, or 'Nothing' if matching
-- fails.
match ::
  Command ->
  Text ->
  Maybe Text
match (Command d c) t =
  case d of
    FCD.Check ->
      if Text.null c
      then Just t
      else
        let (hd, tl) = splitOnOne c t in
        if hd == t
        then Nothing
        else Just tl
    FCD.CheckEmpty ->
      let split = 
            if Text.null c
            then "\n"
            else c <> "\n"
      in
        let (_hd, tl) = splitOnOne split t in
        Text.stripPrefix "\n" tl
    FCD.CheckNext ->
      -- First, go to the next line
      let (hd0, tl0) = splitOnOne "\n" t in
      if hd0 == t
      then Nothing
      else
        -- Then, look for the needle on that line
        let (hd1, tl1) = splitOnOne "\n" tl0 in
        let (hd2, tl2) = splitOnOne c hd1 in
        if hd2 == hd1
        then Nothing
        else Just (tl2 <> tl1)
    FCD.CheckNot -> Nothing -- TODO
    FCD.Com -> Just t
