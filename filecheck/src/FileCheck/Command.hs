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
import FileCheck.Pos (Loc)

-- | A 'Command' is a pair of a 'Directive' and its argument.
--
-- It represents a single line written by the user, e.g., @CHECK: foo@ is parsed
-- to @'Command' 'FCD.Check' "foo"@.
data Command
  = Command
    { cmdDirective :: Directive
    , cmdContent :: !Text
    , cmdPos :: !(Maybe Loc)
    }

splitOnOne :: Text -> Text -> (Text, Text)
splitOnOne needle haystack =
  let (hd, tl) = Text.breakOn needle haystack in
  (hd, Text.drop (Text.length needle) tl)

-- | Parse a 'Command'. Returns 'Nothing' on parse failure.
parse :: Maybe Prefix -> Maybe Loc -> Text -> Maybe Command
parse pfx pos t = do
  let split = ":"
  let (hd, tl) = splitOnOne split t
  d <- FCD.parse pfx hd
  let tl' = Maybe.fromMaybe tl (Text.stripPrefix " " tl)
  pure (Command d tl' pos)

-- | Match a 'Command' against a chunk of 'Text'.
--
-- Returns 'Just' the remaining text after the match, or 'Nothing' if matching
-- fails. See the README and test suite for more details about the semantics
-- of matching.
match ::
  Command ->
  Text ->
  Maybe Text
match (Command d c _pos) t =
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
