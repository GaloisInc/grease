{-# LANGUAGE OverloadedStrings #-}

module FileCheck.Command
  ( Command(..)
  , parse
  , Match(..)
  , match
  ) where

import Data.Text qualified as Text
import Data.Text (Text)
import FileCheck.Directive (Directive, Prefix)
import FileCheck.Directive qualified as FCD
import FileCheck.Pos qualified as FCP
import FileCheck.Pos (Loc, Span)
import Prelude hiding (span)

-- | A t'Command' is a pair of a 'Directive' and its argument.
--
-- It represents a single line written by the user, e.g., @CHECK: foo@ is parsed
-- to @v'Command' 'FCD.Check' "foo"@.
data Command
  = Command
    { cmdDirective :: Directive
    , cmdContent :: !Text
    , cmdSpan :: !(Maybe Span)
    }

splitOnOne :: Text -> Text -> (Text, Text)
splitOnOne needle haystack =
  let (hd, tl) = Text.breakOn needle haystack in
  (hd, Text.drop (Text.length needle) tl)

-- | Parse a t'Command'. Returns 'Nothing' on parse failure.
parse :: Maybe Prefix -> Text -> Maybe Loc -> Text -> Maybe Command
parse pfx comment loc t0 = do
  t <- Text.stripPrefix comment t0
  let split = ":"
  let (hd, tl) = splitOnOne split t
  d <- FCD.parse pfx hd
  let tl' = Text.strip tl
  let span = 
        case loc of
          Nothing -> Nothing
          Just l ->
            let start = FCP.pos l in
            Just (FCP.Span (FCP.path l) start (FCP.incPos start t0))
  pure (Command d tl' span)

-- | A successful match of some t'Command' against some 'Text'
data Match
  = Match
    { -- | The 'Span' of the match
      matchSpan :: {-# UNPACK #-} !Span
      -- | The 'Text' that was matched
    , matchText :: !Text
      -- | The rest of the 'Text' after the match
    , matchRemainder :: !Text
    }

-- | Match a t'Command' against a chunk of 'Text'.
--
-- Returns 'Just' the remaining text after the match, or 'Nothing' if matching
-- fails. See the README and test suite for more details about the semantics
-- of matching.
match ::
  Command ->
  Loc ->
  Text ->
  Maybe Match
match (Command d c _loc) loc t =
  let start = FCP.pos loc in
  let here = FCP.Span (FCP.path loc) start start in
  let mkSpan = FCP.Span (FCP.path loc) in
  case d of
    FCD.Check ->
      if Text.null c
      then Just (Match here "" t)
      else
        let (hd, tl) = splitOnOne c t in
        if hd == t
        then Nothing
        else
          let start' = FCP.incPos (FCP.pos loc) hd in
          let loc' = mkSpan start' (FCP.incPos start' c) in
          Just (Match loc' c tl)
    FCD.CheckEmpty ->
      let split = 
            if Text.null c
            then "\n"
            else c <> "\n"
      in do
        let (hd, tl) = splitOnOne split t
        tl' <- Text.stripPrefix "\n" tl
        let start' = FCP.incPos (FCP.pos loc) hd
        let loc' = mkSpan start' (FCP.incPos start' (c <> "\n"))
        Just (Match loc' c tl')
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
        else
          let start' = FCP.incPos (FCP.pos loc) (hd0 <> "\n" <> hd2) in
          let loc' = mkSpan start' (FCP.incPos start' c) in
          Just (Match loc' c (tl2 <> tl1))
    FCD.CheckNot -> Nothing  -- TODO
    FCD.Com -> Just (Match here "" t)
