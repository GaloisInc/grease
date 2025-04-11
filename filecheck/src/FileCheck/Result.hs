{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The result of running a FileCheck Lua program
module FileCheck.Result
  ( Match(..)
  , Progress(..)
  , newProgress
  , updateProgress
  , progressToSuccess
  , Failure(..)
  , Success(..)
  , Result(..)
  , resultNull
  , printResult
  ) where

import Control.Exception qualified as X
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import FileCheck.Pos (Loc, Span)
import FileCheck.Pos qualified as FCP
import FileCheck.Traceback (Traceback)
import FileCheck.Traceback qualified as FCT

-- | A successful match of an API call against some text
data Match
  = Match
    { -- | The 'Span' of the match
      matchSpan :: {-# UNPACK #-} !Span
      -- | The 'Text' that was matched
    , matchText :: !ByteString
      -- | 'Traceback' at the time of the match
    , matchTraceback :: !Traceback
      -- | The rest of the 'Text' after the match
    , matchRemainder :: !ByteString
    }

indent :: Text
indent = "  "

indentLines :: Text -> Text
indentLines = Text.unlines . map (indent <>) . Text.lines

printMatch :: Match -> Text
printMatch m =
  Text.unlines
  [ Text.unwords
    [ "✔️ match at"
    , FCP.printSpan (matchSpan m) <> ":"
    ]
  , indentLines (Text.decodeUtf8Lenient (matchText m))
  , FCT.printTraceback (matchTraceback m)
  ]

-- | A sequence of successful matches of API calls against some text
data Progress
 = Progress
   { -- | t'Loc' after the last match
     progressLoc :: {-# UNPACK #-} !Loc
     -- | Successful 'Match'es
   , progressMatches :: Seq Match
     -- | Remaining text after the last match
   , progressRemainder :: !ByteString
   }

printProgress :: Progress -> Text
printProgress p = Text.unlines (map printMatch (toList (progressMatches p)))

-- | Create a new 'Progress' starting at position @'FCP.Pos' 1 1@.
newProgress :: FilePath -> ByteString -> Progress
newProgress path txt =
  let loc0 = FCP.Loc (Just path) (FCP.Pos 1 1) in
  Progress loc0 Seq.empty txt

-- | Update 'Progress' with a new 'Match'
updateProgress :: Match -> Progress -> Progress
updateProgress m p =
  Progress
  { progressLoc = (progressLoc p) { FCP.pos = FCP.spanEnd (matchSpan m) }
  , progressMatches = progressMatches p Seq.:|> m
  , progressRemainder = matchRemainder m
  }

-- | Helper, not exported
progressToSuccess :: Progress -> Success
progressToSuccess (Progress loc matches remainder) =
  Success loc matches remainder

-- | Failure to match a program against some text.
data Failure
 = Failure
   { failureProgress :: Progress
   , failureTraceback :: !Traceback
   }

trunc :: Text -> Text
trunc txt =
  let ls = Text.lines txt in
  if length ls > 3
  then Text.unlines (take 3 ls) <> "\n..."
  else txt

instance Show Failure where
  show f =
    Text.unpack $
      Text.unlines
      [ ""  -- a leading newline makes the output of Tasty more readable
      , "Check failed! Passing checks:"
      , printProgress (failureProgress f)
      , "Failing check:"
      , Text.unwords
        [ "❌ no match at"
        , FCP.printLoc (progressLoc (failureProgress f)) <> ":"
        ]
      , indentLines (trunc (Text.decodeUtf8Lenient (progressRemainder (failureProgress f))))
      , FCT.printTraceback (failureTraceback f)
      ]

instance X.Exception Failure

-- | The result of matching a program against some text.
data Success
 = Success
   { -- | t'Loc' after the last match
     successLoc :: {-# UNPACK #-} !Loc
     -- | Successful 'Match'es
   , successMatches :: Seq Match
     -- | Remaining text after the last match
   , successRemainder :: !ByteString
   }

-- | The result of running a FileCheck Lua program
newtype Result = Result (Either Failure Success)

-- | Does this 'Rusult' reflect running zero checks?
resultNull :: Result -> Bool
resultNull =
  \case
    Result (Left {}) -> False
    Result (Right s) -> null (successMatches s)

-- | Display a 'Result' in human-readable 'Text'
printResult :: Result -> Text
printResult =
  \case
    Result (Left f) -> Text.pack (show f)
    Result (Right (Success loc matches remainder)) ->
      printProgress (Progress loc matches remainder)
