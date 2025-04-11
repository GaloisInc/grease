{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The result of running a FileCheck Lua program
module FileCheck.Result
  ( Match(..)
  , Progress(..)
  , newProgress
  , progressToSuccess
  , Failure(..)
  , Success(..)
  , Result(..)
  , resultNull
  , printResult
  ) where

import Control.Exception qualified as X
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import FileCheck.LuaApi.Func (LuaFunc, funcName)
import FileCheck.Pos (Loc, Span)
import FileCheck.Pos qualified as FCP

-- | A successful match of an API call against some text
data Match
  = Match
    { -- | The function that matched
      matchFunc :: {-# UNPACK #-} !LuaFunc
    , matchLuaLine :: {-# UNPACK #-} !FCP.LuaLine
      -- | The 'Span' of the match
    , matchSpan :: {-# UNPACK #-} !Span
      -- | The 'Text' that was matched
    , matchText :: !Text
      -- | The rest of the 'Text' after the match
    , matchRemainder :: !Text
    }

indent :: Text
indent = "  "

indentLines :: Text -> Text
indentLines = Text.unlines . map (indent <>) . Text.lines

printMatch :: Match -> Text
printMatch m =
  Text.unlines
  [ Text.unwords
    [ "✔️"
    , funcName (matchFunc m)
    , "on line"
    , Text.pack (show (FCP.getLuaLine (matchLuaLine m)))
    , "matched text at"
    , FCP.printSpan (matchSpan m)
    ]
  , "match:"
  , indentLines (matchText m)
  ]

-- | A sequence of successful matches of API calls against some text
data Progress
 = Progress
   { -- | t'Loc' after the last match
     progressLoc :: {-# UNPACK #-} !Loc
     -- | Successful 'Match'es
   , progressMatches :: Seq Match
     -- | Remaining text after the last match
   , progressRemainder :: !Text
   }

printProgress :: Progress -> Text
printProgress p = Text.unlines (map printMatch (toList (progressMatches p)))

-- | Helper, not exported
newProgress :: FilePath -> Text -> Progress
newProgress path txt =
  let loc0 = FCP.Loc (Just path) (FCP.Pos 1 1) in
  Progress loc0 Seq.empty txt

-- | Helper, not exported
progressToSuccess :: Progress -> Success
progressToSuccess (Progress loc matches remainder) =
  Success loc matches remainder

-- | Failure to match a program against some text.
data Failure
 = Failure
   { failureFailedFunc :: {-# UNPACK #-} !LuaFunc
   , failureLuaLine :: {-# UNPACK #-} !FCP.LuaLine
   , failureProgress :: Progress
   }

instance Show Failure where
  show f =
    Text.unpack $
      Text.unlines
      [ ""  -- a leading newline makes the output of Tasty more readable
      , "Check failed! Passing checks:"
      , printProgress (failureProgress f)
      , "Failing check:"
      , Text.unwords
        [ "❌"
        , funcName (failureFailedFunc f)
        , "on line"
        -- TODO: translate lua line
        , Text.pack (show (FCP.getLuaLine (failureLuaLine f)))
        , "did not match text at"
        , FCP.printLoc (progressLoc (failureProgress f))
        ]
      , "text:"
      , indentLines (progressRemainder (failureProgress f))
      ]

instance X.Exception Failure

-- | The result of matching a program against some text.
data Success
 = Success
   { -- | t'Loc' after the last match
     successLoc :: Loc
     -- | Successful 'Match'es
   , successMatches :: Seq Match
     -- | Remaining text after the last match
   , successRemainder :: Text
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
