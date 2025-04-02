{-# LANGUAGE OverloadedStrings #-}

module FileCheck
  ( Prefix(..)
  , Output(..)
  , FCD.Directive(..)
  , FCC.Command(..)
  , FileCheckFailure(..)
  , Result(..)
  , printResult
  , check
  , check'
  , CommandParseFailure
  , parseCommentsAndCheck
  , parseCommentsAndCheck'
  , Loc(..)
  , Pos(..)
  , FCP.Span(..)
  , FCC.Match(..)
  ) where

import Control.Exception qualified as X
import Data.Foldable (foldl', toList)
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Text qualified as Text
import Data.Text (Text)
import FileCheck.Command (Command, Match)
import FileCheck.Command qualified as FCC
import FileCheck.Directive (Prefix)
import FileCheck.Directive qualified as FCD
import FileCheck.Pos (Loc(..), Pos(..))
import FileCheck.Pos qualified as FCP
import GHC.Stack (HasCallStack)
import Prelude hiding (lines, span)

-- | Output of the program under test
newtype Output = Output Text

data FileCheckFailure
  = FileCheckFailure
    { -- | All 'Command's that were to be checked
      failureCommands :: [Command]
      -- | The 'Command' that did not match
    , failureFailedCommand :: !Command
    , failureResult :: !Result
    }

indent :: Text
indent = "  "

indentLines :: Text -> Text
indentLines = Text.unlines . map (indent <>) . Text.lines

printMatches :: [(Command, Match)] -> Text
printMatches =
  Text.unlines . map (uncurry printMatch)
  where
    printMatch :: Command -> Match -> Text
    printMatch c m =
      Text.unlines
      [ Text.unwords
        [ "✔️"
        , FCD.print (FCC.cmdDirective c)
        , "at"
        , maybe "<unknown location>" FCP.printSpan (FCC.cmdSpan c)
        , "matched text at"
        , FCP.printSpan (FCC.matchSpan m)          
        ]
      , "pattern:"
      , indent <> FCC.cmdContent c
      , "match:"
      , indentLines (FCC.matchText m)
      ]

instance Show FileCheckFailure where
  show f =
    let r = failureResult f in
    Text.unpack $
      Text.unlines
      [ ""  -- a leading newline makes the output of Tasty more readable
      , "Check failed! Passing checks:"
      , printMatches (zip (failureCommands f) (toList (resultMatches r)))
      , "Failing check:"
      , Text.unwords
        [ "❌"
        , FCD.print (FCC.cmdDirective (failureFailedCommand f))
        , "at"
        , maybe "<unknown location>" FCP.printSpan (FCC.cmdSpan (failureFailedCommand f))
        , "did not match text at"
        , FCP.printLoc (resultLocation r)
        ]
      , "pattern:"
      , indent <> FCC.cmdContent (failureFailedCommand f)
      , "text:"
      , indentLines (resultRemainder r)
      ]

instance X.Exception FileCheckFailure

data Result
  = Result
    { -- | 'Loc' after the last match
      resultLocation :: Loc
      -- | Successful 'Matches'
    , resultMatches :: Seq Match
      -- | Remaining text after the last match
    , resultRemainder :: Text
    }

printResult ::
  [Command] ->
  Result ->
  Text
printResult cmds r =
  let matches = resultMatches r in
  case drop (length matches) cmds of
    [] -> printMatches (zip cmds (toList matches))
    (failed : _) -> Text.pack (show (FileCheckFailure cmds failed r))

-- | Match text against a sequence of 'Command's.
check ::
  [Command] ->
  Output ->
  Result
check cmds (Output txt0) =
  let loc0 = Loc (Just "<out>") (FCP.Pos 1 1) in
  case foldl' go (Right (Result loc0 Seq.empty txt0)) cmds of
    Left r -> r
    Right r -> r
  where
  go (Right (Result loc ms txt)) cmd =
    case FCC.match cmd loc txt of
      Nothing -> Left (Result loc ms txt)
      Just m ->
        let loc' = FCP.endLoc (FCC.matchSpan m) in
        Right (Result loc' (ms Seq.:|> m) (FCC.matchRemainder m))
  go (Left r) _cmd = Left r

-- | Helper, not exported
checkResult ::
  [Command] ->
  Result ->
  IO ([Command], Result)
checkResult cmds r =
  let matches = resultMatches r in
  case drop (length matches) cmds of
    [] -> pure (cmds, r)
    (failed : _) -> X.throwIO (FileCheckFailure cmds failed r)

-- | Like 'check', but throws 'FileCheckFailure' on failure.
check' ::
  HasCallStack =>
  [Command] ->
  Output ->
  IO Result
check' cmds out = snd <$> checkResult cmds (check cmds out)

data CommandParseFailure = CommandParseFailure Text
  deriving Show

instance X.Exception CommandParseFailure

-- | Parse 'Command's from comments embedded in a file, and use them to check
-- an 'Output'.
--
-- Throws 'CommandParseFailure' if the commands cannot be parsed.
parseCommentsAndCheck ::
  HasCallStack =>
  Maybe Prefix ->
  -- | Start of line comment
  Text ->
  -- | Name of the file from which the text comes
  Maybe FilePath ->
  -- | Text containing comments with embedded commands
  Text ->
  Output ->
  ([Command], Result)
parseCommentsAndCheck pfx comment fileName cmdsTxt out =
  let lines = zip [1..] (Text.lines cmdsTxt) in
  let mkLoc lineNo = Just (FCP.Loc fileName (Pos lineNo 1)) in
  let cmds = Maybe.mapMaybe (\(no, l) -> FCC.parse pfx comment (mkLoc no) l) lines
  in (cmds, check cmds out)


-- | Like 'parseCommentsAndCheck', but throws 'FileCheckFailure' on failure. 
parseCommentsAndCheck' ::
  HasCallStack =>
  Maybe Prefix ->
  Text ->
  -- | Name of the file from which the text comes
  Maybe FilePath ->
  -- | Text containing comments with embedded commands
  Text ->
  Output ->
  IO ([Command], Result)
parseCommentsAndCheck' pfx comment fileName cmdsTxt out =
  let (cmds, r) = parseCommentsAndCheck pfx comment fileName cmdsTxt out in
  checkResult cmds r
