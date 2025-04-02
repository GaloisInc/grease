{-# LANGUAGE OverloadedStrings #-}

module FileCheck
  ( Output(..)
  , FCD.Directive(..)
  , FCC.Command(..)
  , FileCheckFailure(..)
  , check
  , check'
  , CommandParseFailure
  , parseCommentsAndCheck
  , parseCommentsAndCheck'
  , Loc(..)
  , Pos(..)
  ) where

import Control.Exception qualified as X
import Control.Monad qualified as Monad
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text (Text)
import FileCheck.Command (Command)
import FileCheck.Command qualified as FCC
import FileCheck.Directive qualified as FCD
import FileCheck.Directive (Prefix)
import FileCheck.Pos qualified as FCP
import FileCheck.Pos (Loc(..), Pos(..))
import GHC.Stack (HasCallStack)
import Prelude hiding (lines)

-- | Output of the program under test
newtype Output = Output Text

data FileCheckFailure = FileCheckFailure Command Text

instance Show FileCheckFailure where
  show (FileCheckFailure (FCC.Command d c loc) t) =
    Text.unpack $
      Text.unlines
      [ mconcat
        [ case loc of
            Just l -> FCP.printLoc l <> ": "
            Nothing -> mempty
        , FCD.print d <> ": " <> c
        ]
      , "does not match"
      , t
      ]

instance X.Exception FileCheckFailure

-- | Match text against a sequence of 'Command's.
check ::
  [Command] ->
  Output ->
  Either FileCheckFailure ()
check cmds (Output txt0) =
  Monad.void $ Monad.foldM go txt0 cmds
  where
  go txt cmd =
    case FCC.match cmd txt of
      Nothing -> Left (FileCheckFailure cmd txt)
      Just txt' -> Right txt'

-- | Like 'check', but throws 'FileCheckFailure' on failure.
check' ::
  HasCallStack =>
  [Command] ->
  Output ->
  IO ()
check' cmds out =
  case check cmds out of
    Left failure -> X.throwIO failure
    Right () -> pure ()

data CommandParseFailure = CommandParseFailure Text
  deriving Show

instance X.Exception CommandParseFailure

-- | Parse 'Command's from lines of text, and use them to check an 'Output'.
--
-- Throws 'CommandParseFailure' if the commands cannot be parsed.
parseAndCheck ::
  HasCallStack =>
  Maybe Prefix ->
  -- | Name of the file from which the lines come
  Maybe FilePath ->
  -- | Lines representing commands (together with their line numbers)
  [(Int, Text)] ->
  Output ->
  Either FileCheckFailure ()
parseAndCheck pfx fileName cmdsTxt out =
  let mkLoc lineNo = Just (Loc fileName (Pos lineNo 0)) in
  let cmds = Maybe.mapMaybe (\(no, l) -> FCC.parse pfx (mkLoc no) l) cmdsTxt
  in check cmds out

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
  Either FileCheckFailure ()
parseCommentsAndCheck pfx comment fileName cmdsTxt out =
  let lines = Text.lines cmdsTxt in
  let cmds = Maybe.mapMaybe (Text.stripPrefix comment) lines in
  parseAndCheck pfx fileName (zip [0..] cmds) out


-- | Like 'parseCommentsAndCheck', but throws 'FileCheckFailure' on failure. 
parseCommentsAndCheck' ::
  HasCallStack =>
  Maybe Prefix ->
  -- | Name of the file from which the text comes
  Maybe FilePath ->
  Text ->
  Text ->
  Output ->
  IO ()
parseCommentsAndCheck' pfx fileName comment cmdsTxt out =
  case parseCommentsAndCheck pfx comment fileName cmdsTxt out of
    Left failure -> X.throwIO failure
    Right () -> pure ()
