-- | See the package README for a high-level description.
module FileCheck
  ( Output(..)
  , Loc(..)
  , Pos(..)
  , FCP.Span(..)
  , FCC.Match(..)
  , LuaCode(..)
  , check
  , parseCommentsAndCheck
  , parseCommentsAndCheck'
  , FCR.Failure(..)
  , FCR.Progress(..)
  , FCR.Success(..)
  , FCR.Result(..)
  , FCR.resultNull
  , FCR.printResult
  ) where

import Control.Exception qualified as X
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import FileCheck.Command qualified as FCC
import FileCheck.LuaApi qualified as FCLA
import FileCheck.Pos (Loc(..), Pos(..))
import FileCheck.Pos qualified as FCP
import GHC.Stack (HasCallStack)
import Prelude hiding (lines, span)
import qualified FileCheck.Result as FCR

-- | Output of the program under test
newtype Output = Output Text

newtype LuaCode = LuaCode Text

check ::
  LuaCode ->
  Output ->
  IO FCR.Result
check (LuaCode codeTxt) (Output out) = FCLA.check codeTxt out

parseCommentsAndCheck ::
  HasCallStack =>
  -- | Start of line comment
  Text ->
  -- | Text containing comments with embedded 
  Text ->
  Output ->
  IO FCR.Result
parseCommentsAndCheck comment txt out =
  let lua = Maybe.mapMaybe (Text.stripPrefix comment) (Text.lines txt) in
  check (LuaCode (Text.unlines lua)) out

parseCommentsAndCheck' ::
  HasCallStack =>
  -- | Start of line comment
  Text ->
  -- | Text containing comments with embedded 
  Text ->
  Output ->
  IO ()
parseCommentsAndCheck' comment txt out = do
  FCR.Result r <- parseCommentsAndCheck comment txt out
  case r of
    Left f -> X.throwIO f
    Right {} -> pure ()
