-- | See the package README for a high-level description.
module FileCheck
  ( FCE.LuaProgram
  , FCE.addPrefix
  , FCE.plainLuaProgram
  , FCE.fromLines
  , FCE.fromLineComments
  , Output(..)
  , Loc(..)
  , Pos(..)
  , FCP.Span(..)
  , check
  , check'
  , FCR.Failure(..)
  , FCR.Progress(..)
  , FCR.Success(..)
  , FCR.Result(..)
  , FCR.resultNull
  , FCR.printResult
  ) where

import Control.Exception qualified as X
import Data.ByteString (ByteString)
import FileCheck.Extract (LuaProgram)
import FileCheck.Extract qualified as FCE
import FileCheck.LuaApi qualified as FCLA
import FileCheck.Pos (Loc(..), Pos(..))
import FileCheck.Pos qualified as FCP
import FileCheck.Result qualified as FCR
import GHC.Stack (HasCallStack)
import Prelude hiding (lines, span)

-- | Output of the program under test
newtype Output = Output ByteString

-- | Check some program output against a FileCheck Lua program.
check ::
  LuaProgram ->
  Output ->
  IO FCR.Result
check prog (Output out) = FCLA.check prog out

-- | Like 'check', but throws an exception on failure.
check' ::
  HasCallStack =>
  LuaProgram ->
  Output ->
  IO ()
check' prog out = do
  FCR.Result r <- check prog out
  case r of
    Left f -> X.throwIO f
    Right {} -> pure ()
