{-# LANGUAGE PatternSynonyms #-}

module Grease.Assertion.Token (Token (..), AnnotatedToken (..), parseNat, parseBv, pattern AnnotatedBool) where

import Data.BitVector.Sized (BV, mkBV)
import Data.Maybe qualified as Maybe
import Data.Parameterized qualified as Param
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Word (Word64)
import Error.Diagnose.Position qualified as Pos
import GHC.Natural (Natural)
import Lang.Crucible.Syntax.Monad (position)

data TypedBv w = TypedBv {bvValue :: BV w, bvWidth :: NatRepr.NatRepr w}
data Token
  = LParen
  | RParen
  | TokenBool Bool
  | SepConj
  | Comma
  | TypeOf
  | Zext
  | Trunc
  | Lt
  | Lte
  | Slt
  | Slte
  | Eq
  | Exclam
  | Ite
  | LabelVar String
  | ExistentialVar String
  | ProgramVar String
  | NaturalNum Int
  | BvValue (Param.Some TypedBv)
  | BvTypeName
  | Eof

data AnnotatedToken = AnnotatedToken
  { token :: Token
  , position :: Pos.Position
  }

pattern AnnotatedBool :: Bool -> Pos.Position -> AnnotatedToken
pattern AnnotatedBool b pos <- AnnotatedToken (TokenBool b) pos

-- These functions should only be used by Alex actions
-- so the lexer form guarentees existence

splitStringOn :: String -> Char -> Maybe (String, String)
splitStringOn [] _ = Nothing
splitStringOn (c : rst) on =
  if c == on
    then
      Just ("", rst)
    else
      let rst' = splitStringOn rst on
       in (\(tot, end) -> (c : tot, end)) <$> rst'

parseBv :: String -> Param.Some TypedBv
parseBv s =
  let (valueStr, widthStr) = Maybe.fromJust $ splitStringOn s ':'
      wNat = parseNat widthStr
      w = NatRepr.mkNatRepr wNat
   in case w of
        Param.Some x -> Param.Some $ TypedBv{bvValue = read valueStr, bvWidth = x}

parseNat :: String -> Natural
parseNat s = read s
