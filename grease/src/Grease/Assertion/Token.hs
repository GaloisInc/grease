module Grease.Assertion.Token (Token (..), AnnotatedToken (..), hexStringToNat) where

import Data.Word (Word64)

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
  | Bv
  | Eof

data AnnotatedToken = AnnotatedToken
  { lineNumber :: Int
  , columnNumber :: Int
  , size :: Int
  , token :: Token
  }

hexStringToNat :: String -> Int
hexStringToNat s = undefined
