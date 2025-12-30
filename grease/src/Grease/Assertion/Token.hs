module Grease.Assertion.Token (Token (..)) where

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

stringToNat :: String -> Int
stringToNat s = undefined
