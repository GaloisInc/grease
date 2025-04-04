{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module FileCheck.Directive
  ( Prefix(..)
  , Directive(..)
  , parse
  , printDirective
  ) where

import Data.Text (Text)
import qualified Data.Maybe as Maybe

-- | See LLVM FileCheck\'s @-check-prefix@ flag.
newtype Prefix = Prefix Text

-- | These directives are inspired by LLVM FileCheck, see upstream docs (and the
-- test suite) for details on what they check and how they check it.
data Directive
  = -- | @CHECK@
    Check
  | -- | @CHECK-EMPTY@
    CheckEmpty
  | -- | @CHECK-NEXT@
    CheckNext
  | -- | @CHECK-NOT@
    CheckNot
  | -- | @COM@ (comment)
    Com

parseWithPrefix :: Prefix -> Text -> Maybe Directive
parseWithPrefix (Prefix pfx) t =
  if | t == pfx -> Just Check
     | t == pfx <> "-EMPTY" -> Just CheckEmpty
     | t == pfx <> "-NEXT" -> Just CheckNext
     | t == pfx <> "-NOT" -> Just CheckNot
     | t == "COM" -> Just Com
     | otherwise -> Nothing

-- | Parse a 'Directive'. Returns 'Nothing' on parse failure.
parse :: Maybe Prefix -> Text -> Maybe Directive
parse pfx = parseWithPrefix (Maybe.fromMaybe (Prefix "CHECK") pfx)

-- | Print a 'Directive'. Conceptually the inverse of 'parse'.
printDirective :: Directive -> Text
printDirective =
  \case
    Check -> "CHECK"
    CheckEmpty -> "CHECK-EMPTY"
    CheckNext -> "CHECK-NEXT"
    CheckNot -> "CHECK-NOT"
    Com -> "COM"
