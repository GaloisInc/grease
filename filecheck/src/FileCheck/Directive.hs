{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module FileCheck.Directive
  ( Prefix(..)
  , Directive(..)
  , parse
  , print
  ) where

import Data.Text (Text)
import qualified Data.Maybe as Maybe
import Prelude hiding (print)

-- | @-check-prefix@
newtype Prefix = Prefix Text

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

parse :: Maybe Prefix -> Text -> Maybe Directive
parse pfx = parseWithPrefix (Maybe.fromMaybe (Prefix "CHECK") pfx)

print :: Directive -> Text
print =
  \case
    Check -> "CHECK"
    CheckEmpty -> "CHECK-EMPTY"
    CheckNext -> "CHECK-NEXT"
    CheckNot -> "CHECK-NOT"
    Com -> "COM"
