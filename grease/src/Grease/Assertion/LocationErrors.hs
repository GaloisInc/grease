module Grease.Assertion.LocationErrors (Located (..)) where

import Error.Diagnose.Position qualified as Pos

-- From diagnose, annotates a with a position
-- that can be used to emit errors
data Located a
  = a :@ Pos.Position
  deriving (Show, Eq, Ord, Functor)
