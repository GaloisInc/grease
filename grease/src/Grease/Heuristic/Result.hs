{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Heuristic.Result (
  CantRefine (..),
  HeuristicResult (..),
  mergeResultsOptimistic,
) where

import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Grease.Bug qualified as Bug
import Grease.Shape (ArgShapes)
import Grease.Shape.NoTag (NoTag)
import Prettyprinter qualified as PP

-- | There is no way to proceed with refinement, for some explicit reason
data CantRefine
  = MissingFunc (Maybe String)
  | MissingSemantics Text
  | MutableGlobal String
  deriving (Generic, Show)

instance Aeson.ToJSON CantRefine

instance PP.Pretty CantRefine where
  pretty =
    \case
      MissingFunc (Just nm) -> "Missing implementation for '" PP.<> PP.pretty nm PP.<> "'"
      MissingFunc Nothing -> "Missing implementation for function"
      MissingSemantics msg -> PP.pretty msg
      MutableGlobal name -> "Load from mutable global " PP.<> PP.pretty name

data HeuristicResult ext tys
  = CantRefine CantRefine
  | PossibleBug Bug.BugInstance
  | RefinedPrecondition (ArgShapes ext NoTag tys)
  | Unknown

-- | Merge two 'HeuristicResult's, preferring the first except if it is
-- 'CantRefine' or 'Unknown', in which case use the second.
--
-- This is useful for heuristics for @memcpy@, which applies heuristics to its
-- source and destination pointers separately, then merges them.
mergeResultsOptimistic ::
  HeuristicResult ext tys ->
  HeuristicResult ext tys ->
  HeuristicResult ext tys
mergeResultsOptimistic r1 r2 =
  case (r1, r2) of
    (CantRefine{}, _) -> r2
    (PossibleBug{}, _) -> r1
    (RefinedPrecondition{}, _) -> r1
    (Unknown{}, _) -> r2
