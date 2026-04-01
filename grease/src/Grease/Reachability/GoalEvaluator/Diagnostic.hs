{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.GoalEvaluator.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Macaw.Memory qualified as MM
import Grease.Diagnostic.Severity (Severity (Debug, Info))
import Grease.Reachability.AnalysisLoc (ResolvedTargetLoc)
import Prettyprinter qualified as PP
import What4.SatResult qualified as W4

data Diagnostic where
  ReachedTarget ::
    MM.MemWidth w =>
    -- | The resolved target location
    ResolvedTargetLoc w ->
    -- | Result of checking satisfiability at the target
    W4.SatResult m c ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      ReachedTarget rtLoc result ->
        let ppResult :: PP.Doc a
            ppResult = case result of
              W4.Unsat{} -> "unsat"
              W4.Sat{} -> "sat"
              W4.Unknown -> "unknown"
         in "Reached target" PP.<+> PP.pretty rtLoc PP.<+> PP.parens ppResult

severity :: Diagnostic -> Severity
severity =
  \case
    ReachedTarget _ result ->
      case result of
        -- We only print target reachability by default if the path to the
        -- target is satisfiable. For unsatisfiable or unknown paths to the
        -- target, you must increase the verbosity level.
        W4.Sat{} -> Info
        W4.Unsat{} -> Debug
        W4.Unknown -> Debug
