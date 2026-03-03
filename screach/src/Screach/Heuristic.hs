{-# LANGUAGE OverloadedStrings #-}

module Screach.Heuristic (
  reachedHeuristic,
  isReachedBug,
) where

import Control.Lens (to, (^.))
import Data.Functor.Const (Const)
import Data.Parameterized.Context qualified as Ctx
import Data.Text qualified as Text
import Grease.Bug qualified as Bug
import Grease.Heuristic qualified as GH
import Grease.Setup qualified as GS
import Grease.Setup.Annotations qualified as GSA
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag qualified as Shape
import Lang.Crucible.Backend qualified as CB
import Screach.GoalEvaluator qualified as SGE
import What4.LabeledPred as WLP

-- | A GREASE refinement heuristic that works in concert with
-- "Screach.GoalEvaluator" to catch when Screach has reached a target.
reachedHeuristic ::
  bak ->
  GSA.Annotations sym ext tys ->
  GS.InitialMem sym ->
  CB.ProofObligation sym ->
  Maybe (GH.ErrorDescription sym) ->
  -- | Argument names
  Ctx.Assignment (Const String) tys ->
  Shape.ArgShapes ext Shape.NoTag tys ->
  IO (GH.HeuristicResult ext tys)
reachedHeuristic _bak _anns _initMem goal _errDesc _argNames _argShapes = do
  let simErr = goal ^. to CB.proofGoal ^. WLP.labeledPredMsg
  if SGE.isReachedTargetError simErr
    then
      pure $
        GH.PossibleBug $
          Bug.BugInstance
            { -- HACK: We reuse the `MustFail` bug type, because Screach doesn't
              -- use the must-fail heuristic from GREASE. The important thing
              -- is that Screach must later be able to recognize this bug as
              -- indicating that the target was reached, which it does via
              -- `isReachedBug` below.
              Bug.bugType = Bug.MustFail
            , Bug.bugLoc = "unknown"
            , Bug.bugUb = Nothing
            , Bug.bugDetails = Just (Text.pack SGE.reachedTargetMsg)
            }
    else pure GH.Unknown

isReachedBug :: Bug.BugInstance -> Bool
isReachedBug b@(Bug.BugInstance{Bug.bugType = Bug.MustFail}) =
  Bug.bugDetails b == Just (Text.pack SGE.reachedTargetMsg)
isReachedBug _ = False
