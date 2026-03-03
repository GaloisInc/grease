-- Note that this is partially copy/pasted from Grease.Refine.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Screach.RefinementOptions (
  AllSolutions (..),
  RefineReplay (..),
) where

-- | Whether to follow the previously explored trace when refining
newtype RefineReplay = RefineReplay {getRefineReplay :: Bool}
  -- See Note [Derive Read/Show instances with the newtype strategy]
  deriving newtype (Read)

-- | Whether to search exhaustively for reachability solutions or stop when the first is found
newtype AllSolutions = AllSolutions {getAllSolutions :: Bool}
  -- See Note [Derive Read/Show instances with the newtype strategy]
  deriving newtype (Read)

{-
Note [Derive Read/Show instances with newtype strategy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Make sure to derive the Read instance for option-related newtypes using the
`newtype` strategy, not the `stock` strategy. For instance, we parse
ExtraStackSlots values in the command-line parser using the `Read` instance, and
we want users to be able to write `--stack-argument-slots 1` instead of the much
more verbose `--stack-argument-slots "ExtraStackSlots{getExtraStackSlots = 1}"`.

Similar considerations apply for derived Show instances, which also have
different behavior when `stock`-derived.
-}
