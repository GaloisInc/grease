{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Screach.Run.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Macaw.Memory qualified as MM
import Data.Text (Text)
import Data.Void (Void, absurd)
import Grease.Diagnostic.Severity (Severity (Error, Info, Warn))
import Prettyprinter qualified as PP
import Screach.AnalysisLoc (ResolvedTargetLoc)

data Diagnostic where
  DebuggerOutput ::
    PP.Doc Void ->
    Diagnostic
  MalformedElf ::
    PP.Doc Void ->
    Diagnostic
  SearchingForTarget ::
    MM.MemWidth w =>
    -- | The resolved target location
    ResolvedTargetLoc w ->
    Diagnostic
  RefinementReach ::
    PP.Doc Void ->
    Diagnostic
  RefinementSuccess ::
    Diagnostic
  RefinementNoHeuristic ::
    Diagnostic
  RefinementItersExceeded ::
    Diagnostic
  RefinementCantRefine ::
    Diagnostic
  RefinementStateTimedOut ::
    Diagnostic
  RefinementPathAborted ::
    -- | Short description of why the path was aborted
    Text ->
    Diagnostic
  RefinementBug ::
    Diagnostic
  RefinementResultCount ::
    -- | The number of refinement results.
    Int ->
    Diagnostic
  VerifyFailure ::
    Diagnostic
  VerifyReachable ::
    -- | Total number of results
    Int ->
    -- | Current result number
    Int ->
    Diagnostic
  VerifySuccess ::
    Diagnostic
  UserError ::
    PP.Doc Void ->
    Diagnostic
  Unsupported ::
    PP.Doc Void ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      DebuggerOutput out -> fmap absurd out
      MalformedElf e -> "Malformed ELF file: " <> fmap absurd e
      SearchingForTarget rtLoc ->
        "Searching for target" PP.<+> PP.pretty rtLoc
      RefinementReach prettyArgs ->
        PP.vcat
          [ "Reached target!"
          , absurd <$> prettyArgs
          ]
      RefinementSuccess ->
        failedToReach "Refinement success"
      RefinementNoHeuristic ->
        failedToReach "Refinement failure"
      RefinementItersExceeded ->
        failedToReach "Refinement iters exceeded"
      RefinementCantRefine ->
        failedToReach "Can't refine"
      RefinementStateTimedOut ->
        failedToReach "Can't refine due to timeout"
      RefinementPathAborted reason ->
        "Execution path aborted:" PP.<+> PP.pretty reason
      RefinementBug ->
        failedToReach "Found likely bug"
      RefinementResultCount numResults ->
        "Result count:" PP.<+> PP.pretty numResults
      VerifyFailure -> "Failed to verify reachability!"
      VerifyReachable total cur ->
        PP.hsep
          [ "Verifying reachability of result"
          , PP.pretty cur
          , "/"
          , PP.pretty total
          ]
      VerifySuccess -> "Verified reachability"
      UserError e -> "User error:" PP.<+> fmap absurd e
      Unsupported e -> "Not yet supported:" PP.<+> fmap absurd e
   where
    failedToReach :: PP.Doc a -> PP.Doc a
    failedToReach reason = "Failed to reach target!" PP.<+> PP.parens reason

severity :: Diagnostic -> Severity
severity =
  \case
    DebuggerOutput{} -> Info
    MalformedElf{} -> Error
    SearchingForTarget{} -> Info
    RefinementReach{} -> Info
    RefinementSuccess{} -> Info
    RefinementNoHeuristic{} -> Info
    RefinementItersExceeded{} -> Info
    RefinementCantRefine{} -> Info
    RefinementStateTimedOut{} -> Info
    RefinementPathAborted{} -> Info
    RefinementBug{} -> Info
    RefinementResultCount{} -> Info
    VerifyFailure{} -> Warn
    VerifyReachable{} -> Info
    VerifySuccess{} -> Info
    UserError{} -> Error
    Unsupported{} -> Error
