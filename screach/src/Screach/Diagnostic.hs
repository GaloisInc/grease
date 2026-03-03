{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functionality for logging diagnostic messages in @screach@.
module Screach.Diagnostic (
  Diagnostic (..),
  ScreachLogAction,
  log,
  severity,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Time.Clock qualified as Time
import Data.Time.Format qualified as Time
import Grease.Diagnostic qualified as Grease
import Grease.Diagnostic.Severity (Severity (Debug, Warn))
import Lumberjack qualified as LJ
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Screach.Distance.Diagnostic qualified as Distance
import Screach.GoalEvaluator.Diagnostic qualified as GoalEvaluator
import Screach.Run.Diagnostic qualified as Run
import System.IO (stderr)
import What4.ProgramLoc qualified as WPL
import Prelude hiding (log)

-- | A diagnostic message that @screach@ can emit.
--
-- Diagnostics for each module are defined in a sub-module. Each diagnostic
-- type implements 'PP.Pretty'.
data Diagnostic where
  GoalEvaluatorDiagnostic :: GoalEvaluator.Diagnostic -> Diagnostic
  GreaseDiagnostic :: Grease.Diagnostic -> Diagnostic
  RunDiagnostic :: Run.Diagnostic -> Diagnostic
  DistanceDiagnostic :: Distance.Diagnostic -> Diagnostic
  ScheduledSuccessor :: WPL.ProgramLoc -> WPL.ProgramLoc -> Int -> Diagnostic
  ExecutingFrame :: WPL.ProgramLoc -> String -> Diagnostic
  ResumingFrame :: WPL.ProgramLoc -> Diagnostic
  AttemptedToReturnViaCallgraphForNonAddressFunction :: WPL.ProgramLoc -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      GoalEvaluatorDiagnostic diag -> PP.pretty diag
      GreaseDiagnostic diag -> PP.pretty diag
      RunDiagnostic diag -> PP.pretty diag
      DistanceDiagnostic diag -> PP.pretty diag
      ScheduledSuccessor loc fromLoc dist ->
        PP.hsep ["Scheduled:", PP.viaShow loc, "from", PP.viaShow fromLoc, "at distance", PP.pretty dist]
      ExecutingFrame loc stType -> PP.hsep ["Executing frame:", PP.viaShow loc, PP.pretty stType]
      ResumingFrame loc -> PP.hsep ["Resuming frame:", PP.viaShow loc]
      AttemptedToReturnViaCallgraphForNonAddressFunction loc ->
        PP.hsep
          [ "Attempted to return from function via the callgraph (because the symbolic execution stack was empty) during sdse:"
          , PP.viaShow loc
          ]

severity :: Diagnostic -> Severity
severity =
  \case
    GoalEvaluatorDiagnostic diag -> GoalEvaluator.severity diag
    GreaseDiagnostic diag -> Grease.severity diag
    RunDiagnostic diag -> Run.severity diag
    DistanceDiagnostic diag -> Distance.severity diag
    ScheduledSuccessor{} -> Debug
    ExecutingFrame _ _ -> Debug
    ResumingFrame{} -> Debug
    AttemptedToReturnViaCallgraphForNonAddressFunction{} -> Warn

-- | The type of @screach@ 'LJ.LogAction's. This must work over any 'MonadIO'
-- instance to ensure that we can log messages in multiple monads, including
-- 'IO' and @'StateT' s 'IO'@.
type ScreachLogAction = forall m. MonadIO m => LJ.LogAction m Diagnostic

-- | Log a message to 'stderr' along with the current time.
log :: MonadIO m => PP.Doc a -> m ()
log msg = do
  t <- liftIO Time.getCurrentTime
  let time = Time.formatTime Time.defaultTimeLocale "[%F %T]" t
  liftIO $ PP.hPutDoc stderr $ PP.pretty time PP.<+> msg
  liftIO $ PP.hPutDoc stderr PP.line
