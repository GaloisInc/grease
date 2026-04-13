{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostics for reachability verification
module Screach.Verify.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Info, Warn))
import Prettyprinter qualified as PP

data Diagnostic
  = VerifyReachable
      -- | Total number of results
      Int
      -- | Current result number
      Int
  | VerifySuccess
  | VerifyFailure
  deriving (Show)

instance PP.Pretty Diagnostic where
  pretty =
    \case
      VerifyReachable total cur ->
        PP.hsep
          [ "Verifying reachability of result"
          , PP.pretty cur
          , "/"
          , PP.pretty total
          ]
      VerifySuccess -> "Verified reachability"
      VerifyFailure -> "Failed to verify reachability!"

severity :: Diagnostic -> Severity
severity =
  \case
    VerifyFailure -> Warn
    _ -> Info
