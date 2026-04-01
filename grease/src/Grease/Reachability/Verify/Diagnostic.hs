{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostics for reachability verification
--
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Reachability.Verify.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Info, Warn))
import Prettyprinter qualified as PP

data Diagnostic
  = -- | "Reached target!"
    ReachabilityReach
  | -- | "Verifying reachability of result N / M"
    VerifyReachable
      -- | Total number of results
      Int
      -- | Current result number
      Int
  | -- | "Verified reachability"
    VerifySuccess
  | -- | "Failed to verify reachability!"
    VerifyFailure
  deriving (Show)

instance PP.Pretty Diagnostic where
  pretty =
    \case
      ReachabilityReach -> "Reached target!"
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
