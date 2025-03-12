{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Diagnostic.Severity
  ( Severity(..)
  , natToSeverity
  , severityToNat
  ) where

import Numeric.Natural (Natural)

data Severity
  = Error
  | Warn
  | Info
  | Debug
  | Trace
  deriving (Eq, Ord, Show)

natToSeverity :: Natural -> Severity
natToSeverity 0 = Error
natToSeverity 1 = Warn
natToSeverity 2 = Info
natToSeverity 3 = Debug
natToSeverity _ = Trace

severityToNat :: Severity -> Natural
severityToNat Error = 0
severityToNat Warn = 1
severityToNat Info = 2
severityToNat Debug = 3
severityToNat Trace = 4
