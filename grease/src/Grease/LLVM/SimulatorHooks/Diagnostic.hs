{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.SimulatorHooks.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Void (Void, absurd)
import Grease.Diagnostic.Severity (Severity (Debug, Info))
import Prettyprinter qualified as PP

data Diagnostic where
  SkippedSymbolicFnHandleCall :: Diagnostic
  Stmt :: PP.Doc Void -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty d =
    case d of
      SkippedSymbolicFnHandleCall ->
        "Skipped call to a symbolic function handle"
      Stmt s ->
        "Executing statement:" PP.<+> fmap absurd s

severity :: Diagnostic -> Severity
severity =
  \case
    SkippedSymbolicFnHandleCall{} -> Info
    Stmt{} -> Debug
