{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.SetupHook.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Warn))
import Prettyprinter qualified as PP
import What4.FunctionName qualified as WFN

data Diagnostic where
  CantSkip ::
    WFN.FunctionName -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      CantSkip fNm ->
        "Can't skip" PP.<+> PP.pretty fNm

severity :: Diagnostic -> Severity
severity =
  \case
    CantSkip{} -> Warn
