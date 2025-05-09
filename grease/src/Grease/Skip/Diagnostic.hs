{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Skip.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Prettyprinter qualified as PP

-- what4
import What4.FunctionName qualified as W4

import Grease.Diagnostic.Severity (Severity(Debug))

data Diagnostic where
  FunctionCall ::
    W4.FunctionName
      {- ^ The function name -} ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      FunctionCall fnName ->
        "Invoking the" PP.<+> PP.squotes (PP.pretty fnName) PP.<+> "function"

severity :: Diagnostic -> Severity
severity =
  \case
    FunctionCall{} -> Debug
