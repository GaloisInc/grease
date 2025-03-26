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

import qualified Prettyprinter as PP

-- what4
import qualified What4.FunctionName as W4

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
