{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Setup.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Grease.Diagnostic.Severity (Severity (Trace))
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Prettyprinter qualified as PP
import What4.Interface qualified as W4

data Diagnostic where
  SetupMem ::
    forall sym.
    W4.IsExpr (W4.SymExpr sym) =>
    CLM.MemImpl sym ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      SetupMem mem ->
        -- This printout starts with "Base memory", which is fairly
        -- descriptive, so we don't add additional text here.
        CLM.ppMem (CLM.memImplHeap mem)

severity :: Diagnostic -> Severity
severity =
  \case
    SetupMem{} -> Trace
