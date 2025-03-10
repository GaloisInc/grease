{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}

module Grease.Setup.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import qualified Prettyprinter as PP

import qualified Lang.Crucible.LLVM.MemModel as Mem

import Grease.Diagnostic.Severity (Severity(Trace))
import qualified What4.Interface as W4

data Diagnostic where
  SetupMem ::
    forall sym.
    W4.IsExpr (W4.SymExpr sym) =>
    Mem.MemImpl sym ->
    Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      SetupMem mem ->
        -- This printout starts with "Base memory", which is fairly
        -- descriptive, so we don't add additional text here.
        Mem.ppMem (Mem.memImplHeap mem)

severity :: Diagnostic -> Severity
severity =
  \case
    SetupMem{} -> Trace
