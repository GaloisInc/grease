{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.ResolveCall.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Data.Macaw.Memory qualified as MM
import Data.Text (Text)
import Grease.Diagnostic.Severity (Severity (Debug, Info))
import Grease.Macaw.SkippedCall qualified as Skip
import Prettyprinter qualified as PP
import What4.FunctionName qualified as W4

data Diagnostic where
  PltCall ::
    MM.MemWidth w =>
    -- | The name of the PLT stub
    W4.FunctionName ->
    -- | The address of the PLT stub
    MM.MemSegmentOff w ->
    -- | The address that the PLT call jumps to
    MM.MemSegmentOff w ->
    Diagnostic
  FunctionCall ::
    MM.MemWidth w =>
    -- | The function name
    W4.FunctionName ->
    -- | The address where the function is defined
    MM.MemWord w ->
    -- | The address that the function returns to (if known)
    Maybe (MM.MemWord w) ->
    Diagnostic
  FunctionOverride ::
    -- | The overridden function's name
    W4.FunctionName ->
    Diagnostic
  SyscallOverride ::
    -- | Syscall name
    Text ->
    -- | Syscall number
    Int ->
    Diagnostic
  SkippedFunctionCall ::
    Skip.SkippedFunctionCall arch -> Diagnostic
  SkippedSyscall ::
    Skip.SkippedSyscall -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty d =
    case d of
      PltCall pltStubName pltStubAddr pltCallAddr ->
        "Calling a PLT stub"
          PP.<+> PP.parens (PP.pretty pltStubName)
          PP.<+> "at address"
          PP.<+> PP.pretty pltStubAddr
          PP.<> ", which jumps to address"
          PP.<+> PP.pretty pltCallAddr
      FunctionCall fnName fnAddr mbRetAddr ->
        "Invoking the"
          PP.<+> PP.squotes (PP.pretty fnName)
          PP.<+> "function"
          PP.<+> PP.parens ("address" PP.<+> PP.pretty fnAddr)
          PP.<> foldMap
            (\retAddr -> ", which returns to address" PP.<+> PP.pretty retAddr)
            mbRetAddr
      FunctionOverride fnName ->
        "Using an override for the"
          PP.<+> PP.squotes (PP.pretty fnName)
          PP.<+> "function"
      SyscallOverride name num ->
        "Using an override for the"
          PP.<+> PP.pretty name
          PP.<+> PP.parens (PP.pretty num)
          PP.<+> "syscall"
      SkippedFunctionCall call -> PP.pretty call
      SkippedSyscall call -> PP.pretty call

severity :: Diagnostic -> Severity
severity =
  \case
    PltCall{} -> Debug
    FunctionCall{} -> Debug
    FunctionOverride{} -> Debug
    SyscallOverride{} -> Debug
    SkippedFunctionCall{} -> Info
    SkippedSyscall{} -> Info
