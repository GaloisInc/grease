{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.SkippedCall
  ( SkippedFunctionCall(..)
  , SkippedSyscall(..)
  ) where

import Data.Macaw.CFG.Core qualified as MC
import Data.Macaw.Memory (MemSegmentOff, MemWidth)
import Data.Text (Text)
import Prettyprinter qualified as PP
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4

-- | The reasons that GREASE might skip a function call
data SkippedFunctionCall arch where
  SymbolicAddress ::
    SkippedFunctionCall arch
  InvalidAddress ::
    String {- ^ Address -} ->
    SkippedFunctionCall arch
  PltNoOverride ::
    (1 W4.<= MC.ArchAddrWidth arch, MemWidth (MC.ArchAddrWidth arch)) =>
    MemSegmentOff (MC.ArchAddrWidth arch) ->
    W4.FunctionName ->
    SkippedFunctionCall arch
  NotExecutable ::
    (1 W4.<= MC.ArchAddrWidth arch, MemWidth (MC.ArchAddrWidth arch)) =>
    MemSegmentOff (MC.ArchAddrWidth arch) ->
    SkippedFunctionCall arch

-- | The reasons that GREASE might skip a function call
data SkippedSyscall where
  SymbolicSyscallNumber ::
    SkippedSyscall
  UnknownSyscallNumber ::
    Int ->
    SkippedSyscall
  SyscallWithoutOverride ::
    Text {- ^ Syscall name -} ->
    Int {- ^ Syscall number -} ->
    SkippedSyscall

instance PP.Pretty (SkippedFunctionCall arch) where
  pretty =
    \case
      SymbolicAddress -> "Skipped call to a symbolic address"
      InvalidAddress addr -> "Skipped call to an invalid address:" PP.<+> PP.pretty addr
      PltNoOverride addr name ->
        "Skipped call to a PLT stub at address" PP.<+>
        PP.pretty addr PP.<> PP.colon PP.<+> PP.pretty name
      NotExecutable addr ->
        "Skipped call to a non-executable address:" PP.<+> PP.pretty addr

instance PP.Pretty SkippedSyscall where
  pretty =
    \case
      SymbolicSyscallNumber -> "Skipped syscall call with a symbolic syscall number"
      UnknownSyscallNumber num -> "Skipped syscall with unknown number" PP.<+> PP.parens (PP.pretty num)
      SyscallWithoutOverride name num -> "Skipped system call without an override:" PP.<+> PP.pretty name PP.<+> PP.parens (PP.pretty num)
