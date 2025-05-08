{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.SkippedCall (SkippedCall(..)) where

import Data.Text (Text)

import Prettyprinter qualified as PP

-- what4
import What4.FunctionName qualified as W4
import What4.Interface qualified as W4

-- macaw-base
import Data.Macaw.CFG.Core qualified as MC
import Data.Macaw.Memory (MemSegmentOff, MemWidth)

-- | The reasons that GREASE might skip a function call
data SkippedCall arch where
  SymbolicAddress ::
    SkippedCall arch
  InvalidAddress ::
    String {- ^ Address -} ->
    SkippedCall arch
  PltNoOverride ::
    (1 W4.<= MC.ArchAddrWidth arch, MemWidth (MC.ArchAddrWidth arch)) =>
    MemSegmentOff (MC.ArchAddrWidth arch) ->
    W4.FunctionName ->
    SkippedCall arch
  NotExecutable ::
    (1 W4.<= MC.ArchAddrWidth arch, MemWidth (MC.ArchAddrWidth arch)) =>
    MemSegmentOff (MC.ArchAddrWidth arch) ->
    SkippedCall arch
  SymbolicSyscallNumber ::
    SkippedCall arch
  UnknownSyscallNumber ::
    Int ->
    SkippedCall arch
  SyscallWithoutOverride ::
    Text {- ^ Syscall name -} ->
    Int {- ^ Syscall number -} ->
    SkippedCall arch

instance PP.Pretty (SkippedCall arch) where
  pretty =
    \case
      SymbolicAddress -> "Skipped call to a symbolic address"
      InvalidAddress addr -> "Skipped call to an invalid address:" PP.<+> PP.pretty addr
      PltNoOverride addr name ->
        "Skipped call to a PLT stub at address" PP.<+>
        PP.pretty addr PP.<> PP.colon PP.<+> PP.pretty name
      NotExecutable addr ->
        "Skipped call to a non-executable address:" PP.<+> PP.pretty addr
      SymbolicSyscallNumber -> "Skipped syscall call with a symbolic syscall number"
      UnknownSyscallNumber num -> "Skipped syscall with unknown number" PP.<+> PP.parens (PP.pretty num)
      SyscallWithoutOverride name num -> "Skipped system call without an override:" PP.<+> PP.pretty name PP.<+> PP.parens (PP.pretty num)
