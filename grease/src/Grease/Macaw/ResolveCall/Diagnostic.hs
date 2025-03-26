{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.ResolveCall.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Data.Text (Text)

import qualified Prettyprinter as PP

-- what4
import qualified What4.FunctionName as W4

-- macaw
import qualified Data.Macaw.Memory as MM

import Grease.Diagnostic.Severity (Severity(Debug, Info))
import qualified Grease.Macaw.SkippedCall as Skip

data Diagnostic where
  PltCall ::
    MM.MemWidth w =>
    W4.FunctionName
      {- ^ The name of the PLT stub -} ->
    MM.MemSegmentOff w
      {- ^ The address of the PLT stub -} ->
    MM.MemSegmentOff w
      {- ^ The address that the PLT call jumps to -} ->
    Diagnostic
  FunctionCall ::
    MM.MemWidth w =>
    W4.FunctionName
      {- ^ The function name -} ->
    MM.MemWord w
      {- ^ The address where the function is defined -} ->
    Maybe (MM.MemWord w)
      {- ^ The address that the function returns to (if known) -} ->
    Diagnostic
  FunctionOverride ::
    W4.FunctionName
      {- ^ The overridden function's name -} ->
      Diagnostic
  SyscallOverride ::
    Text {- ^ Syscall name -} ->
    Int {- ^ Syscall number -} ->
    Diagnostic
  SkippedCall ::
    Skip.SkippedCall arch -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty d =
    case d of
      PltCall pltStubName pltStubAddr pltCallAddr ->
        "Calling a PLT stub" PP.<+>
        PP.parens (PP.pretty pltStubName) PP.<+>
        "at address" PP.<+>
        PP.pretty pltStubAddr PP.<>
        ", which jumps to address" PP.<+>
        PP.pretty pltCallAddr
      FunctionCall fnName fnAddr mbRetAddr ->
        "Invoking the" PP.<+> PP.squotes (PP.pretty fnName) PP.<+>
        "function" PP.<+>
        PP.parens ("address" PP.<+> PP.pretty fnAddr) PP.<>
        foldMap
          (\retAddr -> ", which returns to address" PP.<+> PP.pretty retAddr)
          mbRetAddr
      FunctionOverride fnName ->
        "Using an override for the" PP.<+> PP.squotes (PP.pretty fnName) PP.<+>
        "function"
      SyscallOverride name num ->
        "Using an override for the" PP.<+> PP.pretty name PP.<+>
        PP.parens (PP.pretty num) PP.<+> "syscall"
      SkippedCall call -> PP.pretty call

severity :: Diagnostic -> Severity
severity =
  \case
    PltCall{} -> Debug
    FunctionCall{} -> Debug
    FunctionOverride{} -> Debug
    SyscallOverride{} -> Debug
    SkippedCall{} -> Info
