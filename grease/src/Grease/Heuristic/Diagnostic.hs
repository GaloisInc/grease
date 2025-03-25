{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Heuristic.Diagnostic
  ( Diagnostic(..)
  , severity
  ) where

import Control.Lens ((^.))

import qualified Prettyprinter as PP

import qualified Data.Macaw.CFG as MC

import Grease.Cursor (CursorExt, ppCursor)
import Grease.Cursor.Pointer (Dereference, ppDereference)
import Grease.Shape.Pointer (PtrTarget)
import Grease.Shape.Selector (ArgSelector, argSelectorPath)
import Grease.Diagnostic.Severity (Severity(Debug, Info))

data Diagnostic where
  DefaultHeuristicsBytesToPtr ::
    forall ext w argTys ts t.
    CursorExt ext ~ Dereference ext w =>
    -- | Argument name
    String ->
    ArgSelector ext argTys ts t ->
    Diagnostic
  DefaultHeuristicsGrowAndInitMem ::
    forall ext w argTys ts t.
    CursorExt ext ~ Dereference ext w =>
    -- | Argument name
    String ->
    ArgSelector ext argTys ts t ->
    Diagnostic
  HeuristicPtrTarget ::
    MC.PrettyF tag => PtrTarget w tag -> Diagnostic

instance PP.Pretty Diagnostic where
  pretty =
    \case
      DefaultHeuristicsBytesToPtr @ext argName sel ->
        mconcat
          [ "Heuristic: turn bytes into pointer: "
          , ppCursor
              (PP.pretty argName)
              (ppDereference @ext)
              (sel ^. argSelectorPath)
          ]
      DefaultHeuristicsGrowAndInitMem @ext argName sel ->
        mconcat
          [ "Heuristic: grow and initialize memory referenced by: "
          , ppCursor
              (PP.pretty argName)
              (ppDereference @ext)
              (sel ^. argSelectorPath)
          ]
      HeuristicPtrTarget tgt ->
        mconcat
          [ "Heuristic: refining pointer target: "
          , PP.pretty tgt
          ]

severity :: Diagnostic -> Severity
severity =
  \case
    DefaultHeuristicsGrowAndInitMem{} -> Info
    DefaultHeuristicsBytesToPtr{} -> Info
    HeuristicPtrTarget{} -> Debug
