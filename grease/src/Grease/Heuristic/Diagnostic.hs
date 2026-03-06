{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Heuristic.Diagnostic (
  Diagnostic (..),
  severity,
) where

import Control.Lens ((^.))
import Data.Macaw.CFG qualified as MC
import Grease.Cursor (CursorExt, ppCursor)
import Grease.Cursor.Pointer (Dereference, ppDereference)
import Grease.Diagnostic.Severity (Severity (Debug, Info))
import Grease.Shape.Pointer (KnownPtrMode, PtrTarget)
import Grease.Shape.Selector (ArgSelector, argSelectorPath)
import Prettyprinter qualified as PP

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
    forall w tag ptrData.
    (KnownPtrMode ptrData, MC.PrettyF tag) =>
    PtrTarget w ptrData tag ->
    Diagnostic
  DecomposingOffset ::
    forall ext w argTys ts t.
    CursorExt ext ~ Dereference ext w =>
    -- | Argument name
    String ->
    -- | Constant offset extracted
    Integer ->
    ArgSelector ext argTys ts t ->
    Diagnostic

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
      DecomposingOffset @ext argName constantOff sel ->
        mconcat
          [ "Heuristic: decomposed pointer offset for "
          , ppCursor
              (PP.pretty argName)
              (ppDereference @ext)
              (sel ^. argSelectorPath)
          , ", constant offset: "
          , PP.viaShow constantOff
          , " bytes"
          ]

severity :: Diagnostic -> Severity
severity =
  \case
    DefaultHeuristicsGrowAndInitMem{} -> Info
    DefaultHeuristicsBytesToPtr{} -> Info
    HeuristicPtrTarget{} -> Debug
    DecomposingOffset{} -> Info
