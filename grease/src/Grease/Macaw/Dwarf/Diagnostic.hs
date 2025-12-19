{-# LANGUAGE LambdaCase #-}

module Grease.Macaw.Dwarf.Diagnostic (Diagnostic (..), severity) where

import Data.Macaw.Dwarf qualified as Dwarf
import Grease.Diagnostic.Severity (Severity (..))
import Prettyprinter qualified as PP

data Diagnostic where
  FailedToParse :: Dwarf.Subprogram -> String -> Diagnostic
  deriving (Show)

instance PP.Pretty Diagnostic where
  pretty = \case
    FailedToParse subprog msg -> PP.hcat [PP.pretty "Failed to parse dwarf shape in:", PP.viaShow subprog, PP.pretty " with message: ", PP.pretty msg]

severity :: Diagnostic -> Severity
severity = \case
  FailedToParse _ _ -> Warn
