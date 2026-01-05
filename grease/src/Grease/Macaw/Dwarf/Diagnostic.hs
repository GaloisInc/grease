{-# LANGUAGE LambdaCase #-}

module Grease.Macaw.Dwarf.Diagnostic (Diagnostic (..), severity, DwarfShapeParsingError (..)) where

import Data.Macaw.Dwarf qualified as DWARF
import Data.Macaw.Dwarf qualified as Dwarf
import Data.Macaw.Types (TypeRepr)
import Grease.Diagnostic.Severity (Severity (..))
import Grease.Macaw.RegName
import Prettyprinter qualified as PP

data Diagnostic where
  FailedToParse :: (PP.Pretty a, Show a) => Dwarf.Subprogram -> Dwarf.Variable -> a -> Diagnostic

deriving instance Show Diagnostic

instance PP.Pretty Diagnostic where
  pretty = \case
    FailedToParse subprog var msg ->
      PP.vcat
        [ PP.pretty "Failed to parse DWARF shape in:"
        , PP.viaShow subprog
        , PP.pretty "Stopped at var: "
        , PP.viaShow var
        , PP.pretty "DWARF error message: "
        , PP.pretty msg
        ]

severity :: Diagnostic -> Severity
severity = \case
  FailedToParse _ _ _ -> Warn

data DwarfShapeParsingError where
  UnsupportedType :: DWARF.TypeApp -> DwarfShapeParsingError
  TypeMistmatchForRegister :: RegName -> String -> TypeRepr actual -> DwarfShapeParsingError
  UnexpectedDWARFForm :: String -> DwarfShapeParsingError

deriving instance Show DwarfShapeParsingError

instance PP.Pretty DwarfShapeParsingError where
  pretty = \case
    UnsupportedType ty ->
      PP.sep [PP.pretty "Unsupported DWARF type: ", PP.viaShow ty]
    TypeMistmatchForRegister rname expectedType actualType ->
      PP.sep
        [ PP.pretty "At register "
        , PP.viaShow rname
        , PP.pretty "DWARF expected a register type of: "
        , PP.viaShow expectedType
        , PP.pretty " Macaw register type is: "
        , PP.viaShow actualType
        ]
    UnexpectedDWARFForm msg -> PP.sep [PP.pretty "Unexpected format of DWARF: ", PP.pretty msg]
