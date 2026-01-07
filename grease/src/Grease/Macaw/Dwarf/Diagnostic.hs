{-# LANGUAGE LambdaCase #-}

module Grease.Macaw.Dwarf.Diagnostic (Diagnostic (..), severity, DwarfShapeParsingError (..)) where

import Data.Macaw.Dwarf qualified as DWARF
import Data.Macaw.Dwarf qualified as Dwarf
import Data.Macaw.Types (TypeRepr)
import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity (..))
import Grease.Macaw.RegName
import Prettyprinter qualified as PP

data Diagnostic where
  FailedToParse :: (PP.Pretty a, Show a) => Dwarf.Subprogram -> Dwarf.Variable -> a -> Diagnostic
  UsingDefaultForPointer :: (PP.Pretty a, Show a) => Dwarf.Subprogram -> a -> Diagnostic
  StoppingStruct :: (PP.Pretty a, Show a) => Dwarf.Subprogram -> Word64 -> a -> Diagnostic
deriving instance Show Diagnostic

instance PP.Pretty Diagnostic where
  pretty = \case
    FailedToParse subprog var msg ->
      PP.vcat
        [ PP.pretty
            "Failure for DWARF parsing at variable: "
        , PP.viaShow var
        , formatFailure subprog msg
        ]
    UsingDefaultForPointer subprog msg ->
      PP.vcat
        [ PP.pretty "Using default for pointer because of DWARF parsing failure:"
        , formatFailure subprog msg
        ]
    StoppingStruct subprog off msg ->
      PP.vcat
        [ PP.pretty "Stopping parsing struct at offset " PP.<+> PP.pretty off
        , formatFailure subprog msg
        ]
   where
    formatFailure subprog msg =
      PP.vcat
        [ PP.pretty "Failed to parse DWARF shape in:"
        , PP.viaShow subprog
        , PP.pretty "DWARF error message: "
        , PP.pretty msg
        ]

severity :: Diagnostic -> Severity
severity = \case
  FailedToParse _ _ _ -> Warn
  UsingDefaultForPointer _ _ -> Warn
  StoppingStruct _ _ _ -> Warn

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
