{-# LANGUAGE DataKinds #-}

module Grease.LLVM.Shapes (
  InitArgShapesError (..),
  llvmInitArgShapes,
) where

import Data.Functor.Const (Const)
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (traverseFC)
import Data.Void (Void)
import Grease.LLVM.DebugInfo qualified as GLD
import Grease.Options qualified as GO
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Simple qualified as Simple
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.FunctionHandle qualified as CFH
import Lang.Crucible.LLVM qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Prettyprinter qualified as PP
import Text.LLVM qualified as L

data InitArgShapesError
  = ParseError (PP.Doc Void)
  | MinimalShapeError Shape.MinimalShapeError
  | TypeMismatch Shape.TypeMismatch

-- | Compute the initial 'ArgShapes' for an LLVM CFG.
--
-- Sources argument shapes from:
--
-- 1. A default initial shape for each register (via 'Shape.minimalShapeWithPtrs')
-- 2. DWARF debug info (via 'GLD.diArgShapes') if
--    'initPrecondUseDebugInfo' is 'True'
-- 3. A shape DSL file (via the 'Shape.ParsedShapes')
-- 4. Simple shapes from the CLI (via 'useSimpleShapes')
--
-- Later steps override earlier ones.
llvmInitArgShapes ::
  CLM.HasPtrWidth 64 =>
  GO.InitialPreconditionOpts ->
  Maybe L.Module ->
  Ctx.Assignment (Const String) argTys ->
  Maybe (Shape.ParsedShapes CLLVM.LLVM) ->
  -- | The CFG of the user-requested entrypoint function.
  C.CFG CLLVM.LLVM blocks argTys ret ->
  Either InitArgShapesError (Shape.ArgShapes CLLVM.LLVM NoTag argTys)
llvmInitArgShapes opts llvmMod argNames parsedShapes cfg = do
  let argTys = C.cfgArgTypes cfg
  let initArgs0 =
        -- TODO(#519): We should match on the type of debug info usage
        -- and support conservative debug info
        case (llvmMod, GO.initPrecondUseDebugInfo opts) of
          (Just m, GO.PreciseDebugInfoShapes) ->
            GLD.diArgShapes (CFH.handleName (C.cfgHandle cfg)) argTys m
          _ -> traverseFC (Shape.minimalShapeWithPtrs @_ @CLLVM.LLVM (const NoTag)) argTys
  initArgs1 <-
    Shape.ArgShapes
      <$> case initArgs0 of
        Left err -> Left (MinimalShapeError err)
        Right ok -> Right ok
  initArgs2 <-
    case parsedShapes of
      Nothing -> Right initArgs1
      Just parsed ->
        case Shape.replaceShapes argNames initArgs1 parsed of
          Left err -> Left (TypeMismatch err)
          Right shapes -> Right shapes
  case Simple.useSimpleShapes argNames initArgs2 (GO.initPrecondSimpleShapes opts) of
    Left err -> Left (TypeMismatch err)
    Right shapes -> Right shapes
