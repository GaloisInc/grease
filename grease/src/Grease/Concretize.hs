{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Concretize (
  -- * Data to be concretized
  InitialState (..),

  -- * Concretization
  ConcMem (..),
  ConcArgs (..),
  ConcretizedData (..),
  SomeConcretizedValue (..),
  concArgsToSym,
  makeConcretizedData,

  -- * Pretty-printing
  printConcArgs,
  printConcFs,
  printConcData,
) where

import Control.Monad.IO.Class (liftIO)
import Data.BitVector.Sized qualified as BV
import Data.Foldable (toList)
import Data.Functor qualified as Functor
import Data.Functor.Const (Const)
import Data.Functor.Const qualified as Const
import Data.Functor.Product qualified as Product
import Data.List qualified as List
import Data.Macaw.Memory qualified as MM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Monoid
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text (Text)
import Data.Type.Equality (testEquality)
import Data.Word (Word8)
import Grease.Concretize.ToConcretize (ToConcretizeType)
import Grease.ErrorDescription (ErrorDescription)
import Grease.ErrorDescription qualified as Err
import Grease.Setup (Args (Args), InitialMem (InitialMem))
import Grease.Shape (ExtShape, Shape)
import Grease.Shape qualified as Shape
import Grease.Shape.Concretize (concShape)
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Grease.Shape.Print qualified as ShapePP
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.MemModel.Pointer qualified as CLMP
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Simulator.SymSequence qualified as C
import Lang.Crucible.SymIO qualified as SymIO
import Numeric (showHex)
import Prettyprinter qualified as PP
import What4.Expr qualified as WE
import What4.FloatMode qualified as W4FM
import What4.Interface qualified as WI

---------------------------------------------------------------------

-- * Data to be concretized

-- | Initial state, to be concretized into 'ConcretizedData'
data InitialState sym ext argTys
  = InitialState
  { initStateArgs :: Args sym ext argTys
  , initStateFs :: SymIO.InitialFileSystemContents sym
  , initStateMem :: InitialMem sym
  }

---------------------------------------------------------------------

-- * Concretization

-- | Arguments ('Args') that have been concretized
newtype ConcArgs sym ext argTys
  = ConcArgs {getConcArgs :: Ctx.Assignment (Shape ext (Conc.ConcRV' sym)) argTys}

-- | Turn 'ConcArgs' back into a 'C.RegMap' that can be used to re-execute
-- a CFG.
concArgsToSym ::
  forall sym ext brand st fm wptr argTys.
  CB.IsSymInterface sym =>
  (sym ~ WE.ExprBuilder brand st (WE.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  sym ->
  W4FM.FloatModeRepr fm ->
  Ctx.Assignment C.TypeRepr argTys ->
  ConcArgs sym ext argTys ->
  IO (CS.RegMap sym argTys)
concArgsToSym sym fm argTys (ConcArgs cArgs) =
  CS.RegMap
    <$> Ctx.zipWithM
      ( \tp cShape -> do
          let conc = Conc.unConcRV' (Shape.getTag PtrShape.getPtrTag cShape)
          symb <- Conc.concToSym sym CLMP.concToSymPtrFnMap fm tp conc
          pure (CS.RegEntry @sym tp symb)
      )
      argTys
      cArgs

-- | Memory before execution ('InitialMem') that has been concretized
--
-- See crucible#1217 for ideas on how we could present this more intuitively in
-- the future.
newtype ConcMem sym = ConcMem {getConcMem :: CLM.MemImpl sym}

-- | File system contents before execution ('SymIO.InitialFileSystemContents')
-- that has been concretized
newtype ConcFs = ConcFs {getConcFs :: Map (SymIO.FDTarget SymIO.In) [Word8]}

-- | An extra value that has been concretized
data SomeConcretizedValue sym
  = forall ty.
  SomeConcretizedValue
  { concName :: Text
  , concTy :: C.TypeRepr ty
  , concValue :: Conc.ConcRV' sym ty
  }

-- | Concretized version of 'InitialState' plus @GlobalVar 'ToConcretizeType'@
--
-- Produced by 'makeConcretizedData'
data ConcretizedData sym ext argTys
  = ConcretizedData
  { concArgs :: ConcArgs sym ext argTys
  , concExtra :: [SomeConcretizedValue sym]
  -- ^ Concretized values from the @GlobalVar 'ToConcretizeType'@
  , concFs :: ConcFs
  , concMem :: ConcMem sym
  , concErr :: Maybe (ErrorDescription sym)
  }

makeConcretizedData ::
  forall solver sym ext wptr bak t st argTys fm.
  OnlineSolverAndBackend solver sym bak t st fm =>
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  bak ->
  WE.GroundEvalFn t ->
  Maybe (ErrorDescription sym) ->
  InitialState sym ext argTys ->
  CS.RegValue sym ToConcretizeType ->
  IO (ConcretizedData sym ext argTys)
makeConcretizedData bak groundEvalFn minfo initState extra = do
  let InitialState
        { initStateArgs = Args initArgs
        , initStateFs = initFs
        , initStateMem = InitialMem initMem
        } = initState
  let sym = CB.backendGetSym bak
  let ctx = Conc.ConcCtx @sym @t groundEvalFn CLMP.concPtrFnMap
  let concRV :: forall tp. C.TypeRepr tp -> CS.RegValue' sym tp -> IO (Conc.ConcRV' sym tp)
      concRV t = fmap (Conc.ConcRV' @sym) . Conc.groundRegValue @sym @t ctx t . CS.unRV
  cArgs <- liftIO (traverseFC (Shape.traverseShapeWithType concRV) initArgs)
  let WE.GroundEvalFn gFn = groundEvalFn
  let toWord8 :: BV.BV 8 -> Word8
      toWord8 = fromIntegral . BV.asUnsigned
  let concStruct (Ctx.Empty Ctx.:> anyVal Ctx.:> name) = do
        Conc.ConcRV' (Conc.ConcAnyValue ty cVal) <- concRV C.AnyRepr anyVal
        Conc.ConcRV' (WI.UnicodeLiteral cName) <-
          concRV (C.StringRepr WI.UnicodeRepr) name
        pure $
          SomeConcretizedValue
            { concName = cName
            , concTy = ty
            , concValue = cVal
            }
  -- re: reverse: The sequence is a cons-list, so the values appear in
  -- reverse-chronological (LIFO) order from when they were created.
  cExtra <- List.reverse . toList <$> liftIO (C.concretizeSymSequence gFn concStruct extra)
  cFs <- traverse (traverse (fmap toWord8 . gFn)) (SymIO.symbolicFiles initFs)
  cMem <- CLM.concMemImpl sym gFn initMem
  cErr <- traverse (\eds -> Err.concretizeErrorDescription sym groundEvalFn eds) minfo
  pure $
    ConcretizedData
      { concArgs = ConcArgs cArgs
      , concExtra = cExtra
      , concFs = ConcFs cFs
      , concMem = ConcMem cMem
      , concErr = cErr
      }

---------------------------------------------------------------------

-- * Pretty-printing

-- | Helper function to print concretized shapes with custom offset extractor
printConcNamedShapesFiltered ::
  forall ext w tag nm tys ann.
  PP.Pretty nm =>
  CLMP.HasPtrWidth w =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Offset extractor for concretized pointers
  (forall ty. tag (CLM.LLVMPointerType w) -> Maybe PtrShape.Offset -> PtrShape.Offset) ->
  -- | Names
  Ctx.Assignment (Const.Const nm) tys ->
  -- | Only print those 'Shape's with 'True' in the corresponding entry
  Ctx.Assignment (Const.Const Bool) tys ->
  -- | Shapes to print
  Ctx.Assignment (Shape ext tag) tys ->
  ShapePP.Printer w (PP.Doc ann)
printConcNamedShapesFiltered getOffset names filt shapes =
  TFC.foldlMFC'
    ( \doc (Product.Pair (Product.Pair (Const.Const nm) s) (Const.Const b)) ->
        if b
          then
            ((doc PP.<> PP.line) PP.<>)
              Functor.<$> printConcNamed getOffset nm s
          else pure doc
    )
    Monoid.mempty
    (Ctx.zipWith Product.Pair (Ctx.zipWith Product.Pair names shapes) filt)

-- | Helper to print a single named concretized shape
printConcNamed ::
  PP.Pretty nm =>
  CLMP.HasPtrWidth w =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Offset extractor
  (forall ty. tag (CLM.LLVMPointerType w) -> Maybe PtrShape.Offset -> PtrShape.Offset) ->
  nm ->
  Shape ext tag ty ->
  ShapePP.Printer w (PP.Doc ann)
printConcNamed getOffset name s =
  ((PP.pretty name PP.<> ": ") PP.<>) Functor.<$> ShapePP.printShapeWithOffset getOffset s

printConcArgs ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  MM.AddrWidthRepr wptr ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  -- | Which shapes to print
  Ctx.Assignment (Const Bool) argTys ->
  ConcArgs sym ext argTys ->
  PP.Doc ann
printConcArgs addrWidth argNames filt (ConcArgs cArgs) =
  let cShapes = fmapFC concShape cArgs
      -- Custom offset extractor for concretized pointers
      getConcOffset tag _ =
        let ptr = Conc.unConcRV' tag
            offsetBV = CLMP.concOffset ptr
            offsetBytes = BV.asUnsigned offsetBV
         in PtrShape.Offset (CLB.toBytes offsetBytes)
      rleThreshold = 8 -- this matches uses in Grease.Refine.Diagnostic
   in ShapePP.evalPrinter
        (ShapePP.PrinterConfig addrWidth rleThreshold)
        (printConcNamedShapesFiltered getConcOffset argNames filt cShapes)

-- | Helper, not exported
showHex' :: Integral a => a -> String
showHex' n = showHex n ""

-- | Helper, not exported
padHex :: Integral a => Int -> a -> String
padHex pad v =
  let initial = showHex' v
      zs = List.take (pad - List.length initial) (List.repeat '0')
   in zs List.++ initial

-- | Pretty-print the \"extra\" concretized data
printConcExtra ::
  [SomeConcretizedValue sym] ->
  PP.Doc ann
printConcExtra vals =
  PP.vsep $
    PP.pretty ("Concretized values:" :: Text)
      : map (PP.indent 2 . ppValue) vals
 where
  ppBv8 = PP.pretty . padHex 2 . BV.asUnsigned

  ppValue :: SomeConcretizedValue sym -> PP.Doc ann
  ppValue (SomeConcretizedValue{concName = name, concTy = ty, concValue = Conc.ConcRV' val}) =
    PP.vsep
      [ PP.pretty name
      , PP.indent 2 $
          case ty of
            C.VectorRepr (C.BVRepr w)
              | Just C.Refl <- testEquality w (knownNat @8) ->
                  PP.fillSep (List.map (\(Conc.ConcRV' b) -> ppBv8 b) (toList val))
            _ -> PP.pretty ("<can't print this value>" :: Text)
      ]

-- | Pretty-print the concretized filesystem
printConcFs ::
  ConcFs ->
  PP.Doc ann
printConcFs cFs =
  PP.vsep $
    PP.pretty ("Concretized filesystem:" :: Text)
      : map (PP.indent 2 . uncurry ppFile) (Map.toList (getConcFs cFs))
 where
  ppWord8 = PP.pretty . padHex 2

  ppFile :: SymIO.FDTarget SymIO.In -> [Word8] -> PP.Doc ann
  ppFile tgt content =
    PP.vsep
      [ PP.pretty (SymIO.fdTargetToText tgt)
      , PP.indent 2 (PP.fillSep (List.map ppWord8 content))
      ]

-- | Pretty-print concretized arguments and the concretized filesystem
printConcData ::
  CLMP.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  MM.AddrWidthRepr wptr ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  -- | Which shapes to print
  Ctx.Assignment (Const Bool) argTys ->
  ConcretizedData sym ext argTys ->
  PP.Doc ann
printConcData addrWidth argNames filt cData =
  let args = printConcArgs addrWidth argNames filt (concArgs cData)
      fs = printConcFs (concFs cData)
      extra = printConcExtra (concExtra cData)
      vsep2 = PP.concatWith (\x y -> x <> PP.line <> PP.line <> y)
   in vsep2 $
        concat $
          [ [args]
          , if Map.null (getConcFs (concFs cData)) then [] else [fs]
          , if List.null (concExtra cData) then [] else [extra]
          ]
