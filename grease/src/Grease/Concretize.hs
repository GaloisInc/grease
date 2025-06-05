{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Grease.Concretize
  ( -- * Data to be concretized
    InitialState(..)
  , ToConcretize(..)
  , HasToConcretize(..)
  , stateToConcretize
  , addToConcretize
    -- * Concretization
  , ConcMem(..)
  , ConcArgs(..)
  , ConcretizedData(..)
  , SomeConcretizedValue(..)
  , concArgsToSym
  , makeConcretizedData
    -- * Pretty-printing
  , printConcArgs
  , printConcFs
  , printConcData
  ) where

import Control.Lens qualified as Lens
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO(..))
import Data.BitVector.Sized qualified as BV
import Data.Foldable (toList)
import Data.Functor.Const (Const)
import Data.List qualified as List
import Data.Macaw.Memory qualified as MM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some (Some (Some))
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
import Data.Text (Text)
import Data.Type.Equality (testEquality)
import Data.Word (Word8)
import Grease.Setup (Args(Args), InitialMem(..))
import Grease.Shape (Shape, ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.Concretize (concShape)
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Grease.Shape.Print qualified as ShapePP
import Grease.Utility (OnlineSolverAndBackend)
import Lang.Crucible.Backend qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.Concretize qualified as Conc
import Lang.Crucible.LLVM.Errors qualified as Mem
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.MemModel.CallStack qualified as Mem
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.SymIO qualified as SymIO
import Numeric (showHex)
import Prettyprinter qualified as PP
import What4.Expr qualified as W4
import What4.FloatMode qualified as W4FM

---------------------------------------------------------------------
-- * Data to be concretized

-- | Initial state, to be concretized into 'ConcretizedData'
data InitialState sym ext argTys
  = InitialState
    { initStateArgs :: Args sym ext argTys
    , initStateFs :: SymIO.InitialFileSystemContents sym
    , initStateMem :: InitialMem sym
    }

-- | Extra data created during simulation (usually by overrides) to be
-- concretized
newtype ToConcretize sym
  = ToConcretize { _getToConcretize :: [(Text, Some (C.RegEntry sym))] }

makeLenses ''ToConcretize

-- | A class for Crucible personality types @p@ (see
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality') which contain a
-- 'ToConcretize'.
class HasToConcretize p sym | p -> sym where
  toConcretize :: Lens.Lens' p (ToConcretize sym)

instance HasToConcretize (ToConcretize sym) sym where
  toConcretize = id

-- | `Lens.Lens'` for the 'ToConcretize' in the
-- 'Lang.Crucible.Simulator.ExecutionTree.cruciblePersonality'
stateToConcretize ::
  HasToConcretize p sym =>
  Lens.Lens' (C.SimState p sym ext r f a) (ToConcretize sym)
stateToConcretize = C.stateContext . C.cruciblePersonality . toConcretize

-- | Add a value to the 'ToConcretize' in the 'C.SimState'.
addToConcretize ::
  HasToConcretize p sym =>
  -- | Name
  Text ->
  C.RegEntry sym ty ->
  C.OverrideSim p sym ext rtp args ret ()
addToConcretize txt val =
  C.stateContext . C.cruciblePersonality . toConcretize . getToConcretize Lens.%=
    ((txt, Some val) :)

---------------------------------------------------------------------
-- * Concretization

-- | Arguments ('Args') that have been concretized
newtype ConcArgs sym ext argTys
  = ConcArgs { getConcArgs :: Ctx.Assignment (Shape ext (Conc.ConcRV' sym)) argTys }

-- | Turn 'ConcArgs' back into a 'C.RegMap' that can be used to re-execute
-- a CFG.
concArgsToSym ::
  forall sym ext brand st fm wptr argTys.
  C.IsSymInterface sym =>
  (sym ~ W4.ExprBuilder brand st (W4.Flags fm)) =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  sym ->
  W4FM.FloatModeRepr fm ->
  Ctx.Assignment C.TypeRepr argTys ->
  ConcArgs sym ext argTys ->
  IO (C.RegMap sym argTys)
concArgsToSym sym fm argTys (ConcArgs cArgs) =
  C.RegMap <$>
    Ctx.zipWithM
    (\tp cShape -> do
      let conc = Conc.unConcRV' (Shape.getTag PtrShape.getPtrTag cShape)
      symb <- Conc.concToSym sym Mem.concToSymPtrFnMap fm tp conc
      pure (C.RegEntry @sym tp symb))
    argTys
    cArgs

-- | Memory before execution ('InitialMem') that has been concretized
--
-- See crucible#1217 for ideas on how we could present this more intuitively in
-- the future.
newtype ConcMem sym = ConcMem { getConcMem :: Mem.MemImpl sym }

-- | File system contents before execution ('SymIO.InitialFileSystemContents')
-- that has been concretized
newtype ConcFs = ConcFs { getConcFs :: Map (SymIO.FDTarget SymIO.In) [Word8] }

-- | An extra value that has been concretized
data SomeConcretizedValue sym
  = forall ty.
    SomeConcretizedValue
    { concName :: Text
    , concTy :: C.TypeRepr ty
    , concValue :: Conc.ConcRV' sym ty
    }

-- | Concretized version of 'InitialState' plus 'ToConcretize'
--
-- Produced by 'makeConcretizedData'
data ConcretizedData sym ext argTys
  = ConcretizedData
    { concArgs :: ConcArgs sym ext argTys
      -- | Concretized values from 'ToConcretize'
    , concExtra :: [SomeConcretizedValue sym]
    , concFs :: ConcFs
    , concMem :: ConcMem sym
    , concErr :: Maybe (Mem.BadBehavior sym)
    }

makeConcretizedData ::
  forall solver sym ext wptr bak t st argTys fm.
  OnlineSolverAndBackend solver sym bak t st fm =>
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  bak ->
  W4.GroundEvalFn t ->
  Maybe (Mem.CallStack, Mem.BadBehavior sym) ->
  InitialState sym ext argTys ->
  ToConcretize sym ->
  IO (ConcretizedData sym ext argTys)
makeConcretizedData bak groundEvalFn minfo initState extra = do
  let InitialState
        { initStateArgs = Args initArgs
        , initStateFs = initFs
        , initStateMem = InitialMem initMem
        } = initState
  let sym = C.backendGetSym bak
  let ctx = Conc.ConcCtx @sym @t groundEvalFn Mem.concPtrFnMap
  let concRV :: forall tp. C.TypeRepr tp -> C.RegValue' sym tp -> IO (Conc.ConcRV' sym tp)
      concRV t = fmap (Conc.ConcRV' @sym) . Conc.concRegValue @sym @t ctx t . C.unRV
  cArgs <- liftIO (traverseFC (Shape.traverseShapeWithType concRV) initArgs)
  let W4.GroundEvalFn gFn = groundEvalFn
  let toWord8 :: BV.BV 8 -> Word8
      toWord8 = fromIntegral . BV.asUnsigned
  let doConcExtra :: Text -> Some (C.RegEntry sym) -> IO (SomeConcretizedValue sym)
      doConcExtra name (Some (C.RegEntry ty val)) = do
        concVal <- concRV ty (C.RV val)
        pure $
          SomeConcretizedValue
          { concName = name
          , concTy = ty
          , concValue = concVal
          }
  cExtra <- liftIO (traverse (uncurry doConcExtra) (_getToConcretize extra))
  cFs <- traverse (traverse (fmap toWord8 . gFn)) (SymIO.symbolicFiles initFs)
  cMem <- Mem.concMemImpl sym gFn initMem
  cErr <- traverse (\(_, bb) -> Mem.concBadBehavior sym gFn bb) minfo
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

printConcArgs ::
  Mem.HasPtrWidth wptr =>
  (ExtShape ext ~ PtrShape ext wptr) =>
  MM.AddrWidthRepr wptr ->
  -- | Argument names
  Ctx.Assignment (Const String) argTys ->
  -- | Which shapes to print
  Ctx.Assignment (Const Bool) argTys ->
  ConcArgs sym ext argTys ->
  PP.Doc ann
printConcArgs addrWidth argNames filt (ConcArgs cArgs) =
  let cShapes = fmapFC concShape cArgs in
  let rleThreshold = 8 in  -- this matches uses in Grease.Refine.Diagnostic
   ShapePP.evalPrinter
    (ShapePP.PrinterConfig addrWidth rleThreshold)
    (ShapePP.printNamedShapesFiltered argNames filt cShapes)

-- | Pretty-print the \"extra\" concretized data
printConcExtra ::
  [SomeConcretizedValue sym] ->
  PP.Doc ann
printConcExtra vals =
  PP.vsep $
    PP.pretty "Concretized values:" :
      map (PP.indent 2 . ppValue) vals
  where
    ppValue :: SomeConcretizedValue sym -> PP.Doc ann
    ppValue (SomeConcretizedValue { concName = name, concTy = ty, concValue = Conc.ConcRV' val }) =
      PP.vsep
      [ PP.pretty name
      , PP.indent 2 $
          case ty of
            C.VectorRepr (C.BVRepr w) | Just C.Refl <- testEquality w (knownNat @8) ->
              PP.fillSep (List.map (\(Conc.ConcRV' b) -> PP.pretty (BV.ppHex (knownNat @8) b)) (toList val))
            -- TODO(#204): Handle more cases
            _ -> PP.pretty "<can't print this value>"
      ]

-- | Pretty-print the concretized filesystem
printConcFs ::
  ConcFs ->
  PP.Doc ann
printConcFs cFs =
  PP.vsep $
    PP.pretty "Concretized filesystem:" :
      map (PP.indent 2 . uncurry ppFile) (Map.toList (getConcFs cFs))
  where
    ppWord8 = PP.pretty . padHex 2

    showHex' :: Integral a => a -> String
    showHex' n = showHex n ""

    padHex :: Integral a => Int -> a -> String
    padHex pad v =
      let initial = showHex' v
          zs = List.take (pad - List.length initial) (List.repeat '0')
      in zs List.++ initial

    ppFile :: SymIO.FDTarget SymIO.In -> [Word8] -> PP.Doc ann
    ppFile tgt content =
      PP.vsep
      [ PP.pretty (SymIO.fdTargetToText tgt)
      , PP.indent 2 (PP.fillSep (List.map ppWord8 content))
      ]

-- | Pretty-print concretized arguments and the concretized filesystem
printConcData ::
  Mem.HasPtrWidth wptr =>
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
  in PP.vsep $
       concat $
         [ [args]
         , if Map.null (getConcFs (concFs cData)) then [] else [fs]
         , if List.null (concExtra cData) then [] else [extra]
         ]
