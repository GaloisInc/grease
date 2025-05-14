{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}


module Grease.Concretize
  ( ConcMem(..)
  , ConcArgs(..)
  , printConcArgs
  , ConcretizedData(..)
  , concArgsToSym
  , makeConcretizedData
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Const (Const)
import Data.Macaw.Memory qualified as MM
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC (fmapFC, traverseFC)
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
import Prettyprinter qualified as PP
import What4.Expr qualified as W4
import What4.FloatMode qualified as W4FM

-- | Arguments ('Args') that have been concretized
newtype ConcArgs sym ext argTys
  = ConcArgs { getConcArgs :: Ctx.Assignment (Shape ext (Conc.ConcRV' sym)) argTys }

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

data ConcretizedData sym ext argTys
  = ConcretizedData
    { concArgs :: ConcArgs sym ext argTys
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
  InitialMem sym ->
  Maybe (Mem.CallStack, Mem.BadBehavior sym) ->
  Args sym ext argTys ->
  IO (ConcretizedData sym ext argTys)
makeConcretizedData bak groundEvalFn initMem minfo (Args initArgs) = do
  let sym = C.backendGetSym bak
  let ctx = Conc.ConcCtx @sym @t groundEvalFn Mem.concPtrFnMap
  let InitialMem mem = initMem
  let concRV :: forall tp. C.TypeRepr tp -> C.RegValue' sym tp -> IO (Conc.ConcRV' sym tp)
      concRV t = fmap (Conc.ConcRV' @sym) . Conc.concRegValue @sym @t ctx t . C.unRV
  cArgs <- liftIO (traverseFC (Shape.traverseShapeWithType concRV) initArgs)
  let W4.GroundEvalFn gFn = groundEvalFn
  cMem <- Mem.concMemImpl sym gFn mem
  cErr <- traverse (\(_, bb) -> Mem.concBadBehavior sym gFn bb) minfo
  pure $
    ConcretizedData
    { concArgs = ConcArgs cArgs
    , concMem = ConcMem cMem
    , concErr = cErr
    }
