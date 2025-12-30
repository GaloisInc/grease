{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Dwarf shape parsing and initialization. Currently due to downstream parsing support
-- this module only supports parsing DWARFv4. 'loadDwarfPreconditions' will attempt to find a
-- a DWARF prototype for a target entrypoint and build an initial shape based on the high-level types.
--
-- c.f. "Grease.LLVM.DebugInfo".
--
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Dwarf (loadDwarfPreconditions) where

import Control.Lens qualified as Lens
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.Coerce
import Data.ElfEdit qualified as Elf
import Data.Foldable (find)
import Data.Functor.Const qualified as Const
import Data.List qualified as List
import Data.Macaw.CFG (ArchSegmentOff)
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Dwarf (CompileUnit (cuRanges, cuSubprograms), Range (rangeBegin, rangeEnd), Subprogram (subParamMap), dwarfInfoFromElf)
import Data.Macaw.Dwarf qualified as MDwarf
import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Types qualified as MT
import Data.Map (toAscList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Word (Word64)
import Grease.Diagnostic (Diagnostic (DwarfShapesDiagnostic), GreaseLogAction)
import Grease.Macaw.Arch (ArchContext, archABIParams)
import Grease.Macaw.Dwarf.Diagnostic qualified as DwarfDiagnostic
import Grease.Macaw.RegName (RegName (..), mkRegName)
import Grease.Options (TypeUnrollingBound (..))
import Grease.Shape (ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (MemShape (Exactly, Initialized, Pointer, Uninitialized), Offset (Offset), PtrShape (ShapePtr, ShapePtrBV), PtrTarget, TaggedByte (..), memShapeSize, ptrTarget)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.LLVM.Bytes (toBytes)
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Types qualified as CT
import Lumberjack qualified as LJ

-- | Find the pc for DWARF which is relative to the ELF absent its base load address.
-- We find this address by finding the segment that corresponds to the target address
-- by checking for all ELF segments (mapped to Macaw segments)
-- and finding the ELF segment that corresponds to the Macaw segment for the target address.
-- The target mbNewAddr is then the found segments virtual address + the offset of the original addr.
addressToELFAddress ::
  Integral (Elf.ElfWordType (MC.ArchAddrWidth arch)) =>
  Elf.ElfHeaderInfo (MC.ArchAddrWidth arch) ->
  ArchSegmentOff arch ->
  MM.Memory (MC.RegAddrWidth (MC.ArchReg arch)) ->
  Proxy arch ->
  Maybe Word64
addressToELFAddress bin entAddr memory _ =
  do
    let segIndexToSegment = MM.memSegmentIndexMap memory
    let seg = MM.segoffSegment entAddr
    let targetSegment = fst <$> find (\(_, seg2) -> seg == seg2) (Map.toList segIndexToSegment)
    let mbSegment = find (\phdr -> (Just $ Elf.phdrSegmentIndex phdr) == targetSegment) $ Elf.headerPhdrs bin
    let mbNewAddr =
          do
            segHdr <- mbSegment
            pure $ (MM.memWordValue $ MM.segoffOffset entAddr) + fromIntegral (Elf.phdrSegmentVirtAddr segHdr)
    mbNewAddr

-- | For a given function address, attempts to find a DWARF subprogram
-- that represents that function and build a new 'Shape.ArgShapes' by writing
-- shapes built from high-level type information for each parameter to the initial minimal
-- shape.
loadDwarfPreconditions ::
  forall arch ext tys m.
  Symbolic.SymArchConstraints arch =>
  Integral (Elf.ElfWordType (MC.ArchAddrWidth arch)) =>
  ( MonadIO m
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , ExtShape ext
      ~ PtrShape ext (MC.RegAddrWidth (MC.ArchReg arch))
  ) =>
  -- | Log action
  GreaseLogAction ->
  -- | Target function address
  ArchSegmentOff arch ->
  -- | Loaded memory of ELF
  MM.Memory (MC.RegAddrWidth (MC.ArchReg arch)) ->
  -- | How much to unroll recursive types
  TypeUnrollingBound ->
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  Shape.ArgShapes ext NoTag tys ->
  -- | Elf Object
  Elf.ElfHeaderInfo (MC.ArchAddrWidth arch) ->
  -- | Architecture-specific information, needed for ABI info
  ArchContext arch ->
  m (Maybe (Shape.ArgShapes ext NoTag tys))
loadDwarfPreconditions gla targetAddr memory tyUnrollBound argNames initShapes elfHdr archContext =
  runMaybeT
    ( do
        let elf = snd $ Elf.getElf elfHdr
        adjustedAddr <- hoistMaybe $ addressToELFAddress elfHdr targetAddr memory (Proxy @arch)
        let (_, cus) = dwarfInfoFromElf elf
        shps <- MaybeT $ fromDwarfInfo gla archContext tyUnrollBound adjustedAddr cus
        let repl = Shape.replaceShapes argNames initShapes shps
        hoistMaybe $ either (const Nothing) Just repl
    )

extractType :: Subprogram -> MDwarf.TypeRef -> Either String MDwarf.TypeApp
extractType sprog ref =
  extractTypeInternal ref
    >>= ( \case
            (MDwarf.TypeQualType (MDwarf.TypeQualAnn{MDwarf.tqaType = Just tyref})) -> extractType sprog tyref
            (MDwarf.TypedefType (MDwarf.Typedef _ _ _ tyref)) -> extractType sprog tyref
            x -> Right x
        )
 where
  extractTypeInternal :: MDwarf.TypeRef -> Either String MDwarf.TypeApp
  extractTypeInternal vrTy =
    let mp = MDwarf.subTypeMap sprog
     in case Map.lookup vrTy mp of
          Nothing -> Left $ "No type for typeref: " ++ show vrTy
          Just (a, _) -> a

constructPtrTarget ::
  CLM.HasPtrWidth w =>
  TypeUnrollingBound ->
  Subprogram ->
  VisitCount ->
  MDwarf.TypeApp ->
  Either String (PtrTarget w NoTag)
constructPtrTarget tyUnrollBound sprog visitCount tyApp =
  ptrTarget Nothing <$> shapeSeq tyApp
 where
  ishape w = Right $ Seq.singleton $ Initialized NoTag (toBytes w)
  padding :: Integral a => a -> MemShape w NoTag
  padding w = Uninitialized (toBytes w)
  -- if we dont know where to place members we have to fail/just place some bytes
  buildMember :: CLM.HasPtrWidth w => Word64 -> MDwarf.Member -> Either String (Word64, Seq.Seq (MemShape w NoTag))
  buildMember loc member =
    do
      memLoc <- case MDwarf.memberLoc member of
        Nothing -> Left $ "Expected location for member: " ++ show member
        Just x -> Right x
      padd <- Right (if memLoc == loc then Seq.empty else Seq.singleton (padding $ memLoc - loc))
      memberShapes <- shapeOfTyApp $ MDwarf.memberType member
      let memByteSize = sum $ memShapeSize ?ptrWidth <$> memberShapes
      let nextLoc = memLoc + fromIntegral memByteSize
      Right (nextLoc, padd Seq.>< memberShapes)

  shapeOfTyApp :: CLM.HasPtrWidth w => MDwarf.TypeRef -> Either String (Seq.Seq (MemShape w NoTag))
  shapeOfTyApp x = shapeSeq =<< extractType sprog =<< pure x

  shapeSeq :: CLM.HasPtrWidth w => MDwarf.TypeApp -> Either String (Seq.Seq (MemShape w NoTag))
  shapeSeq (MDwarf.UnsignedIntType w) = ishape w
  shapeSeq (MDwarf.SignedIntType w) = ishape w
  shapeSeq MDwarf.SignedCharType = ishape (1 :: Int)
  shapeSeq MDwarf.UnsignedCharType = ishape (1 :: Int)
  -- TODO(#263): Need modification to DWARF to collect DW_TAG_count and properly evaluate dwarf ops for upper bounds in subranges
  shapeSeq (MDwarf.ArrayType _elTy _ub) = Left "Array types currently unsupported due to GREASE's DWARF parser, see https://github.com/GaloisInc/grease/issues/263"
  -- need to compute padding between elements, in-front of the first element, and at end
  -- we could get a missing DWARF member per the format so we have to do something about padding in-front
  -- even if in practice this should not occur.
  shapeSeq (MDwarf.StructType sdecl) =
    let structSize = MDwarf.structByteSize sdecl
     in do
          (endLoc, seqs) <-
            foldM
              ( \(currLoc, seqs) mem ->
                  do
                    (nextLoc, additionalShapes) <- buildMember currLoc mem
                    Right (nextLoc, seqs Seq.>< additionalShapes)
              )
              (0, Seq.empty)
              (MDwarf.structMembers sdecl)
          let endPad = if endLoc >= structSize then Seq.empty else Seq.singleton (padding $ structSize - endLoc)
          Right $ (seqs Seq.>< endPad)
  shapeSeq (MDwarf.PointerType _ maybeRef) =
    let mshape =
          constructPtrMemShapeFromRef tyUnrollBound sprog visitCount
            =<< Maybe.maybe (Left $ "Pointer missing pointee") Right maybeRef
     in Seq.singleton <$> mshape
  shapeSeq ty = Left $ "Unsupported dwarf type: " ++ show ty

type VisitCount = Map.Map MDwarf.TypeRef Int

nullPtr :: CLM.HasPtrWidth w => MemShape w NoTag
nullPtr =
  let zbyt = TaggedByte{taggedByteTag = NoTag, taggedByteValue = 0}
   in Exactly (take (fromInteger $ CT.intValue ?ptrWidth `div` 8) $ repeat $ zbyt)

constructPtrMemShapeFromRef ::
  CLM.HasPtrWidth w =>
  TypeUnrollingBound ->
  Subprogram ->
  VisitCount ->
  MDwarf.TypeRef ->
  Either String (MemShape w NoTag)
constructPtrMemShapeFromRef bound sprog vcount ref =
  let TypeUnrollingBound tyUnrollBound = bound
      ct = fromMaybe 0 (Map.lookup ref vcount)
      newMap = Map.insert ref (ct + 1) vcount
   in if ct > tyUnrollBound
        then
          pure $ nullPtr
        else do
          ty <- extractType sprog ref
          tgt <- constructPtrTarget bound sprog newMap ty
          pure $ Pointer NoTag (Offset 0) tgt

intPtrShape ::
  Symbolic.SymArchConstraints arch =>
  MC.ArchReg arch tp ->
  Either String (C.Some (PtrShape ext w NoTag))
intPtrShape reg =
  case MT.typeRepr reg of
    MT.BVTypeRepr w -> Right $ C.Some $ ShapePtrBV NoTag w
    ty ->
      let rname = mkRegName reg
       in Left $ "Register: " ++ show rname ++ " assigned integer type when has type: " ++ show ty

doLog :: (MonadIO m) => GreaseLogAction -> DwarfDiagnostic.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (DwarfShapesDiagnostic diag)

pointerShapeOfDwarf ::
  (CLM.HasPtrWidth w, Symbolic.SymArchConstraints arch) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.TypeApp ->
  Either String (C.Some (PtrShape ext w NoTag))
pointerShapeOfDwarf _ _ r _ (MDwarf.SignedIntType _) = intPtrShape r
pointerShapeOfDwarf _ _ r _ (MDwarf.UnsignedIntType _) = intPtrShape r
pointerShapeOfDwarf _ tyUnrollBound _ sprog (MDwarf.PointerType _ tyRef) =
  let memShape = constructPtrTarget tyUnrollBound sprog Map.empty =<< extractType sprog =<< Maybe.maybe (Left $ "Pointer missing pointee") Right tyRef
      pointerShape = ShapePtr NoTag (Offset 0) <$> memShape
   in (C.Some <$> pointerShape)
pointerShapeOfDwarf _ _ _ _ ty = Left $ "Unsupported base dwarf type: " ++ show ty

-- Stops after the first nothing to avoid adding shapes after
-- failing to build some shape (this would result in shapes applying to incorrect registers)
takeRight :: (a -> Either e b) -> [a] -> ([b], Maybe (a, e))
takeRight _ [] = ([], Nothing)
takeRight f (h : tl) =
  case f h of
    Left e -> ([], Just (h, e))
    Right b -> let (lst, e) = takeRight f tl in (b : lst, e)

shapeFromVar ::
  ( ExtShape ext ~ PtrShape ext wptr
  , CLM.HasPtrWidth wptr
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.Variable ->
  Either String (C.Some (Shape.Shape ext NoTag))
shapeFromVar arch tyUnrollBound buildingForReg sprog vr =
  C.mapSome Shape.ShapeExt
    <$> ( pointerShapeOfDwarf arch tyUnrollBound buildingForReg sprog
            =<< extractType sprog
            =<< Maybe.maybe (Left $ "Variable missing type: " ++ show vr) Right (MDwarf.varType vr)
        )

shapeFromDwarf ::
  (MonadIO m, Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) =>
  GreaseLogAction ->
  ArchContext arch ->
  TypeUnrollingBound ->
  Subprogram ->
  m (Shape.ParsedShapes ext)
shapeFromDwarf gla aContext tyUnrollBound sub =
  let
    abiRegs = aContext Lens.^. archABIParams
    args = (zip abiRegs $ snd <$> (toAscList $ subParamMap sub))
    regAssignmentFromDwarfVar reg var =
      do
        C.Some r <- pure reg
        shp <- shapeFromVar aContext tyUnrollBound r sub var
        pure (Text.pack $ coerce $ mkRegName r, shp)
    (ascParams, err) =
      takeRight
        (uncurry regAssignmentFromDwarfVar)
        args
   in
    do
      Maybe.maybe
        (pure ())
        ( \((_, vr), errMsg) ->
            let msg = List.concat ["Stopped parsing at variable: \n", show vr, "\nVariable error message:\n", errMsg]
             in doLog gla $ DwarfDiagnostic.FailedToParse sub msg
        )
        err
      pure $ Shape.ParsedShapes{Shape._getParsedShapes = Map.fromList ascParams}

-- | Given a list of `Data.Macaw.Dwarf.CompileUnit` attempts to find a subprogram corresponding to
-- the provided PC and synthesize a shape from the DWARF provided prototype for the function.
-- The provided PC is relative to the base of the image (as is represented in DWARF).
fromDwarfInfo ::
  forall m arch ext wptr.
  (MonadIO m, Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) =>
  GreaseLogAction ->
  ArchContext arch ->
  TypeUnrollingBound ->
  -- | The entrypoint PC of the target subprogram relative to the target ELF object.
  Word64 ->
  [Data.Macaw.Dwarf.CompileUnit] ->
  m (Maybe (Shape.ParsedShapes ext))
fromDwarfInfo gla aContext tyUnrollBound addr cus =
  runMaybeT
    ( do
        targetCu <-
          hoistMaybe $
            List.find
              ( \x ->
                  let rs = cuRanges x
                      isInCU range =
                        let begin = rangeBegin range
                            end = rangeEnd range
                         in begin <= addr
                              && addr
                                < end
                   in any isInCU rs
              )
              cus
        targetSubProg <- hoistMaybe $ List.find (isInSubProg addr) (cuSubprograms targetCu)
        lift $
          shapeFromDwarf
            gla
            aContext
            tyUnrollBound
            targetSubProg
    )
 where
  isInSubProg ::
    Word64 ->
    Subprogram ->
    Bool
  isInSubProg w sub =
    let entryMatch = (==) w <$> MDwarf.subEntryPC sub
        def = fromMaybe False
     in def
          ( do
              sdef <- MDwarf.subDef sub
              lpc <- MDwarf.subLowPC sdef
              hpc <- MDwarf.subHighPC sdef
              pure $ w >= lpc && w < (lpc + hpc)
          )
          || def entryMatch
