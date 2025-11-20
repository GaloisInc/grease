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
import Data.Parameterized.Context qualified as Ctx
import Data.Proxy (Proxy (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Word (Word64)
import Grease.Macaw.Arch (ArchContext, archABIParams)
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
  forall arch ext tys.
  Symbolic.SymArchConstraints arch =>
  Integral (Elf.ElfWordType (MC.ArchAddrWidth arch)) =>
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , MM.MemWidth (MC.ArchAddrWidth arch)
  , ExtShape ext
      ~ PtrShape ext (MC.RegAddrWidth (MC.ArchReg arch))
  ) =>
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
  Maybe (Shape.ArgShapes ext NoTag tys)
loadDwarfPreconditions targetAddr memory tyUnrollBound argNames initShapes elfHdr archContext =
  do
    let elf = snd $ Elf.getElf elfHdr
    adjustedAddr <- addressToELFAddress elfHdr targetAddr memory (Proxy @arch)
    let (_, cus) = dwarfInfoFromElf elf
    shps <- fromDwarfInfo archContext tyUnrollBound adjustedAddr cus
    let repl = Shape.replaceShapes argNames initShapes shps
    either (const Nothing) Just repl

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

extractType :: Subprogram -> MDwarf.TypeRef -> Maybe MDwarf.TypeApp
extractType sprog ref =
  extractTypeInternal ref
    >>= ( \case
            (MDwarf.TypeQualType (MDwarf.TypeQualAnn{MDwarf.tqaType = Just tyref})) -> extractType sprog tyref
            (MDwarf.TypedefType (MDwarf.Typedef _ _ _ tyref)) -> extractType sprog tyref
            x -> Just x
        )
 where
  extractTypeInternal :: MDwarf.TypeRef -> Maybe MDwarf.TypeApp
  extractTypeInternal vrTy =
    do
      let mp = MDwarf.subTypeMap sprog
      (mbtypeApp, _) <- Map.lookup vrTy mp
      rightToMaybe mbtypeApp

constructPtrTarget ::
  CLM.HasPtrWidth w =>
  TypeUnrollingBound ->
  Subprogram ->
  VisitCount ->
  MDwarf.TypeApp ->
  Maybe (PtrTarget w NoTag)
constructPtrTarget tyUnrollBound sprog visitCount tyApp =
  ptrTarget Nothing <$> shapeSeq tyApp
 where
  ishape w = Just $ Seq.singleton $ Initialized NoTag (toBytes w)
  padding :: Integral a => a -> MemShape w NoTag
  padding w = Uninitialized (toBytes w)
  -- if we dont know where to place members we have to fail/just place some bytes
  buildMember :: CLM.HasPtrWidth w => Word64 -> MDwarf.Member -> Maybe (Word64, Seq.Seq (MemShape w NoTag))
  buildMember loc member =
    do
      memLoc <- MDwarf.memberLoc member
      padd <- Just (if memLoc == loc then Seq.empty else Seq.singleton (padding $ memLoc - loc))
      memberShapes <- shapeOfTyApp $ MDwarf.memberType member
      let memByteSize = sum $ memShapeSize ?ptrWidth <$> memberShapes
      let nextLoc = memLoc + fromIntegral memByteSize
      Just (nextLoc, padd Seq.>< memberShapes)

  shapeOfTyApp :: CLM.HasPtrWidth w => MDwarf.TypeRef -> Maybe (Seq.Seq (MemShape w NoTag))
  shapeOfTyApp x = shapeSeq =<< extractType sprog =<< pure x

  shapeSeq :: CLM.HasPtrWidth w => MDwarf.TypeApp -> Maybe (Seq.Seq (MemShape w NoTag))
  shapeSeq (MDwarf.UnsignedIntType w) = ishape w
  shapeSeq (MDwarf.SignedIntType w) = ishape w
  shapeSeq MDwarf.SignedCharType = ishape (1 :: Int)
  shapeSeq MDwarf.UnsignedCharType = ishape (1 :: Int)
  -- TODO(#263): Need modification to DWARF to collect DW_TAG_count and properly evaluate dwarf ops for upper bounds in subranges
  shapeSeq (MDwarf.ArrayType _elTy _ub) = Nothing
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
                    Just (nextLoc, seqs Seq.>< additionalShapes)
              )
              (0, Seq.empty)
              (MDwarf.structMembers sdecl)
          let endPad = if endLoc >= structSize then Seq.empty else Seq.singleton (padding $ structSize - endLoc)
          Just $ (seqs Seq.>< endPad)
  shapeSeq (MDwarf.PointerType _ maybeRef) =
    let mshape = constructPtrMemShapeFromRef tyUnrollBound sprog visitCount =<< maybeRef
     in Seq.singleton <$> mshape
  shapeSeq _ = Nothing

type VisitCount = Map.Map MDwarf.TypeRef Int

nullPtr :: CLM.HasPtrWidth w => MemShape w NoTag
nullPtr =
  let zbyt = TaggedByte{taggedByteTag = NoTag, taggedByteValue = 0}
   in Exactly (take (fromInteger $ CT.intValue ?ptrWidth `div` 8) $ repeat $ zbyt)

constructPtrMemShapeFromRef :: CLM.HasPtrWidth w => TypeUnrollingBound -> Subprogram -> VisitCount -> MDwarf.TypeRef -> Maybe (MemShape w NoTag)
constructPtrMemShapeFromRef bound@(TypeUnrollingBound tyUnrollBound) sprog vcount ref =
  let ct = fromMaybe 0 (Map.lookup ref vcount)
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
  Maybe (C.Some (PtrShape ext w NoTag))
intPtrShape reg =
  case MT.typeRepr reg of
    MT.BVTypeRepr w -> Just $ C.Some $ ShapePtrBV NoTag w
    _ -> Nothing

pointerShapeOfDwarf ::
  (CLM.HasPtrWidth w, Symbolic.SymArchConstraints arch) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.TypeApp ->
  Maybe (C.Some (PtrShape ext w NoTag))
pointerShapeOfDwarf _ _ r _ (MDwarf.SignedIntType _) = intPtrShape r
pointerShapeOfDwarf _ _ r _ (MDwarf.UnsignedIntType _) = intPtrShape r
pointerShapeOfDwarf archCtx tyUnrollBound r sprog (MDwarf.TypeQualType (MDwarf.TypeQualAnn{MDwarf.tqaType = Just tyref})) =
  pointerShapeOfDwarf archCtx tyUnrollBound r sprog =<< extractType sprog tyref
pointerShapeOfDwarf _ tyUnrollBound _ sprog (MDwarf.PointerType _ tyRef) =
  let memShape = constructPtrTarget tyUnrollBound sprog Map.empty =<< extractType sprog =<< tyRef
      pointerShape = ShapePtr NoTag (Offset 0) <$> memShape
   in (C.Some <$> pointerShape)
pointerShapeOfDwarf _ _ _ _ _ = Nothing

-- Stops after the first nothing to avoid adding shapes after
-- failing to build some shape (this would result in shapes applying to incorrect registers)
takeJust :: (a -> Maybe b) -> [a] -> [b]
takeJust _ [] = []
takeJust f (h : tl) =
  case f h of
    Nothing -> []
    Just e -> e : takeJust f tl

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
  Maybe (C.Some (Shape.Shape ext NoTag))
shapeFromVar arch tyUnrollBound buildingForReg sprog vr =
  C.mapSome Shape.ShapeExt
    <$> ( pointerShapeOfDwarf arch tyUnrollBound buildingForReg sprog
            =<< extractType sprog
            =<< MDwarf.varType vr
        )

shapeFromDwarf :: (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) => ArchContext arch -> TypeUnrollingBound -> Subprogram -> Shape.ParsedShapes ext
shapeFromDwarf aContext tyUnrollBound sub =
  let
    abiRegs = aContext Lens.^. archABIParams
    args = (zip abiRegs $ snd <$> (toAscList $ subParamMap sub))
    regAssignmentFromDwarfVar reg var =
      do
        C.Some r <- pure reg
        shp <- shapeFromVar aContext tyUnrollBound r sub var
        pure (Text.pack $ coerce $ mkRegName r, shp)
    ascParams =
      takeJust
        (uncurry regAssignmentFromDwarfVar)
        args
   in
    Shape.ParsedShapes{Shape._getParsedShapes = Map.fromList ascParams}

-- | Given a list of `Data.Macaw.Dwarf.CompileUnit` attempts to find a subprogram corresponding to
-- the provided PC and synthesize a shape from the DWARF provided prototype for the function.
-- The provided PC is relative to the base of the image (as is represented in DWARF).
fromDwarfInfo ::
  (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) =>
  ArchContext arch ->
  TypeUnrollingBound ->
  -- | The entrypoint PC of the target subprogram relative to the target ELF object.
  Word64 ->
  [Data.Macaw.Dwarf.CompileUnit] ->
  Maybe (Shape.ParsedShapes ext)
fromDwarfInfo aContext tyUnrollBound addr cus =
  do
    targetCu <-
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
    targetSubProg <- List.find (isInSubProg addr) (cuSubprograms targetCu)
    pure $
      shapeFromDwarf
        aContext
        tyUnrollBound
        targetSubProg
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
