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
import Control.Monad (foldM, join)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadIO (..), MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.Coerce
import Data.ElfEdit qualified as Elf
import Data.Foldable (find, for_)
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
  forall arch ext tys.
  Symbolic.SymArchConstraints arch =>
  Integral (Elf.ElfWordType (MC.ArchAddrWidth arch)) =>
  ( CLM.HasPtrWidth (MC.ArchAddrWidth arch)
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
  IO (Maybe (Shape.ArgShapes ext NoTag tys))
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

type ShapeParsingM a = ExceptT DwarfDiagnostic.DwarfShapeParsingError IO a

extractType :: Subprogram -> MDwarf.TypeRef -> ShapeParsingM MDwarf.TypeApp
extractType sprog ref =
  extractTypeInternal ref
    >>= ( \case
            (MDwarf.TypeQualType (MDwarf.TypeQualAnn{MDwarf.tqaType = Just tyref})) -> extractType sprog tyref
            (MDwarf.TypedefType (MDwarf.Typedef _ _ _ tyref)) -> extractType sprog tyref
            x -> except $ Right x
        )
 where
  extractTypeInternal :: MDwarf.TypeRef -> ShapeParsingM MDwarf.TypeApp
  extractTypeInternal vrTy =
    let mp = MDwarf.subTypeMap sprog
     in case Map.lookup vrTy mp of
          Nothing -> except $ Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "No type for typeref: " ++ show vrTy
          Just (a, _) -> case a of
            Left x -> except $ Left $ DwarfDiagnostic.UnexpectedDWARFForm x
            Right x -> except $ Right x

constructPtrTarget ::
  CLM.HasPtrWidth w =>
  GreaseLogAction ->
  TypeUnrollingBound ->
  Subprogram ->
  VisitCount ->
  MDwarf.TypeApp ->
  IO (PtrTarget w NoTag)
constructPtrTarget gla tyUnrollBound sprog visitCount tyApp =
  ptrTarget Nothing
    <$> liftIO
      ( do
          mbe <- runExceptT $ shapeSeq tyApp
          case mbe of
            Left e -> do
              doLog gla (DwarfDiagnostic.UsingDefaultForPointer sprog e)
              pure Seq.empty
            Right x -> pure x
      )
 where
  ishape w = except $ Right $ Seq.singleton $ Initialized NoTag (toBytes w)
  padding :: Integral a => a -> MemShape w NoTag
  padding w = Uninitialized (toBytes w)
  -- if we dont know where to place members we have to fail/just place some bytes
  buildMember :: CLM.HasPtrWidth w => Word64 -> MDwarf.Member -> ShapeParsingM (Word64, Seq.Seq (MemShape w NoTag))
  buildMember loc member =
    do
      memLoc <- case MDwarf.memberLoc member of
        Nothing -> except $ Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "Expected location for member: " ++ show member
        Just x -> except $ Right x
      padd <- except $ Right (if memLoc == loc then Seq.empty else Seq.singleton (padding $ memLoc - loc))
      memberShapes <- shapeOfTyApp $ MDwarf.memberType member
      let memByteSize = sum $ memShapeSize ?ptrWidth <$> memberShapes
      let nextLoc = memLoc + fromIntegral memByteSize
      except $ Right (nextLoc, padd Seq.>< memberShapes)

  shapeOfTyApp :: CLM.HasPtrWidth w => MDwarf.TypeRef -> ShapeParsingM (Seq.Seq (MemShape w NoTag))
  shapeOfTyApp x = shapeSeq =<< extractType sprog =<< pure x

  numOfRange :: MDwarf.SubrangeBounds -> ShapeParsingM Int
  numOfRange =
    \case
      MDwarf.SubrangeCount val -> except $ Right $ fromIntegral val
      MDwarf.SubrangeUpperBound val -> case val of
        [MDwarf.DW_OP_const8u w] -> except $ Right $ fromIntegral w
        _ -> except $ Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "Array types represented with DW_AT_upper_bound that do not have a value of DW_ATVAL_UINT are not supported"

  shapeSeq :: CLM.HasPtrWidth w => MDwarf.TypeApp -> ShapeParsingM (Seq.Seq (MemShape w NoTag))
  shapeSeq (MDwarf.UnsignedIntType w) = ishape w
  shapeSeq (MDwarf.SignedIntType w) = ishape w
  shapeSeq MDwarf.SignedCharType = ishape (1 :: Int)
  shapeSeq MDwarf.UnsignedCharType = ishape (1 :: Int)
  shapeSeq (MDwarf.ArrayType elTy ub) =
    do
      let onlySubrange = if List.length ub == 1 then Maybe.listToMaybe ub else Nothing
      ubRange <- case onlySubrange of
        Just x -> except $ Right x
        Nothing -> except $ Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "Expected only one DW_AT_subrange_type for DW_TAG_array_type"
      num <- numOfRange (MDwarf.subrangeUpperBound ubRange)
      tyShape <- shapeOfTyApp elTy
      pure $ join $ Seq.replicate num tyShape
  -- need to compute padding between elements, in-front of the first element, and at end
  -- we could get a missing DWARF member per the format so we have to do something about padding in-front
  -- even if in practice this should not occur.
  shapeSeq (MDwarf.StructType sdecl) =
    let structSize = MDwarf.structByteSize sdecl
     in do
          let builtMembers =
                foldM
                  ( \(currLoc, seqs) mem ->
                      do
                        (nextLoc, additionalShapes) <- withExceptT (\e -> ((currLoc, seqs), e)) (buildMember currLoc mem)
                        except $ Right (nextLoc, seqs Seq.>< additionalShapes)
                  )
                  (0, Seq.empty)
                  (MDwarf.structMembers sdecl)
          (endLoc, seqs) <- liftIO $ do
            x <- runExceptT builtMembers
            case x of
              Left ((off, previous), e) -> doLog gla (DwarfDiagnostic.StoppingStruct sprog off e) >> pure (off, previous)
              Right res -> pure res

          let endPad = if endLoc >= structSize then Seq.empty else Seq.singleton (padding $ structSize - endLoc)
          except $ Right $ (seqs Seq.>< endPad)
  shapeSeq (MDwarf.PointerType _ maybeRef) =
    let mshape =
          constructPtrMemShapeFromRef gla tyUnrollBound sprog visitCount
            =<< (except $ Maybe.maybe (Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "Pointer missing pointee") Right maybeRef)
     in Seq.singleton <$> mshape
  shapeSeq ty = except $ Left $ DwarfDiagnostic.UnsupportedType ty

type VisitCount = Map.Map MDwarf.TypeRef Int

nullPtr :: CLM.HasPtrWidth w => MemShape w NoTag
nullPtr =
  let zbyt = TaggedByte{taggedByteTag = NoTag, taggedByteValue = 0}
   in Exactly (take (fromInteger $ CT.intValue ?ptrWidth `div` 8) $ repeat $ zbyt)

constructPtrMemShapeFromRef ::
  CLM.HasPtrWidth w =>
  GreaseLogAction ->
  TypeUnrollingBound ->
  Subprogram ->
  VisitCount ->
  MDwarf.TypeRef ->
  ShapeParsingM (MemShape w NoTag)
constructPtrMemShapeFromRef gla bound sprog vcount ref =
  let TypeUnrollingBound tyUnrollBound = bound
      ct = fromMaybe 0 (Map.lookup ref vcount)
      newMap = Map.insert ref (ct + 1) vcount
   in if ct > tyUnrollBound
        then
          pure $ nullPtr
        else do
          ty <- extractType sprog ref
          tgt <- liftIO $ constructPtrTarget gla bound sprog newMap ty
          pure $ Pointer NoTag (Offset 0) tgt

intPtrShape ::
  Symbolic.SymArchConstraints arch =>
  MC.ArchReg arch tp ->
  Either DwarfDiagnostic.DwarfShapeParsingError (C.Some (PtrShape ext w NoTag))
intPtrShape reg =
  case MT.typeRepr reg of
    MT.BVTypeRepr w -> Right $ C.Some $ ShapePtrBV NoTag w
    ty ->
      let rname = mkRegName reg
       in Left $ DwarfDiagnostic.TypeMistmatchForRegister rname "BVType" ty

doLog :: (MonadIO m) => GreaseLogAction -> DwarfDiagnostic.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (DwarfShapesDiagnostic diag)

pointerShapeOfDwarf ::
  (CLM.HasPtrWidth w, Symbolic.SymArchConstraints arch) =>
  ArchContext arch ->
  GreaseLogAction ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.TypeApp ->
  ShapeParsingM (C.Some (PtrShape ext w NoTag))
pointerShapeOfDwarf _ _ _ r _ (MDwarf.SignedIntType _) = except $ intPtrShape r
pointerShapeOfDwarf _ _ _ r _ (MDwarf.UnsignedIntType _) = except $ intPtrShape r
pointerShapeOfDwarf _ gla tyUnrollBound _ sprog (MDwarf.PointerType _ mbTyRef) =
  let memShape = do
        tyRef <- except $ Maybe.maybe (Left $ DwarfDiagnostic.UnexpectedDWARFForm $ "Pointer missing pointee") Right mbTyRef
        typeApp <- extractType sprog tyRef
        liftIO $ constructPtrTarget gla tyUnrollBound sprog Map.empty typeApp
      pointerShape = ShapePtr NoTag (Offset 0) <$> memShape
   in (C.Some <$> pointerShape)
pointerShapeOfDwarf _ _ _ _ _ ty = except $ Left $ DwarfDiagnostic.UnsupportedType ty

-- Stops after the first 'Left' to avoid adding shapes after
-- failing to build some shape (this would result in shapes applying to incorrect registers)
takeUntilError :: (Monad m) => (a -> ExceptT e m b) -> [a] -> m ([b], Maybe (a, e))
takeUntilError _ [] = pure $ ([], Nothing)
takeUntilError f (h : tl) = do
  c <- runExceptT $ f h
  case c of
    Left e -> pure $ ([], Just (h, e))
    Right b -> do
      (rst, maybeErr) <- takeUntilError f tl
      pure $ (b : rst, maybeErr)

shapeFromVar ::
  ( ExtShape ext ~ PtrShape ext wptr
  , CLM.HasPtrWidth wptr
  , Symbolic.SymArchConstraints arch
  ) =>
  ArchContext arch ->
  GreaseLogAction ->
  TypeUnrollingBound ->
  MC.ArchReg arch tp ->
  Subprogram ->
  MDwarf.Variable ->
  ShapeParsingM (C.Some (Shape.Shape ext NoTag))
shapeFromVar arch gla tyUnrollBound buildingForReg sprog vr =
  C.mapSome Shape.ShapeExt
    <$> ( pointerShapeOfDwarf arch gla tyUnrollBound buildingForReg sprog
            =<< extractType sprog
            =<< (except $ Maybe.maybe (Left $ DwarfDiagnostic.UnexpectedDWARFForm "Variable is missing a type") Right (MDwarf.varType vr))
        )

shapeFromDwarf ::
  forall arch ext wptr.
  (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) =>
  GreaseLogAction ->
  ArchContext arch ->
  TypeUnrollingBound ->
  Subprogram ->
  IO (Shape.ParsedShapes ext)
shapeFromDwarf gla aContext tyUnrollBound sub =
  let
    abiRegs = aContext Lens.^. archABIParams
    args = (zip abiRegs $ snd <$> (toAscList $ subParamMap sub))
    regAssignmentFromDwarfVar :: C.Some (MC.ArchReg arch) -> MDwarf.Variable -> ShapeParsingM (Text.Text, C.Some (Shape.Shape ext NoTag))
    regAssignmentFromDwarfVar reg var =
      do
        C.Some r <- pure reg
        shp <- shapeFromVar aContext gla tyUnrollBound r sub var
        pure (Text.pack $ coerce $ mkRegName r, shp)
   in
    do
      (ascParams, err) <-
        liftIO $
          takeUntilError
            (uncurry regAssignmentFromDwarfVar)
            args
      liftIO $
        for_ @Maybe
          err
          ( \((_, vr), errMsg) ->
              doLog gla $ DwarfDiagnostic.FailedToParse sub vr errMsg
          )
      pure $ Shape.ParsedShapes{Shape._getParsedShapes = Map.fromList ascParams}

-- | Given a list of `Data.Macaw.Dwarf.CompileUnit` attempts to find a subprogram corresponding to
-- the provided PC and synthesize a shape from the DWARF provided prototype for the function.
-- The provided PC is relative to the base of the image (as is represented in DWARF).
fromDwarfInfo ::
  forall arch ext wptr.
  (Symbolic.SymArchConstraints arch, ExtShape ext ~ PtrShape ext wptr, CLM.HasPtrWidth wptr) =>
  GreaseLogAction ->
  ArchContext arch ->
  TypeUnrollingBound ->
  -- | The entrypoint PC of the target subprogram relative to the target ELF object.
  Word64 ->
  [Data.Macaw.Dwarf.CompileUnit] ->
  IO (Maybe (Shape.ParsedShapes ext))
fromDwarfInfo gla aContext tyUnrollBound addr cus =
  runMaybeT
    ( do
        let isTargetAddrInCu cu =
              let rs = cuRanges cu
                  isInCU range =
                    let begin = rangeBegin range
                        end = rangeEnd range
                     in begin <= addr
                          && addr
                            < end
               in any isInCU rs
        targetCu <-
          hoistMaybe $
            List.find
              isTargetAddrInCu
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
