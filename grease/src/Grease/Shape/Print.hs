{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grease.Shape.Print
  ( PrinterConfig(..)
  , Printer
  , evalPrinter
  , printNamedShapesFiltered
  , printNamedShapes
  , printNamed
  , print
  ) where


import Prelude hiding (print)
import Control.Monad qualified as Monad
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Reader qualified as ReaderT
import Data.BitVector.Sized (BV)
import Data.BitVector.Sized qualified as BV
import Data.Foldable qualified as Foldable
import Data.Functor qualified as Functor
import Data.Functor.Const qualified as Const
import Data.Functor.Product qualified as Product
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Macaw.Memory (AddrWidthRepr)
import Data.Macaw.Memory qualified as DMM
import Data.Monoid qualified as Monoid
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (NatRepr)
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text (Text)
import Data.Tuple qualified as Tuple
import Data.Void (Void)
import Data.Void qualified as Void
import Grease.Shape (Shape, ExtShape)
import Grease.Shape qualified as Shape
import Grease.Shape.Pointer (PtrShape, BlockId(BlockId))
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Numeric (showHex)
import Prettyprinter qualified as PP

data Allocs
  = Allocs
    { allocs :: IntMap (PP.Doc Void)
    , maxKey :: {-# UNPACK #-} !Int
    }

allocNext :: Allocs -> (BlockId, Allocs)
allocNext as = let k = maxKey as in (BlockId k, as { maxKey = k + 1 })

emptyAllocs :: Allocs
emptyAllocs = Allocs IntMap.empty 0

data PrinterConfig w = PrinterConfig
  { cfgAddrWidth :: AddrWidthRepr w
    -- | Threshold for applying run-length encoding (RLE) to sequences of
    -- initialized, symbolic bytes or uninitialized bytes.
  , cfgRleThreshold :: Int
  }

-- | 'ReaderT'/'State' monad for pretty-printing
newtype Printer w a
  = Printer { runPrinter :: ReaderT.ReaderT (PrinterConfig w) (State Allocs) a }
  deriving (Applicative, Functor, Monad)

deriving instance MonadReader (PrinterConfig w) (Printer w)
deriving instance MonadState Allocs (Printer w)

-- | Run a 'Printer', printing all of the 'Shape's and then the allocations.
evalPrinter :: PrinterConfig w -> Printer w (PP.Doc ann) -> PP.Doc ann
evalPrinter cfg p =
  let (doc, as) =
        State.runState (ReaderT.runReaderT (runPrinter p) cfg) emptyAllocs in
  if IntMap.null (allocs as)
  then doc
  else doc PP.<> PP.line PP.<> PP.line PP.<> printAllocs (cfgAddrWidth cfg) as

printerAlloc :: Printer w (PP.Doc Void) -> Maybe BlockId -> Printer w BlockId
printerAlloc computeDoc bid = do
  -- The sequencing of block number allocations is important for legibility: It's
  -- nicer when pointers have lower block numbers than the pointers inside the
  -- allocation that they point to, because then allocation numbers increase
  -- while reading right-to-left, top-to-bottom.
  as <- State.get
  let (blk@(BlockId k), as') = (case bid of
        Nothing -> allocNext as
        Just bid' -> (bid', as))
  State.put as'
  doc <- computeDoc
  State.modify (\as''-> as'' { allocs = IntMap.insert k doc (allocs as'') })
  pure blk

addrWidth :: Printer w (AddrWidthRepr w)
addrWidth = Printer (ReaderT.asks cfgAddrWidth)

rleThreshold :: Printer w Int
rleThreshold = Printer (ReaderT.asks cfgRleThreshold)

printAllocs :: AddrWidthRepr w -> Allocs -> PP.Doc ann
printAllocs aw as =
  let padding =
        -- See doc/shape-dsl.md for justification for these numbers.
        case aw of
          DMM.Addr32 -> 2
          DMM.Addr64 -> 6
      printPair blk doc =
        PP.pretty (padHex padding blk) PP.<> PP.pretty (": " :: Text) PP.<> doc
      docs =
        List.map
          (Functor.fmap Void.absurd . Tuple.uncurry printPair)
          (IntMap.toAscList (allocs as))
  in PP.vsep docs

-- | Print 'Shape's associated with given names (e.g., register names)
printNamedShapesFiltered ::
  PP.Pretty nm =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Names
  Ctx.Assignment (Const.Const nm) tys ->
  -- | Only print those 'Shape's with 'True' in the corresponding entry here
  Ctx.Assignment (Const.Const Bool) tys ->
  Ctx.Assignment (Shape ext tag) tys ->
  Printer w (PP.Doc ann)
printNamedShapesFiltered names filt shapes =
  TFC.foldlMFC'
    (\doc (Product.Pair (Product.Pair (Const.Const nm) s) (Const.Const b)) ->
      if b
      then ((doc PP.<> PP.line) PP.<>) Functor.<$> printNamed nm s
      else pure doc)
    Monoid.mempty
    (Ctx.zipWith Product.Pair (Ctx.zipWith Product.Pair names shapes) filt)

-- | Print 'Shape's associated with given names (e.g., register names)
printNamedShapes ::
  PP.Pretty nm =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Names
  Ctx.Assignment (Const.Const nm) tys ->
  Ctx.Assignment (Shape ext tag) tys ->
  Printer w (PP.Doc ann)
printNamedShapes names =
  printNamedShapesFiltered
    names
    (Ctx.generate (Ctx.size names) (\_ -> Const.Const True))

-- | Print a single named 'Shape'
--
-- Ignores @tag@s.
printNamed ::
  PP.Pretty nm =>
  ExtShape ext ~ PtrShape ext w =>
  -- | Name
  nm ->
  Shape ext tag ty ->
  Printer w (PP.Doc ann)
printNamed name s =
  ((PP.pretty name PP.<> ": ") PP.<>) Functor.<$> print s

-- | Print a single 'Shape'
--
-- Ignores @tag@s.
print ::
  ExtShape ext ~ PtrShape ext w =>
  Shape ext tag ty ->
  Printer w (PP.Doc ann)
print =
  \case
    Shape.ShapeBool _tag -> pure "bool"
    Shape.ShapeUnit _tag -> pure "unit"
    Shape.ShapeStruct _tag fields -> printStruct fields
    Shape.ShapeExt ext -> printPtr ext

-- | Ignores @tag@s.
printStruct ::
  ExtShape ext ~ PtrShape ext w =>
  Ctx.Assignment (Shape ext tag) ctx ->
  Printer w (PP.Doc ann)
printStruct fields = do
  docs <- Monad.sequence (TFC.toListFC print fields)
  pure (PP.braces (PP.hcat (List.intersperse ", " docs)))

-- | Ignores @tag@s.
printPtr :: PtrShape ext w tag ty -> Printer w (PP.Doc ann)
printPtr =
  \case
    PtrShape.ShapePtrBV _tag w -> printBv w
    PtrShape.ShapePtrBVLit _tag w bv -> printBvLit w bv
    PtrShape.ShapePtr _tag offset tgt@(PtrShape.PtrTarget bid _) -> do
      blk <- printerAlloc (printTgt tgt) bid 
      printBlockOffset blk offset

printBv :: NatRepr w' -> Printer w (PP.Doc ann)
printBv w' = do
  -- TODO: assert that width % 8 == 0?
  let bytes = NatRepr.widthVal w' `div` 8
  let prettyByte = List.replicate bytes (PP.pretty ("XX" :: Text))
  pure (PP.hsep prettyByte)

printBvLit :: NatRepr w' -> BV w' -> Printer w (PP.Doc ann)
printBvLit w' bv =
  -- On `4`: There are 8 bits in a byte, and two hex digits denote one byte
  let hexDigits = NatRepr.widthVal w' `div` 4 in
  pure (PP.pretty (padHex hexDigits (BV.asUnsigned bv)))

printBlockOffset :: BlockId -> PtrShape.Offset -> Printer w (PP.Doc ann)
printBlockOffset blk off = do
  b <- printBlk blk
  o <- printOff off
  pure (b PP.<> "+" PP.<> o)

printBlk :: BlockId -> Printer w (PP.Doc ann)
printBlk (BlockId blk) = do
  aw <- addrWidth
  let padding =
        -- See doc/shape-dsl.md for justification for these numbers.
        case aw of
          DMM.Addr32 -> 2
          DMM.Addr64 -> 6
  pure (PP.pretty (padHex padding blk))

printOff :: PtrShape.Offset -> Printer w (PP.Doc ann)
printOff (PtrShape.Offset off) = do
  aw <- addrWidth
  let padding =
        -- See doc/shape-dsl.md for justification for these numbers.
        case aw of
          DMM.Addr32 -> 8
          DMM.Addr64 -> 16
  let off' = integerToInt (Bytes.bytesToInteger off)
  pure (PP.pretty (padHex padding off'))

-- | Helper, not exported
showHex' :: Integral a => a -> String
showHex' n = showHex n ""

padHex :: Integral a => Int -> a -> String
padHex pad v =
  let initial = showHex' v
      zs = List.take (pad - List.length initial) (List.repeat '0')
  in zs List.++ initial

-- | RLE-encode either @##@ or @XX@ if length > 8
printRle :: Char -> Int -> Printer w (PP.Doc ann)
printRle c n = do
  t <- rleThreshold
  pure $
    if n <= t
    then PP.fillSep (List.replicate n s)
    else s PP.<> PP.pretty ("*" :: Text)  PP.<> PP.pretty (showHex' n)
  where s = PP.pretty @[Char] [c, c]

-- | Ignores @tag@s.
printTgt :: PtrShape.PtrTarget w tag -> Printer w (PP.Doc Void)
printTgt (PtrShape.PtrTarget _ memShapes) = do
  docs <- traverse printMemShape memShapes
  pure (PP.align (PP.fillSep (Foldable.toList docs)))

-- | Helper, not exported
integerToInt :: Integer -> Int
integerToInt = fromIntegral

-- | Helper, not exported
bytesToInt :: Bytes.Bytes -> Int
bytesToInt = integerToInt . Bytes.bytesToInteger

-- | Ignores @tag@s.
printMemShape :: PtrShape.MemShape w tag -> Printer w (PP.Doc Void)
printMemShape = \case
  PtrShape.Uninitialized bytes -> printRle '#' (bytesToInt bytes)
  PtrShape.Initialized _tag bytes -> printRle 'X' (bytesToInt bytes)
  PtrShape.Pointer _tag off target@(PtrShape.PtrTarget bid _) -> do
      blk <- printerAlloc (printTgt target) bid
      printBlockOffset blk off
  PtrShape.Exactly bytes ->
    let ppWord8 = PP.pretty . padHex 2 in
    pure (PP.fillSep (List.map (ppWord8 . PtrShape.taggedByteValue) bytes))
