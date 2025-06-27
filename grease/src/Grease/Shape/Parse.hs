{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
-- Module      : Grease.Shape.Parse
--
-- The 'PtrShape.ShapePtr' constructor takes a 'PtrShape.PtrTarget' as an argument.
-- However, in the serialized format, the pointer targets are printed at the very
-- end, on their own lines. Therefore, we parse into a 'ShapesAst', then turn this
-- into a list of actual 'Shape's.
--
-- See @doc/shape-dsl.md@ for a description of the syntax.
module Grease.Shape.Parse (
  parseShapes,
  ParsedShapes (..),
  ParseError (..),
) where

import Control.Applicative qualified as Applicative
import Control.Monad qualified as Monad
import Data.Bifunctor qualified as Bifunctor
import Data.BitVector.Sized (BV)
import Data.BitVector.Sized qualified as BV
import Data.Coerce (coerce)
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
import Data.Functor qualified as Functor
import Data.Functor.Const qualified as Const
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (NatRepr)
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Some (Some (Some))
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Proxy (Proxy (Proxy))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable qualified as Traversable
import Data.Tuple qualified as Tuple
import Data.Type.Equality (testEquality, type (:~:) (Refl))
import Data.Void (Void)
import Data.Word (Word8)
import GHC.TypeLits (Nat)
import Grease.Panic (panic)
import Grease.Shape (ExtShape, ParsedShapes (..), Shape)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Pointer (BlockId (BlockId, getBlockId), Offset, PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.LLVM.Bytes (Bytes)
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.MemModel.Pointer (HasPtrWidth)
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Lang.Crucible.Types qualified as C
import Lang.Crucible.Types qualified as CT
import Numeric
import Prettyprinter qualified as PP
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPCL

newtype Allocs = Allocs {getAllocs :: IntMap ParsePtrTarget}
  deriving Show

newtype ParsePtrTarget = ParsePtrTarget {getParsePtrTarget :: Seq ParseMemShape}
  deriving Show

-- | Like 'PtrShape.MemShape', but non-recursive
data ParseMemShape
  = Uninitialized !Bytes
  | -- | Some number of symbolically-initialized bytes
    Initialized !Bytes
  | -- | Several (generally 4 or 8) initialized bytes that form a pointer, plus
    -- an offset into that pointer
    Pointer BlockId !Offset
  | -- | Some concrete bytes
    Exactly [Word8]
  deriving Show

lookupAlloc :: BlockId -> Allocs -> Maybe ParsePtrTarget
lookupAlloc blk as = IntMap.lookup (getBlockId blk) (getAllocs as)

removeAlloc :: BlockId -> Allocs -> Allocs
removeAlloc blk as = Allocs (IntMap.delete (getBlockId blk) (getAllocs as))

ptrTarget :: Allocs -> ParsePtrTarget -> Maybe BlockId -> Either BlockId (PtrShape.PtrTarget wptr NoTag)
ptrTarget as tgt bid =
  PtrShape.PtrTarget bid
    Functor.<$> Traversable.traverse (memShape as) (getParsePtrTarget tgt)

memShape :: Allocs -> ParseMemShape -> Either BlockId (PtrShape.MemShape wptr NoTag)
memShape allocs =
  \case
    Uninitialized bs -> Either.Right (PtrShape.Uninitialized bs)
    Initialized bs -> Either.Right (PtrShape.Initialized NoTag bs)
    Exactly wds ->
      Either.Right (PtrShape.Exactly (List.map (PtrShape.TaggedByte NoTag) wds))
    Pointer blk off ->
      PtrShape.Pointer NoTag off
        Functor.<$> case lookupAlloc blk allocs of
          Maybe.Just tgt -> ptrTarget (removeAlloc blk allocs) tgt (Just blk)
          Maybe.Nothing -> Either.Left blk

-- | @ext@ type parameter to 'Shape' for use in parsing
type Parse :: Type -> Nat -> Type
data Parse ext w

-- | AST of serialized format for 'Shape's
data ShapesAst ext w
  = ShapesAst
  { astAllocs :: Allocs
  , astShapes :: Map Text (Some (Shape (Parse ext w) NoTag))
  }

type instance Shape.ExtShape (Parse ext w) = ParsePtrShape w

-- | Like 'PtrShape.PtrShape', except that 'PtrShape.PtrTarget' is replaced
-- by 'BlockId'.
type ParsePtrShape :: Nat -> (C.CrucibleType -> Type) -> C.CrucibleType -> Type
data ParsePtrShape w tag t where
  ParseShapePtrBV ::
    1 C.<= w' =>
    NatRepr w' ->
    ParsePtrShape w tag (Mem.LLVMPointerType w')
  ParseShapePtrBVLit ::
    1 C.<= w' =>
    NatRepr w' ->
    BV w' ->
    ParsePtrShape w tag (Mem.LLVMPointerType w')
  ParseShapePtr ::
    PtrShape.Offset ->
    BlockId ->
    ParsePtrShape w tag (Mem.LLVMPointerType w)

astToMap ::
  ExtShape ext ~ PtrShape ext w =>
  ShapesAst ext w ->
  Either BlockId (ParsedShapes ext)
astToMap ast =
  coerce $
    Traversable.traverse
      (\(Some s) -> Some Functor.<$> convertShape (astAllocs ast) s)
      (astShapes ast)

convertShape ::
  ExtShape ext ~ PtrShape ext w =>
  Allocs ->
  Shape (Parse ext w) NoTag t ->
  Either BlockId (Shape ext NoTag t)
convertShape as =
  \case
    Shape.ShapeUnit NoTag -> Either.Right (Shape.ShapeUnit NoTag)
    Shape.ShapeBool NoTag -> Either.Right (Shape.ShapeBool NoTag)
    Shape.ShapeStruct tag fields ->
      Shape.ShapeStruct tag
        Functor.<$> TFC.traverseFC (convertShape as) fields
    Shape.ShapeExt (ParseShapePtrBV w) ->
      Either.Right (Shape.ShapeExt (PtrShape.ShapePtrBV NoTag w))
    Shape.ShapeExt (ParseShapePtrBVLit w bv) ->
      Either.Right (Shape.ShapeExt (PtrShape.ShapePtrBVLit NoTag w bv))
    Shape.ShapeExt (ParseShapePtr off (BlockId blk)) ->
      case IntMap.lookup blk (getAllocs as) of
        Maybe.Nothing -> Either.Left (BlockId blk)
        Maybe.Just tgt ->
          Shape.ShapeExt . PtrShape.ShapePtr NoTag off
            Functor.<$> ptrTarget as tgt (Just $ BlockId blk)

-----------------------------------------------------------

-- * Parsing

type Parser = Parsec Void Text

data ParseError
  = ParseError (MP.ParseErrorBundle Text Void)
  | MissingBlock BlockId
  deriving Show

instance PP.Pretty ParseError where
  pretty =
    \case
      ParseError err -> PP.pretty (MP.errorBundlePretty err)
      MissingBlock (BlockId blk) ->
        PP.pretty ("Missing definition for allocation " List.++ showHex blk "")

parseShapes ::
  forall ext w.
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  FilePath ->
  Text ->
  Either ParseError (ParsedShapes ext)
parseShapes path txt =
  let parsed = MP.runParser (parser (Proxy @ext) Applicative.<* MP.eof) path txt
   in Bifunctor.first MissingBlock . astToMap
        Monad.=<< Bifunctor.first ParseError parsed

parser ::
  forall ext w proxy.
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  proxy ext ->
  Parser (ShapesAst ext w)
parser _proxy = do
  let namedShape = do
        let name =
              Text.cons
                Functor.<$> (MP.single '%' MP.<|> MPC.letterChar)
                Applicative.<*> (Text.pack Functor.<$> MP.some MPC.alphaNumChar)
        chars <- name Applicative.<* MP.chunk ": "
        shape <- parseShape @ext
        _ <- MP.optional MPC.newline
        Applicative.pure (chars, shape)
  shapes <- MP.some namedShape
  as <- MP.many (MP.try (MP.some MPC.newline Monad.>> parseAlloc))
  _ <- MP.optional MPC.newline
  let allocs = Allocs (IntMap.fromList as)
  Applicative.pure (ShapesAst allocs (Map.fromList shapes))

parseAlloc :: Parser (Int, ParsePtrTarget)
parseAlloc = do
  blk <- MP.label "an allocation number (hexadecimal number)" MPCL.hexadecimal
  _ <- MP.chunk ": "
  tgt <- parsePtrTarget
  Applicative.pure (blk, tgt)

memShapeSep :: Parser ()
memShapeSep =
  Monad.void (MP.chunk " ")
    -- These newline/space combos come from `PP.fillSep`
    MP.<|> Monad.void (MP.try (MPC.newline Applicative.*> MP.some (MP.chunk " ")))

parsePtrTarget :: Parser ParsePtrTarget
parsePtrTarget =
  ParsePtrTarget . Seq.fromList Functor.<$> MP.sepBy parseMemShape memShapeSep

parseMemShape :: Parser ParseMemShape
parseMemShape =
  Applicative.asum @[]
    [ Uninitialized Functor.<$> parseUninit
    , Initialized Functor.<$> parseInit
    , do
        -- The `try` is needed to disambiguate the block number from the bytes
        -- of `Exactly`
        blk <-
          MP.try
            ( do
                blk <- BlockId Functor.<$> MPCL.hexadecimal
                _ <- MP.chunk "+"
                Applicative.pure blk
            )
        off <- parseOffset
        Applicative.pure $ Pointer blk off
    , Exactly . Foldable.toList Functor.<$> parseExactly
    ]

trySepBy1 :: Parser a -> Parser sep -> Parser [a]
trySepBy1 p sep = (:) Functor.<$> p Applicative.<*> MP.many (MP.try (sep Applicative.*> p))

parseUninit :: Parser Bytes.Bytes
parseUninit = parseUninitRle MP.<|> parseUninitExplicit

parseUninitExplicit :: Parser Bytes.Bytes
parseUninitExplicit =
  Bytes.toBytes . List.length Functor.<$> trySepBy1 (MP.chunk "##") memShapeSep

parseRle :: Parser Char -> Parser Bytes
parseRle parseChar = do
  -- The `try` is necessary to disambiguate from explicit bytes
  MP.try $ do
    _ <- parseChar
    _ <- parseChar
    _ <- MP.single '*'
    pure ()
  rl <- MPCL.hexadecimal
  pure (Bytes.toBytes @Int rl)

parseUninitRle :: Parser Bytes.Bytes
parseUninitRle = parseRle (MP.single '#')

parseInit :: Parser Bytes.Bytes
parseInit = parseInitRle MP.<|> parseInitExplicit

parseInitExplicit :: Parser Bytes.Bytes
parseInitExplicit =
  Bytes.toBytes . List.length Functor.<$> trySepBy1 (MP.chunk "XX") memShapeSep

parseInitRle :: Parser Bytes.Bytes
parseInitRle = parseRle (MP.single 'X')

-- | Helper, not exported. Requires actual hex 'Char's.
hexCharsToWord8 :: Char -> Char -> Word8
hexCharsToWord8 c1 c2 = Tuple.fst (List.head (Numeric.readHex (c1 : c2 : [])))

parseExactly :: Parser (Seq Word8)
parseExactly = mconcat <$> trySepBy1 parseExactlyByte memShapeSep

-- | Parse either a byte (e.g., @e0@) or a RLE'd byte (e.g., @e0*0a@).
parseExactlyByte :: Parser (Seq Word8)
parseExactlyByte = do
  -- We don't use parseRle because it led to a conflict between the RLE and
  -- non-RLE parsers, see #238.
  c0 <- MPC.hexDigitChar
  c1 <- MPC.hexDigitChar
  let rle = MPC.char '*' Applicative.*> MPCL.hexadecimal
  let nonRle = MP.notFollowedBy MPC.hexDigitChar Applicative.*> Applicative.pure 1
  num <- rle MP.<|> nonRle
  pure (Seq.replicate num (hexCharsToWord8 c0 c1))

parseShape ::
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  Parser (Some (Shape (Parse ext w) NoTag))
parseShape =
  Applicative.asum @[]
    [ Some Functor.<$> parseBool
    , Some Functor.<$> parseUnit
    , parseStruct
    , parsePtrBv
    , Some . Shape.ShapeExt Functor.<$> parsePtr
    , parsePtrBvLit
    ]

parseBool :: Parser (Shape ext NoTag CT.BoolType)
parseBool = MP.chunk "bool" Functor.$> Shape.ShapeBool NoTag

parseUnit :: Parser (Shape ext NoTag CT.UnitType)
parseUnit = MP.chunk "unit" Functor.$> Shape.ShapeUnit NoTag

parseStruct ::
  ExtShape ext ~ PtrShape ext w =>
  HasPtrWidth w =>
  Parser (Some (Shape (Parse ext w) NoTag))
parseStruct = do
  _ <- MP.chunk "{"
  _ <- MP.optional MPC.space
  fields <- MP.sepBy parseShape (MP.chunk "," Monad.>> MP.optional MPC.space)
  _ <- MP.optional MPC.space
  _ <- MP.chunk "}"
  Some assign <- Applicative.pure (Ctx.fromList fields)
  Applicative.pure (Some (Shape.ShapeStruct NoTag assign))

parsePtrBv ::
  ExtShape ext ~ PtrShape ext w =>
  Parser (Some (Shape (Parse ext w) NoTag))
parsePtrBv = do
  n <- List.length Functor.<$> MP.sepBy1 (MP.chunk "XX") (MP.chunk " ")
  Some bits <- Applicative.pure (NatRepr.mkNatRepr (fromIntegral n * 8))
  case NatRepr.isZeroOrGT1 bits of
    Either.Left Refl -> panic "Impossible: `sepBy1` returned empty list" []
    Either.Right NatRepr.LeqProof ->
      Applicative.pure (Some (Shape.ShapeExt (ParseShapePtrBV bits)))

parsePtrBvLit ::
  ExtShape ext ~ PtrShape ext w =>
  Parser (Some (Shape (Parse ext w) NoTag))
parsePtrBvLit = do
  _ <- MP.optional (MP.chunk "0x")
  -- Use `lookAhead` to ensure the hex number has an even number of digits, so
  -- that it represents some number of bytes.
  nBytes <- List.length Functor.<$> MP.lookAhead (MP.some (MPC.hexDigitChar Applicative.*> MPC.hexDigitChar))
  Some bits <- Applicative.pure (NatRepr.mkNatRepr (fromIntegral (8 * nBytes)))
  case NatRepr.isZeroOrGT1 bits of
    Either.Left Refl -> panic "parsePtrBvLit: impossible: zero bytes" []
    Either.Right NatRepr.LeqProof -> do
      bv <- BV.mkBV bits Functor.<$> MPCL.hexadecimal
      Applicative.pure (Some (Shape.ShapeExt (ParseShapePtrBVLit bits bv)))

parsePtr :: HasPtrWidth w => Parser (ParsePtrShape w NoTag (Mem.LLVMPointerType w))
parsePtr = do
  -- The `try` is needed to disambiguate the block number from the bytes of
  -- `ShapePtrBVLit`
  blk <-
    MP.try
      ( do
          blk <- BlockId Functor.<$> MPCL.hexadecimal
          _ <- MP.chunk "+"
          Applicative.pure blk
      )
  off <- parseOffset
  Applicative.pure (ParseShapePtr off blk)

parseOffset :: Parser PtrShape.Offset
parseOffset = PtrShape.Offset . (Bytes.toBytes @Int) Functor.<$> MPCL.hexadecimal

-----------------------------------------------------------
