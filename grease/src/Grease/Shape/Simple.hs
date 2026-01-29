{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
--
-- This module contains a simple datatype that can be compiled to a subset of
-- 'Shape's. The idea is to support a limited form of specifying shapes that
-- is less powerful but also easier to write than the full-fledged shape syntax
-- ("Grease.Shape.Parse"), via CLI flags like @--reg-int@.
module Grease.Shape.Simple (
  SimpleShape (..),
  parseBufInit,
  parseBufUninit,
  parseArgU32,
  parseArgU64,
  toShape,
  useSimpleShapes,
) where

import Control.Applicative (Alternative (empty), asum)
import Control.Monad qualified as Monad
import Data.BitVector.Sized (BV)
import Data.BitVector.Sized qualified as BV
import Data.Functor.Const qualified as Const
import Data.Map.Strict (Map)
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr (knownNat)
import Data.Parameterized.Some (Some (Some))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Grease.Shape (Shape)
import Grease.Shape qualified as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Parse qualified as Parse
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Lang.Crucible.LLVM.Bytes (Bytes)
import Lang.Crucible.LLVM.Bytes qualified as CLB
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char qualified as TMC
import Text.Megaparsec.Char.Lexer qualified as TMCL

-- | A simple datatype that can be compiled to a subset of 'Shape's.
data SimpleShape
  = -- | A pointer to a buffer of @n@ uninitialized bytes
    BufUninit !Bytes
  | -- | A pointer to a buffer of @n@ initialized bytes
    BufInit !Bytes
  | -- | A 32-bit unsigned integer
    ArgU32 !(BV 32)
  | -- | A 64-bit unsigned integer
    ArgU64 !(BV 64)
  deriving Show

-- | Parse a symbol from 'TM.Tokens'.
symbol :: TM.Tokens Text -> TM.Parsec Void Text Text
symbol = TMCL.symbol spaceConsumer

-- | A standard space consumer that does not support comments.
spaceConsumer :: TM.Parsec Void Text ()
spaceConsumer = TMCL.space TMC.space1 empty empty

-- | Parse an (unsigned) integer
parseInt :: TM.Parsec Void Text Integer
parseInt =
  asum
    [ symbol "0x" *> TMCL.hexadecimal
    , TMCL.decimal
    ]

parseBytes :: TM.Parsec Void Text Bytes
parseBytes = CLB.toBytes <$> parseInt

parseBufInit :: TM.Parsec Void Text SimpleShape
parseBufInit = BufInit <$> parseBytes

parseBufUninit :: TM.Parsec Void Text SimpleShape
parseBufUninit = BufUninit <$> parseBytes

parseArgU32 :: TM.Parsec Void Text SimpleShape
parseArgU32 = ArgU32 <$> parseBV32
 where
  parseBV32 :: TM.Parsec Void Text (BV 32)
  parseBV32 = do
    i <- parseInt
    let maxInt32 = fromIntegral (maxBound @Word32)
    Monad.when (i < 0 || i >= maxInt32) $
      fail ("Integer outside of 32-bit range: " ++ show i)
    pure (BV.mkBV (knownNat @32) i)

parseArgU64 :: TM.Parsec Void Text SimpleShape
parseArgU64 = ArgU64 <$> parseBV64
 where
  parseBV64 :: TM.Parsec Void Text (BV 64)
  parseBV64 = do
    i <- parseInt
    let maxInt64 = fromIntegral (maxBound @Word64)
    Monad.when (i < 0 || i >= maxInt64) $
      fail ("Integer outside of 64-bit range: " ++ show i)
    pure (BV.mkBV (knownNat @64) i)

toShape ::
  Shape.ExtShape ext ~ PtrShape ext wptr =>
  SimpleShape -> Some (Shape ext NoTag)
toShape =
  \case
    BufUninit n ->
      let tgt = PtrShape.ptrTarget Nothing (Seq.singleton (PtrShape.Uninitialized n))
       in Some (Shape.ShapeExt (PtrShape.ShapePtr NoTag (PtrShape.Offset 0) tgt))
    BufInit n ->
      let tgt = PtrShape.ptrTarget Nothing (Seq.singleton (PtrShape.Initialized NoTag n))
       in Some (Shape.ShapeExt (PtrShape.ShapePtr NoTag (PtrShape.Offset 0) tgt))
    ArgU32 bv ->
      Some (Shape.ShapeExt (PtrShape.ShapePtrBVLit NoTag (knownNat @32) bv))
    ArgU64 bv ->
      Some (Shape.ShapeExt (PtrShape.ShapePtrBVLit NoTag (knownNat @64) bv))

-- | Override 'Shape.ArgShapes' using 'SimpleShape's (generally from the CLI)
useSimpleShapes ::
  Shape.ExtShape ext ~ PtrShape ext w =>
  CLM.HasPtrWidth w =>
  -- | Argument names
  Ctx.Assignment (Const.Const String) tys ->
  -- | Initial arguments
  Shape.ArgShapes ext NoTag tys ->
  Map Text.Text SimpleShape ->
  Either Shape.TypeMismatch (Shape.ArgShapes ext NoTag tys)
useSimpleShapes argNames initArgs simpleShapes =
  let parsedShapes = Parse.ParsedShapes (fmap toShape simpleShapes)
   in Shape.replaceShapes argNames initArgs parsedShapes
