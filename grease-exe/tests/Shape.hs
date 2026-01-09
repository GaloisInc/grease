{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- `error` is fine in tests
{- HLINT ignore "Use panic" -}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Shape (
  shapeTests,
) where

import Control.Monad qualified as Monad
import Control.Monad.IO.Class (liftIO)
import Data.BitVector.Sized qualified as BV
import Data.Either qualified as Either
import Data.Functor qualified as Functor
import Data.List qualified as List
import Data.Macaw.CFG (AddrWidthRepr (Addr64))
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.NatRepr qualified as NatRepr
import Data.Parameterized.Pair (Pair (Pair))
import Data.Parameterized.Some (Some (Some))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Type.Equality (testEquality)
import Data.Type.Equality qualified as Equality
import Grease.Shape as Shape
import Grease.Shape.NoTag (NoTag (NoTag))
import Grease.Shape.Parse qualified as Parse
import Grease.Shape.Pointer (PtrShape)
import Grease.Shape.Pointer qualified as PtrShape
import Grease.Shape.Print qualified as Print
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Lang.Crucible.LLVM.Bytes qualified as Bytes
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)
import Lang.Crucible.LLVM.MemModel.Pointer qualified as Mem
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import Test.Tasty qualified as TT
import Test.Tasty.HUnit qualified as TH
import Test.Tasty.Hedgehog qualified as TTH

eqShape ::
  (forall t1' t2'. ExtShape ext NoTag t1' -> ExtShape ext NoTag t2' -> Bool) ->
  Shape ext NoTag t1 ->
  Shape ext NoTag t2 ->
  Bool
eqShape eqExt s1 s2 =
  case (s1, s2) of
    (ShapeBool NoTag, ShapeBool NoTag) -> True
    (ShapeUnit NoTag, ShapeUnit NoTag) -> True
    (ShapeStruct NoTag s1', ShapeStruct NoTag s2') -> eqShapes eqExt s1' s2'
    (ShapeExt s1', ShapeExt s2') -> eqExt s1' s2'
    (_, _) -> False

eqShapes ::
  (forall t1' t2'. ExtShape ext NoTag t1' -> ExtShape ext NoTag t2' -> Bool) ->
  Ctx.Assignment (Shape ext NoTag) ts1 ->
  Ctx.Assignment (Shape ext NoTag) ts2 ->
  Bool
eqShapes eqExt c1 c2 =
  case (c1, c2) of
    (Ctx.Empty, Ctx.Empty) ->
      True
    (c1' Ctx.:> x1, c2' Ctx.:> x2) ->
      eqShape eqExt x1 x2 && eqShapes eqExt c1' c2'
    (_, _) -> False

eqMemShape :: PtrShape.MemShape w NoTag -> PtrShape.MemShape w NoTag -> Bool
eqMemShape (PtrShape.Pointer ty off tgt) (PtrShape.Pointer ty' off' tgt') =
  ty == ty'
    && off == off'
    && eqPtrTarget tgt tgt'
eqMemShape x y = x == y

-- | In parse tests we ignore the blockID. This is because Nothing is equivalent to a fresh identifier
-- That is 'PtrTarget' Nothing, 'PtrTarget' Nothing will result in a print of
-- 0: x
-- 1: y
-- The parser will then parse the blockIDs as 0 and 1 accordingly.
eqPtrTarget :: PtrShape.PtrTarget w NoTag -> PtrShape.PtrTarget w NoTag -> Bool
eqPtrTarget (PtrShape.PtrTarget _ mems) (PtrShape.PtrTarget _ mems') =
  Seq.length mems == Seq.length mems'
    && all (uncurry eqMemShape) (Seq.zip mems mems')

eqPtrShape ::
  PtrShape w ext NoTag t1 ->
  PtrShape w ext NoTag t2 ->
  Bool
eqPtrShape s1 s2 =
  case (s1, s2) of
    (PtrShape.ShapePtrBV NoTag w, PtrShape.ShapePtrBV NoTag w') ->
      Maybe.isJust (testEquality w w')
    (PtrShape.ShapePtrBVLit NoTag w bv, PtrShape.ShapePtrBVLit NoTag w' bv') ->
      case testEquality w w' of
        Maybe.Nothing -> False
        Maybe.Just Equality.Refl -> bv == bv'
    (PtrShape.ShapePtr NoTag off1 tgt1, PtrShape.ShapePtr NoTag off2 tgt2) ->
      off1 == off2 && eqPtrTarget tgt1 tgt2
    (_, _) -> False

genShapes ::
  H.Gen (Some (Shape ext NoTag)) ->
  H.Gen (Some (Ctx.Assignment (Shape ext NoTag)))
genShapes gShape = do
  xs <- HG.list (HR.linear 0 16) gShape
  pure (Ctx.fromList xs)

genStruct ::
  H.Gen (Some (Shape ext NoTag)) ->
  H.Gen (Some (Shape ext NoTag))
genStruct gShape = do
  Some xs <- genShapes gShape
  pure (Some (Shape.ShapeStruct NoTag xs))

genShape ::
  H.Gen (Some (ExtShape ext NoTag)) ->
  H.Gen (Some (Shape ext NoTag))
genShape genExt =
  HG.recursive
    HG.choice
    [ pure (Some (Shape.ShapeBool NoTag))
    , pure (Some (Shape.ShapeUnit NoTag))
    ]
    [ genStruct (genShape genExt)
    , do
        Some ext <- genExt
        pure (Some (ShapeExt ext))
    ]

maxBytes :: Integral a => a
maxBytes = 32

genPtrShape :: Mem.HasPtrWidth w => H.Gen (Some (PtrShape ext w NoTag))
genPtrShape =
  HG.choice
    [ do
        bytes <- HG.integral (HR.linear 1 maxBytes)
        Some w' <- pure (NatRepr.mkNatRepr (8 * bytes))
        case NatRepr.isZeroOrGT1 w' of
          Either.Left Equality.Refl -> Monad.fail "Impossible"
          Either.Right NatRepr.LeqProof ->
            pure (Some (PtrShape.ShapePtrBV NoTag w'))
    , do
        bytes <- HG.bytes (HR.linear 1 maxBytes)
        Pair w' bv <- pure (BV.bytestringLE bytes)
        case NatRepr.isZeroOrGT1 w' of
          Either.Left Equality.Refl -> Monad.fail "Impossible"
          Either.Right NatRepr.LeqProof ->
            pure (Some (PtrShape.ShapePtrBVLit NoTag w' bv))
    , do
        (tgt, offset) <- genPtrTarget
        pure (Some (PtrShape.ShapePtr NoTag offset tgt))
    ]

genPtrTarget ::
  Mem.HasPtrWidth wptr =>
  H.Gen (PtrShape.PtrTarget wptr NoTag, PtrShape.Offset)
genPtrTarget = do
  memShapes <- HG.list (HR.linear 0 16) genMemShape
  -- Use smart constructor to avoid "non-canonical" instances
  let tgt = PtrShape.ptrTarget Maybe.Nothing (Seq.fromList memShapes)
  let sz = PtrShape.ptrTargetSize ?ptrWidth tgt
  offsetInt <- HG.integral (HR.linear 0 (Bytes.bytesToInteger sz))
  let offset = PtrShape.Offset (Bytes.toBytes offsetInt)
  pure (tgt, offset)

genMemShape :: Mem.HasPtrWidth wptr => H.Gen (PtrShape.MemShape wptr NoTag)
genMemShape = do
  -- Don't generate zero bytes, because then printing then parsing doesn't
  -- roundtrip
  let minBytes :: Int
      minBytes = 1
  let bytes = Bytes.toBytes Functor.<$> HG.integral (HR.linear minBytes maxBytes)
  HG.recursive
    HG.choice
    [ PtrShape.Uninitialized Functor.<$> bytes
    , PtrShape.Initialized NoTag Functor.<$> bytes
    , PtrShape.Exactly
        Functor.<$> HG.list
          (HR.linear minBytes 32)
          (PtrShape.TaggedByte NoTag Functor.<$> HG.word8 (HR.linear minBound maxBound))
    ]
    [ do
        (tgt, offset) <- genPtrTarget
        pure $ PtrShape.Pointer NoTag offset tgt
    ]

printCfg :: Print.PrinterConfig 64
printCfg =
  Print.PrinterConfig
    { Print.cfgAddrWidth = Addr64
    , Print.cfgRleThreshold = 16 -- lower than `maxBytes` to exercise RLE
    }

doPrintNamed :: Text -> Shape LLVM NoTag ty -> Text
doPrintNamed nm s =
  let doc = Print.evalPrinter printCfg (Print.printNamed nm s)
   in PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions doc)

doPrint :: Shape LLVM NoTag ty -> Text
doPrint s =
  let doc = Print.evalPrinter printCfg (Print.print s)
   in PP.renderStrict (PP.layoutPretty PP.defaultLayoutOptions doc)

testPrint :: String -> Text -> Shape LLVM NoTag ty -> TT.TestTree
testPrint name printed s =
  TH.testCase name (TH.assertEqual name printed (doPrint s))

testParse :: String -> Text -> Shape LLVM NoTag ty -> TT.TestTree
testParse testName shapeSource shapeExpected = do
  let ?ptrWidth = NatRepr.knownNat @64
  let shapeName = "test"
  let shapeSource' = shapeName <> ": " <> shapeSource
  TH.testCase testName $
    case Parse.parseShapes @LLVM (Text.unpack shapeName) shapeSource' of
      Either.Left err ->
        TH.assertFailure $
          List.unlines
            [ "unexpected parse error:"
            , show (PP.pretty err)
            , "tried to parse shape:"
            , Text.unpack shapeSource'
            ]
      Either.Right (Parse.ParsedShapes shapes) -> do
        Some shapeActual <- pure $ Maybe.fromJust $ Map.lookup shapeName shapes
        let shapeExpectedTxt = doPrintNamed shapeName shapeExpected
        let shapeActualTxt = doPrintNamed shapeName shapeActual
        TH.assertEqual testName shapeExpectedTxt shapeActualTxt

ptrShape ::
  PtrShape.Offset ->
  [PtrShape.MemShape 64 NoTag] ->
  Shape LLVM NoTag (LLVMPointerType 64)
ptrShape offset =
  Shape.ShapeExt . PtrShape.ShapePtr NoTag offset . PtrShape.PtrTarget Maybe.Nothing . Seq.fromList

printThenParse :: Shape LLVM NoTag t -> IO (Some (Shape LLVM NoTag))
printThenParse s = do
  let ?ptrWidth = NatRepr.knownNat @64
  let name = "test"
  let txt = doPrintNamed name s
  case Parse.parseShapes @LLVM (Text.unpack name) txt of
    Either.Left err ->
      error $
        List.unlines
          [ "unexpected parse error:"
          , show (PP.pretty err)
          , "tried to parse shape:"
          , show (PP.pretty s)
          , "serialized:"
          , Text.unpack txt
          ]
    Either.Right (Parse.ParsedShapes shapes) ->
      pure (Maybe.fromJust (Map.lookup name shapes))

shapeTests :: TT.TestTree
shapeTests =
  TT.testGroup
    "Shape tests"
    [ TTH.testPropertyNamed "Print then parse" "print_then_parse" $
        H.property $ do
          let ?ptrWidth = NatRepr.knownNat @64
          Some s <- H.forAll (genShape @LLVM genPtrShape)
          Some s' <- liftIO (printThenParse s)
          let testEq = eqShape @LLVM eqPtrShape
          H.assert (testEq s s')
    , TTH.testPropertyNamed "Show does not loop" "show" $
        H.property $ do
          let ?ptrWidth = NatRepr.knownNat @64
          Some s <- H.forAll (genShape @LLVM genPtrShape)
          show s H.=== show s
    , testParse
        "Parse Initialized RLE"
        "000000+0000000000000000\n\n000000: XX*4"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Initialized NoTag 4])
    , testParse
        "Parse Uninitialized RLE"
        "000000+0000000000000000\n\n000000: ##*4"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Uninitialized 4])
    , testParse
        "Parse Exactly RLE"
        "000000+0000000000000000\n\n000000: de*4"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Exactly (List.map (PtrShape.TaggedByte NoTag) [0xde, 0xde, 0xde, 0xde])])
    , testParse
        "Parse Exactly byte then RLE"
        "000000+0000000000000000\n\n000000: 01 00*2 03"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Exactly (List.map (PtrShape.TaggedByte NoTag) [0x01, 0x00, 0x00, 0x03])])
    , testPrint
        "Print ShapeBool"
        "bool"
        (ShapeBool NoTag)
    , testPrint
        "Print ShapeUnit"
        "unit"
        (ShapeUnit NoTag)
    , testPrint
        "Print ShapeStruct"
        "{bool, unit}"
        ( Shape.ShapeStruct
            NoTag
            (Ctx.Empty Ctx.:> Shape.ShapeBool NoTag Ctx.:> Shape.ShapeUnit NoTag)
        )
    , testPrint
        "Print ShapePtrBV (32-bit)"
        "XX XX XX XX"
        (Shape.ShapeExt (PtrShape.ShapePtrBV NoTag (NatRepr.knownNat @32)))
    , testPrint
        "Print ShapePtrBV (64-bit)"
        "XX XX XX XX XX XX XX XX"
        (Shape.ShapeExt (PtrShape.ShapePtrBV NoTag (NatRepr.knownNat @64)))
    , testPrint
        "Print ShapePtrBVLit (8-bit)"
        "61"
        ( let w = NatRepr.knownNat @8
           in Shape.ShapeExt (PtrShape.ShapePtrBVLit NoTag w (BV.mkBV w 97))
        )
    , testPrint
        "Print ShapePtrBVLit (32-bit)"
        "0bad1dea"
        ( let w = NatRepr.knownNat @32
           in Shape.ShapeExt (PtrShape.ShapePtrBVLit NoTag w (BV.mkBV w 0xbad1dea))
        )
    , testPrint
        "Print ShapePtr (64-bit)"
        "000000+00000000000000ff\n\n000000: "
        (ptrShape (PtrShape.Offset 0xff) [])
    , testPrint
        "Print Initialized"
        "000000+0000000000000000\n\n000000: XX XX XX XX"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Initialized NoTag 4])
    , testPrint
        "Print Uninitialized"
        "000000+0000000000000000\n\n000000: ## ## ## ## ## ##"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Uninitialized 6])
    , testPrint
        "Print Exactly"
        "000000+0000000000000000\n\n000000: de ad be ef"
        (ptrShape (PtrShape.Offset 0) [PtrShape.Exactly (List.map (PtrShape.TaggedByte NoTag) [0xde, 0xad, 0xbe, 0xef])])
    , testPrint
        "Print Pointer"
        "000000+0000000000000000\n\n000000: 000001+0000000000000000\n000001: "
        (ptrShape (PtrShape.Offset 0) [PtrShape.Pointer NoTag (PtrShape.Offset 0) (PtrShape.PtrTarget Maybe.Nothing Seq.Empty)])
    , testPrint
        "Print Pointer with non-zero offset"
        "000000+0000000000000000\n\n000000: 000001+00000000000000ff\n000001: "
        (ptrShape (PtrShape.Offset 0) [PtrShape.Pointer NoTag (PtrShape.Offset 0xff) (PtrShape.PtrTarget Maybe.Nothing Seq.Empty)])
    ]
