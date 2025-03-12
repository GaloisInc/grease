{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Grease.Bug.UndefinedBehavior
  ( UB
  , ubType
  , UBType(..)
  , PoisonType(..)
  , makeUb
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

import Lang.Crucible.LLVM.Errors.UndefinedBehavior (UndefinedBehavior)
import qualified Lang.Crucible.LLVM.Errors.UndefinedBehavior as UB
import Lang.Crucible.LLVM.Errors.Poison (Poison)
import qualified Lang.Crucible.LLVM.Errors.Poison as Poison
import qualified Lang.Crucible.Simulator.RegValue as C
import qualified What4.Interface as W4

-- | A simplified, serializable version of 'UndefinedBehavior'
data UB
  = UB
    { ubType :: UBType
    , ubExplanation :: Text
    , ubDetails :: Text
    , ubCitation :: Text
    }
  deriving (Eq, Show, Generic)
instance Aeson.ToJSON UB

data UBType
  = FreeBadOffset
  | FreeUnallocated
  | DoubleFree
  | MemsetInvalidRegion
  | ReadBadAlignment
  | WriteBadAlignment
  | PtrAddOffsetOutOfBounds
  | CompareInvalidPointer
  | CompareDifferentAllocs
  | PtrSubDifferentAllocs
  | PointerFloatCast
  | PointerIntCast
  | PointerUnsupportedOp
  | ComparePointerToBV
  | UDivByZero
  | SDivByZero
  | URemByZero
  | SRemByZero
  | SDivOverflow
  | SRemOverflow
  | AbsIntMin
  | PoisonValueCreated PoisonType
  deriving (Eq, Show, Generic)
instance Aeson.ToJSON UBType

data PoisonType
  = AddNoUnsignedWrap
  | AddNoSignedWrap
  | SubNoUnsignedWrap
  | SubNoSignedWrap
  | MulNoUnsignedWrap
  | MulNoSignedWrap
  | UDivExact
  | SDivExact
  | ShlOp2Big
  | ShlNoUnsignedWrap
  | ShlNoSignedWrap
  | LshrExact
  | LshrOp2Big
  | AshrExact
  | AshrOp2Big
  | ExtractElementIndex
  | InsertElementIndex
  | GEPOutOfBounds
  | LLVMAbsIntMin
  deriving (Eq, Show, Generic)
instance Aeson.ToJSON PoisonType

getUbType ::
  UndefinedBehavior e ->
  UBType
getUbType =
  \case
    UB.FreeBadOffset {} -> FreeBadOffset
    UB.FreeUnallocated {} -> FreeUnallocated
    UB.DoubleFree {} -> DoubleFree
    UB.MemsetInvalidRegion {} -> MemsetInvalidRegion
    UB.ReadBadAlignment {} -> ReadBadAlignment
    UB.WriteBadAlignment {} -> WriteBadAlignment
    UB.PtrAddOffsetOutOfBounds {} -> PtrAddOffsetOutOfBounds
    UB.CompareInvalidPointer {} -> CompareInvalidPointer
    UB.CompareDifferentAllocs {} -> CompareDifferentAllocs
    UB.PtrSubDifferentAllocs {} -> PtrSubDifferentAllocs
    UB.PointerFloatCast {} -> PointerFloatCast
    UB.PointerIntCast {} -> PointerIntCast
    UB.PointerUnsupportedOp {} -> PointerUnsupportedOp
    UB.ComparePointerToBV {} -> ComparePointerToBV
    UB.UDivByZero {} -> UDivByZero
    UB.SDivByZero {} -> SDivByZero
    UB.URemByZero {} -> URemByZero
    UB.SRemByZero {} -> SRemByZero
    UB.SDivOverflow {} -> SDivOverflow
    UB.SRemOverflow {} -> SRemOverflow
    UB.AbsIntMin {} -> AbsIntMin
    UB.PoisonValueCreated poison -> PoisonValueCreated (getPoisonType poison)

getPoisonType :: Poison e -> PoisonType
getPoisonType =
  \case
    Poison.AddNoUnsignedWrap {} -> AddNoUnsignedWrap
    Poison.AddNoSignedWrap {} -> AddNoSignedWrap
    Poison.SubNoUnsignedWrap {} -> SubNoUnsignedWrap
    Poison.SubNoSignedWrap {} -> SubNoSignedWrap
    Poison.MulNoUnsignedWrap {} -> MulNoUnsignedWrap
    Poison.MulNoSignedWrap {} -> MulNoSignedWrap
    Poison.UDivExact {} -> UDivExact
    Poison.SDivExact {} -> SDivExact
    Poison.ShlOp2Big {} -> ShlOp2Big
    Poison.ShlNoUnsignedWrap {} -> ShlNoUnsignedWrap
    Poison.ShlNoSignedWrap {} -> ShlNoSignedWrap
    Poison.LshrExact {} -> LshrExact
    Poison.LshrOp2Big {} -> LshrOp2Big
    Poison.AshrExact {} -> AshrExact
    Poison.AshrOp2Big {} -> AshrOp2Big
    Poison.ExtractElementIndex {} -> ExtractElementIndex
    Poison.InsertElementIndex {} -> InsertElementIndex
    Poison.GEPOutOfBounds {} -> GEPOutOfBounds
    Poison.LLVMAbsIntMin {} -> LLVMAbsIntMin

makeUb ::
  W4.IsExpr (W4.SymExpr sym) =>
  UndefinedBehavior (C.RegValue' sym) ->
  UB
makeUb ub =
  UB
  { ubType = getUbType ub
  , ubExplanation =  docToText (UB.explain ub)
  , ubDetails = docToText (PP.vcat (UB.details ub))
  , ubCitation = docToText (UB.cite ub)
  }
  where docToText = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

