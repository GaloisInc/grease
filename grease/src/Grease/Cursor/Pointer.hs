{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Due to the orphan CursorExt instances below

module Grease.Cursor.Pointer
  ( Dereference(..)
  , ppDereference
  , cursorRepr
  , addField'
  , addField
  , addDeref
  , addIndex
  , addByteIndex
  , asPtrCursor
  ) where

import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Type.Equality ((:~:)(Refl), testEquality)
import Grease.Cursor (Cursor, CursorExt)
import Grease.Cursor qualified as Cursor
import Lang.Crucible.LLVM.Extension (LLVM)
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.Types qualified as C
import Prettyprinter qualified as PP

-- | An extension to 'Cursor' for LLVM pointer types
data Dereference ext w (ts :: [C.CrucibleType]) where
  DereferenceByte ::
    -- | Offset
    !Int ->
    Cursor ext (Mem.LLVMPointerType 8 ': ts) ->
    Dereference ext w (Mem.LLVMPointerType w ': Mem.LLVMPointerType 8 ': ts)
  DereferencePtr ::
    -- | Offset
    !Int ->
    Cursor ext (Mem.LLVMPointerType w ': ts) ->
    Dereference ext w (Mem.LLVMPointerType w ': Mem.LLVMPointerType w ': ts)

type instance CursorExt (Symbolic.MacawExt arch)
  = Dereference (Symbolic.MacawExt arch) (MC.ArchAddrWidth arch)

type instance CursorExt LLVM
  = Dereference LLVM 64

ppDereference ::
  CursorExt ext ~ Dereference ext w =>
  -- | Top level, e.g. the name of a variable
  PP.Doc ann ->
  Dereference ext w ts ->
  PP.Doc ann
ppDereference top =
  \case
    DereferenceByte idx cursor ->
      Cursor.ppCursor top ppDereference cursor PP.<> "[" PP.<> PP.viaShow idx PP.<> "]"
    DereferencePtr idx cursor ->
      Cursor.ppCursor top ppDereference cursor PP.<> "[" PP.<> PP.viaShow idx PP.<> "]"

cursorRepr ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ t =>
  Cursor ext ts ->
  C.TypeRepr t
cursorRepr = Cursor.cursorRepr derefRepr
  where
    derefRepr ::
      CursorExt ext ~ Dereference ext w =>
      Cursor.Last ts ~ t =>
      Dereference ext w ts ->
      C.TypeRepr t
    derefRepr =
      \case
        DereferenceByte _ c -> cursorRepr c
        DereferencePtr _ c -> cursorRepr c

addField' ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ C.StructType fields =>
  C.TypeRepr t ->
  Ctx.Index fields t ->
  Dereference ext w ts ->
  Dereference ext w (Cursor.Snoc ts t)
addField' tRepr idx =
  \case
    DereferencePtr offset cursor -> DereferencePtr offset (addField tRepr idx cursor)
    DereferenceByte offset cursor -> DereferenceByte offset (addField tRepr idx cursor)

addField ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ C.StructType fields =>
  C.TypeRepr t ->
  Ctx.Index fields t ->
  Cursor ext ts ->
  Cursor ext (Cursor.Snoc ts t)
addField tRepr idx cursor =
  Cursor.addField addField' tRepr idx cursor

addDeref ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ Mem.LLVMPointerType w =>
  (Cursor ext '[Mem.LLVMPointerType w] -> Cursor ext (Cursor.Snoc '[Mem.LLVMPointerType w] (Mem.LLVMPointerType w))) ->
  Cursor ext ts ->
  Cursor ext (Cursor.Snoc ts (Mem.LLVMPointerType w))
addDeref f =
  \case
    h@(Cursor.Here {}) -> f h
    Cursor.Field idx' cursor -> Cursor.Field idx' (addDeref f cursor)
    Cursor.CursorExt (DereferenceByte idx' cursor) ->
      Cursor.CursorExt (DereferenceByte idx' (addDeref f cursor))
    Cursor.CursorExt (DereferencePtr idx' cursor) ->
      Cursor.CursorExt (DereferencePtr idx' (addDeref f cursor))

addIndex ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ Mem.LLVMPointerType w =>
  Int ->
  Cursor ext ts ->
  Cursor ext (Cursor.Snoc ts (Mem.LLVMPointerType w))
addIndex idx = addDeref (\c -> Cursor.CursorExt (DereferencePtr idx c))

addByteIndex ::
  CursorExt ext ~ Dereference ext w =>
  Cursor.Last ts ~ Mem.LLVMPointerType w =>
  Int ->
  Cursor ext ts ->
  Cursor ext (Cursor.Snoc ts (Mem.LLVMPointerType 8))
addByteIndex idx =
  \case
    Cursor.Here _ -> Cursor.CursorExt (DereferenceByte idx (Cursor.Here (Mem.LLVMPointerRepr (C.knownNat @8))))
    Cursor.Field idx' cursor -> Cursor.Field idx' (addByteIndex idx cursor)
    Cursor.CursorExt (DereferenceByte idx' cursor) ->
      Cursor.CursorExt (DereferenceByte idx' (addByteIndex idx cursor))
    Cursor.CursorExt (DereferencePtr idx' cursor) ->
      Cursor.CursorExt (DereferencePtr idx' (addByteIndex idx cursor))

asPtrCursor ::
  CursorExt ext ~ Dereference ext w' =>
  C.NatRepr w ->
  Cursor ext ts ->
  Maybe (Cursor.Last ts :~: Mem.LLVMPointerType w)
asPtrCursor w =
  \case
    Cursor.Here (Mem.LLVMPointerRepr w') | Just Refl <- testEquality w' w -> Just Refl
    Cursor.Here _ -> Nothing
    Cursor.Field _ c' -> asPtrCursor w c'
    Cursor.CursorExt (DereferenceByte _ c') -> asPtrCursor w c'
    Cursor.CursorExt (DereferencePtr _ c') -> asPtrCursor w c'

