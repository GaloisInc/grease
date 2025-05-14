{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Grease.Cursor
  ( Cursor(..)
  , CursorExt
  , ppCursor
  , cursorRepr
  , addField
  , module Grease.Cursor.List
  ) where

import Data.Kind (Type)
import Data.Parameterized.Context qualified as Ctx
import Grease.Cursor.List (Last, Snoc, lastCons, lastSnoc)
import Lang.Crucible.Types qualified as C
import Prettyprinter qualified as PP

-- | A 'Cursor' points to a specific part of a value (i.e. a function argument
-- or global variable). It's used for describing function preconditions, such as
-- \"@x->y@ must not be null\", or \"@x[4]@ must be nonzero\".
--
-- Type variables:
--
-- * @ext@: Crucible syntax extension (can be used to extend this type using
--   'CursorExt')
-- * @ts@: List of types that this cursor traverses through
data Cursor ext (ts :: [C.CrucibleType]) where
  Here :: C.TypeRepr t -> Cursor ext '[t]
  Field ::
    Ctx.Index fields f ->
    Cursor ext (f ': ts) ->
    Cursor ext (C.StructType fields ': f ': ts)
  CursorExt ::
    CursorExt ext ts ->
    Cursor ext ts

-- | The @ext@-specific extension to 'Cursor'
type family CursorExt (ext :: Type) :: [C.CrucibleType] -> Type

ppCursor ::
  -- | Top level, e.g. the name of a variable
  PP.Doc ann ->
  (forall ts'. PP.Doc ann -> CursorExt ext ts' -> PP.Doc ann) ->
  Cursor ext ts ->
  PP.Doc ann
ppCursor top ppExt =
  \case
    Here _ft -> top
    CursorExt ext -> ppExt top ext
    Field idx cursor -> ppCursor top ppExt cursor PP.<> "." PP.<> PP.viaShow idx

cursorRepr ::
  Last ts ~ t =>
  (forall ts' t'.
    Last ts' ~ t' =>
    CursorExt ext ts' ->
    C.TypeRepr t') ->
  Cursor ext ts ->
  C.TypeRepr t
cursorRepr ext =
  \case
    Here t -> t
    Field _ c -> cursorRepr ext c
    CursorExt e -> ext e

-- | If you know that a 'Cursor' points to a struct and you know one of the
-- fields of the struct, you can get a 'Cursor' that points to that field.
addField ::
  Last ts ~ C.StructType fields =>
  (forall ts' t' fields'.
    Last ts' ~ C.StructType fields' =>
    C.TypeRepr t' ->
    Ctx.Index fields' t' ->
    CursorExt ext ts' ->
    CursorExt ext (Snoc ts' t')) ->
  C.TypeRepr t ->
  Ctx.Index fields t ->
  Cursor ext ts ->
  Cursor ext (Snoc ts t)
addField ext tRepr idx =
  \case
    Here _ -> Field idx (Here tRepr)
    Field idx' cursor -> Field idx' (addField ext tRepr idx cursor)
    CursorExt extCursor -> CursorExt (ext tRepr idx extCursor)
