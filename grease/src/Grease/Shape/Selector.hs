{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Shape.Selector (
  ArgSelector (..),
  RetSelector (..),
  Selector (..),
  argSelectorPath,
  argSelectorIndex,
  retSelectorFunc,
  retSelectorPath,
  selectorPath,
  ppArgSelector,
  ppMacawArgSelector,
  ppRetSelector,
  ppMacawRetSelector,
  ppSelector,
) where

import Control.Lens (Lens, lens, (.~))
import Control.Lens.TH (makeLenses)
import Data.Function ((&))
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Grease.Cursor (Cursor, CursorExt, ppCursor)
import Grease.Cursor.Pointer (ppDereference)
import Prettyprinter qualified as PP
import What4.FunctionName qualified as WFN

-- | A pointer to a part of a symbolic value inside the arguments of a CFG
--
-- Type variables:
--
-- * @ext@: Crucible syntax extension
-- * @argTys@: CFG argument types
-- * @ts@: See 'Cursor'
-- * @t@: The particular element of @argTys@ this selector points to
data ArgSelector ext argTys ts t = ArgSelector
  { _argSelectorIndex :: Ctx.Index argTys t
  , _argSelectorPath :: Cursor ext (t ': ts)
  }

makeLenses ''ArgSelector

-- | A pointer to a part of a symbolic value inside of the return value of a
-- skipped function (see "Grease.Skip").
--
-- Type variables:
--
-- * @ext@: Crucible syntax extension
-- * @ts@: See 'Cursor'
-- * @t@: The particular element of @argTys@ this selector points to
data RetSelector ext ts t = RetSelector
  { _retSelectorFunc :: WFN.FunctionName
  , _retSelectorPath :: Cursor ext (t ': ts)
  }

makeLenses ''RetSelector

-- | A pointer to a part of a symbolic value invented by GREASE.
data Selector ext argTys ts t
  = SelectArg (ArgSelector ext argTys ts t)
  | SelectRet (RetSelector ext ts t)

selectorPath ::
  Lens
    (Selector ext argTys ts t)
    (Selector ext argTys ts' t)
    (Cursor ext (t ': ts))
    (Cursor ext (t ': ts'))
selectorPath =
  lens
    ( \case
        SelectArg (ArgSelector _ path) -> path
        SelectRet (RetSelector _ path) -> path
    )
    ( \s v ->
        case s of
          SelectArg sel -> SelectArg (sel & argSelectorPath .~ v)
          SelectRet sel -> SelectRet (sel & retSelectorPath .~ v)
    )

-- | For debugging
ppArgSelector ::
  (forall ts'. PP.Doc ann -> CursorExt ext ts' -> PP.Doc ann) ->
  ArgSelector ext regTy ts t ->
  PP.Doc ann
ppArgSelector ppExt (ArgSelector idx path) =
  let top = "arg" PP.<> PP.viaShow (Ctx.indexVal idx)
   in ppCursor top ppExt path

ppMacawArgSelector :: ArgSelector (Symbolic.MacawExt arch) regTy ts t -> PP.Doc ann
ppMacawArgSelector = ppArgSelector ppDereference

-- | For debugging
ppRetSelector ::
  (forall ts'. PP.Doc ann -> CursorExt ext ts' -> PP.Doc ann) ->
  RetSelector ext ts t ->
  PP.Doc ann
ppRetSelector ppExt (RetSelector func path) =
  let top = PP.pretty (WFN.functionName func <> "Return")
   in ppCursor top ppExt path

ppMacawRetSelector :: RetSelector (Symbolic.MacawExt arch) ts t -> PP.Doc ann
ppMacawRetSelector = ppRetSelector ppDereference

-- | For debugging
ppSelector ::
  (forall ts'. PP.Doc ann -> CursorExt ext ts' -> PP.Doc ann) ->
  Selector ext regTy ts t ->
  PP.Doc ann
ppSelector ppExt =
  \case
    SelectArg argSel -> ppArgSelector ppExt argSel
    SelectRet retSel -> ppRetSelector ppExt retSel
