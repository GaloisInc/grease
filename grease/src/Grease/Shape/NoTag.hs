{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
Module           : Grease.Shape
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Grease.Shape.NoTag
  ( NoTag(NoTag)
  ) where

import Data.Kind (Type)
import Data.Macaw.CFG qualified as MC
import Data.Parameterized.Classes (ShowF(showF))
import Lang.Crucible.Types qualified as C

-- | A @tag@ to annotate 'Grease.Shape.Shape'
type NoTag :: C.CrucibleType -> Type
data NoTag t = NoTag
  deriving Eq

instance Semigroup (NoTag t) where
  (<>) = \_ _ -> NoTag
  {-# INLINE (<>) #-}

-- | Empty string
instance Show (NoTag t) where
  show _ = ""
  {-# INLINE show #-}

-- | Empty string
instance ShowF NoTag where
  showF _ = ""
  {-# INLINE showF #-}

-- | Empty string
instance MC.PrettyF NoTag where
  prettyF _ = ""
  {-# INLINE prettyF #-}
