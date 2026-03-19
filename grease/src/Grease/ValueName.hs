{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Copyright        : (c) Galois, Inc. 2026
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.ValueName (
  ValueName (..),
) where

import Lang.Crucible.Types (CrucibleType)
import Prettyprinter qualified as PP

-- | A name for a value (e.g., an argument name)
newtype ValueName (t :: CrucibleType) = ValueName {getValueName :: String}

instance PP.Pretty (ValueName t) where
  pretty (ValueName name) = PP.pretty name
