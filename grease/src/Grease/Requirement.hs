{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Requirement (
  Requirement (..),
  displayReq,
) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter qualified as PP

-- | A requirement that a software program should adhere to. This data type
-- enumerates all of the requirements that @grease@ explicitly supports checking
-- for. (There are a variety of other requirements that are implicitly checked
-- by ensuring that a program is memory-safe using @grease@'s memory model.)

-- NOTE: If you update this, make sure to also update the following:
--
-- - The documentation in @doc/requirements.md@
-- - The Ghidra plugin script's offering of requirements
data Requirement
  = -- | The requirement that code is never executed from memory that has write
    -- permissions.
    InText
  | -- | The requirement that code is not self-modifying.
    NoMprotect
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance Aeson.ToJSON Requirement
instance Aeson.ToJSONKey Requirement

instance PP.Pretty Requirement where
  pretty = PP.pretty . displayReq

displayReq :: Requirement -> Text
displayReq = \case
  InText -> "in-text"
  NoMprotect -> "no-mprotect"
