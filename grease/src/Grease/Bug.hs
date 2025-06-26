{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Bug (
  BugType (..),
  BugInstance (..),
) where

import Data.Aeson.Types qualified as Aeson
import Data.Eq qualified as Eq
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Grease.Bug.UndefinedBehavior qualified as UB
import Prettyprinter qualified as PP

-- | Errors that we don\'t know how to work around with heuristics, so the best
-- we can do is report them as possible bugs.
--
-- NOTE: keep in sync with ghidra plugin script, which matches on this type
data BugType
  = MustFail
  | OneMustFail
  | UninitStackRead
  deriving (Eq.Eq, Generic, Show)

instance Aeson.ToJSON BugType

instance PP.Pretty BugType where
  pretty =
    \case
      MustFail -> "unavoidable error (safety condition is unsatisfiable)"
      OneMustFail -> "at least one bug occurs (safety conditions are jointly unsatisfiable)"
      UninitStackRead -> "uninitialized stack read"

-- TODO: Add callstack
data BugInstance
  = BugInstance
  { bugType :: {-# UNPACK #-} !BugType
  , bugLoc :: {-# UNPACK #-} !Text.Text
  , bugUb :: {-# UNPACK #-} !(Maybe.Maybe UB.UB)
  , bugDetails :: {-# UNPACK #-} !(Maybe.Maybe Text.Text)
  }
  deriving (Generic, Show)
instance Aeson.ToJSON BugInstance

instance PP.Pretty BugInstance where
  pretty bi =
    let firstLine =
          PP.hsep
            [ PP.pretty (bugType bi)
            , "at"
            , PP.pretty (bugLoc bi)
            ]
     in case bugDetails bi of
          Maybe.Nothing -> firstLine
          Maybe.Just details -> PP.vsep [firstLine, PP.pretty details]
