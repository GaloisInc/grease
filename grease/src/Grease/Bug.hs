{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Bug
  ( BugType(..)
  , BugInstance(..)
  ) where

import qualified Data.Eq as Eq
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)

import qualified Prettyprinter as PP

import qualified Grease.Bug.UndefinedBehavior as UB

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
