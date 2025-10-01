-- |
-- Copyright   : (c) Galois, Inc. 2025
-- Maintainer  : GREASE Maintainers <grease@galois.com>
-- Module      : Grease.Error
-- Description : Error handling
module Grease.Error (
  GreaseException (..),
) where

import Control.Exception.Safe qualified as X
import Data.Text (Text)
import Data.Text qualified as Text

-- TODO(#4): Come up with a more thoughtful error handling scheme
newtype GreaseException = GreaseException Text
instance X.Exception GreaseException
instance Show GreaseException where
  show (GreaseException msg) = Text.unpack msg
