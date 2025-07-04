{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Time (
  Nanoseconds (..),
  Milliseconds (..),
  nanosToMillis,
  time,
) where

import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Prettyprinter qualified as PP

newtype Nanoseconds = Nanoseconds Word64

instance PP.Pretty Nanoseconds where
  pretty (Nanoseconds nanos) = PP.pretty nanos PP.<> "ns"

newtype Milliseconds = Milliseconds Word64

instance PP.Pretty Milliseconds where
  pretty (Milliseconds nanos) = PP.pretty nanos PP.<> "ms"

nanosToMillis :: Nanoseconds -> Milliseconds
nanosToMillis (Nanoseconds nano) = Milliseconds (nano `div` 1000000)

time :: IO a -> IO (Nanoseconds, a)
time act = do
  before <- getMonotonicTimeNSec
  a <- act
  after <- getMonotonicTimeNSec
  pure (Nanoseconds (after - before), a)
