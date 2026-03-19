-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.RegName (
  RegName (..),
  mkRegName,
  regNameToString,
) where

import Data.List qualified as List
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Maybe qualified as Maybe

newtype RegName = RegName String
  deriving (Eq, Ord, Show)

-- AArch32 registers are prefixed with "_" for some reason, remove that
mkRegName :: Symbolic.SymArchConstraints arch => MC.ArchReg arch tp -> RegName
mkRegName r = RegName (strip "_" (show (MC.prettyF r)))
 where
  strip pfx s = Maybe.fromMaybe s (List.stripPrefix pfx s)

regNameToString :: RegName -> String
regNameToString (RegName nm) = nm
