{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Macaw.RegName
  ( RegName(..)
  , mkRegName
  , regNames
  , RegNames(..)
  , getRegName
  , regNameToString
  , regNamesToList
  ) where

import Control.Lens ((^.), to)
import Data.Functor.Const (Const(..))
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- parameterized-utils
import Data.Parameterized.Classes (IxedF'(ixF'))
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.TraversableFC (toListFC)

-- macaw-base
import qualified Data.Macaw.CFG as MC

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

newtype RegName = RegName String
  deriving (Eq, Ord, Show)

-- AArch32 registers are prefixed with "_" for some reason, remove that
mkRegName :: Symbolic.SymArchConstraints arch => MC.ArchReg arch tp -> RegName
mkRegName r = RegName (strip "_" (show (MC.prettyF r)))
  where strip pfx s = Maybe.fromMaybe s (List.stripPrefix pfx s)

-- | A list of human-readable names for each register in the architecture
newtype RegNames arch = RegNames (Ctx.Assignment (Const RegName) (Symbolic.MacawCrucibleRegTypes arch))
  deriving Show

regNames ::
  forall arch mem.
  Symbolic.SymArchConstraints arch =>
  Symbolic.GenArchVals mem arch ->
  RegNames arch
regNames archVals =
  RegNames $
    Symbolic.macawAssignToCruc (Const . mkRegName) $
      Symbolic.crucGenRegAssignment (Symbolic.archFunctions archVals)

getRegName ::
  RegNames arch ->
  Ctx.Index (Symbolic.MacawCrucibleRegTypes arch) t ->
  RegName
getRegName (RegNames rNames) idx = rNames ^. ixF' idx . to getConst

regNameToString :: RegName -> String
regNameToString (RegName nm) = nm

regNamesToList :: RegNames arch -> [RegName]
regNamesToList (RegNames rNames) = toListFC getConst rNames
