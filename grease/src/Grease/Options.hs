{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE DerivingStrategies #-}

module Grease.Options
  ( LoopBound(..)
  , defaultLoopBound
  , Milliseconds(..)
  , defaultTimeout
  , MutableGlobalState(..)
  , allMutableGlobalStates
  , ExtraStackSlots(..)
  , ErrorSymbolicFunCalls(..)
  , Opts(..)
  ) where

import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity)
import Grease.Entrypoint
import Grease.Macaw.PLT
import Grease.Requirement (Requirement)
import Grease.Solver (Solver(..))

newtype LoopBound = LoopBound Word64
  deriving Show

defaultLoopBound :: Word64
defaultLoopBound = 32

newtype Milliseconds = Milliseconds Int
  deriving Show

data MutableGlobalState
  = Initialized
  | Symbolic
  | Uninitialized
  deriving (Bounded, Enum, Read, Show)

allMutableGlobalStates :: [MutableGlobalState]
allMutableGlobalStates = [minBound .. maxBound]

-- | Allocate this many pointer-sized stack slots beyond the return address,
-- which are reserved for stack-spilled arguments.
newtype ExtraStackSlots = ExtraStackSlots { getExtraStackSlots :: Int }
  -- See Note [Derive Read/Show instances with the newtype strategy]
  deriving newtype (Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | If 'True', throw an error if attempting to call a symbolic function handle
-- or pointer. If 'False', skip over such calls.
newtype ErrorSymbolicFunCalls =
  ErrorSymbolicFunCalls { getErrorSymbolicFunCalls :: Bool }
  -- See Note [Derive Read/Show instances the with newtype strategy]
  deriving newtype (Enum, Eq, Ord, Read, Show)

{-
Note [Derive Read/Show instances with newtype strategy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Make sure to derive the Read instance for option-related newtypes using the
`newtype` strategy, not the `stock` strategy. For instance, we parse
ExtraStackSlots values in the command-line parser using the `Read` instance, and
we want users to be able to write `--stack-argument-slots 1` instead of the much
more verbose `--stack-argument-slots "ExtraStackSlots{getExtraStackSlots = 1}"`.

Similar considerations apply for derived Show instances, which also have
different behavior when `stock`-derived.
-}

data Opts = Opts
  { optsBinaryPath :: FilePath
  , optsDebug :: Bool
  , optsEntrypoints :: [Entrypoint]
  , optsGlobals :: MutableGlobalState
  , optsIterations :: Maybe Int
  , optsLoopBound :: LoopBound
  , optsNoHeuristics :: Bool
  , optsOverrides :: [FilePath]
  , optsTimeout :: Milliseconds
  , optsRequirement :: [Requirement]
  , optsJSON :: Bool
  , optsVerbosity :: Severity
  , optsRust :: Bool
  , optsPltStubs :: [PltStub]
  , optsPrecond :: Maybe FilePath
  , optsProfileTo :: Maybe FilePath
  , optsStackArgumentSlots :: ExtraStackSlots
  , optsSolver :: Solver
  , optsErrorSymbolicFunCalls :: ErrorSymbolicFunCalls
  } deriving Show

-- | 30 seconds in milliseconds
defaultTimeout :: Int
defaultTimeout = 30000
