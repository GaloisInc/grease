{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Options (
  LoopBound (..),
  defaultLoopBound,
  Milliseconds (..),
  defaultTimeout,
  defaultTypeUnrollingBound,
  TypeUnrollingBound (..),
  MutableGlobalState (..),
  allMutableGlobalStates,
  ExtraStackSlots (..),
  ErrorSymbolicFunCalls (..),
  ErrorSymbolicSyscalls (..),
  SkipInvalidCallAddrs (..),
  SimOpts (..),
  Opts (..),
) where

import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity)
import Grease.Entrypoint
import Grease.Macaw.PLT
import Grease.Requirement (Requirement)
import Grease.Solver (Solver (..))

newtype TypeUnrollingBound = TypeUnrollingBound Int
  deriving Show

-- | See 'simTypeUnrollingBound'
defaultTypeUnrollingBound :: Int
defaultTypeUnrollingBound = 3

newtype LoopBound = LoopBound Word64
  deriving Show

defaultLoopBound :: Word64
defaultLoopBound = 32

newtype Milliseconds = Milliseconds Int
  deriving Show

-- | 30 seconds in milliseconds
defaultTimeout :: Int
defaultTimeout = 30000

data MutableGlobalState
  = Initialized
  | Symbolic
  | Uninitialized
  deriving (Bounded, Enum, Read, Show)

allMutableGlobalStates :: [MutableGlobalState]
allMutableGlobalStates = [minBound .. maxBound]

-- | Allocate this many pointer-sized stack slots beyond the return address,
-- which are reserved for stack-spilled arguments.
newtype ExtraStackSlots = ExtraStackSlots {getExtraStackSlots :: Int}
  -- See Note [Derive Read/Show instances with the newtype strategy]
  deriving newtype (Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | If 'True', throw an error if attempting to call a symbolic function handle
-- or pointer. If 'False', skip over such calls.
newtype ErrorSymbolicFunCalls
  = ErrorSymbolicFunCalls {getErrorSymbolicFunCalls :: Bool}
  -- See Note [Derive Read/Show instances the with newtype strategy]
  deriving newtype (Enum, Eq, Ord, Read, Show)

-- | If 'True', skip calls to invalid addresses in binaries instead of erroring.
newtype SkipInvalidCallAddrs
  = SkipInvalidCallAddrs {getSkipInvalidCallAddrs :: Bool}
  -- See Note [Derive Read/Show instances the with newtype strategy]
  deriving newtype (Enum, Eq, Ord, Read, Show)

-- | If 'True', throw an error if attempting a system call with a symbolic
-- number. If 'False', skip over such calls.
newtype ErrorSymbolicSyscalls
  = ErrorSymbolicSyscalls {getErrorSymbolicSyscalls :: Bool}
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

-- | Options that affect simulation
data SimOpts
  = SimOpts
  { simDebug :: Bool
  -- ^ Run the debugger execution feature
  , simEntryPoints :: [Entrypoint]
  -- ^ Names or address of function to simulate
  , simErrorSymbolicFunCalls :: ErrorSymbolicFunCalls
  -- ^ Default: 'False'.
  , simErrorSymbolicSyscalls :: ErrorSymbolicSyscalls
  -- ^ Default: 'False'.
  , simSkipInvalidCallAddrs :: SkipInvalidCallAddrs
  -- ^ Default: 'False'.
  , simInitialPreconditions :: Maybe FilePath
  -- ^ Path containing initial function preconditions in shapes DSL
  , simEnableDWARFPreconditions :: Bool
  -- ^ Enables parsing DWARF to extract initial shape types. This option is
  -- superseded by `simInitialPreconditions` and does not do anything in LLVM mode.
  , simTypeUnrollingBound :: TypeUnrollingBound
  -- ^ Bounds the number of times a recursive pointer will be visited when building shapes from DWARF (bounded to 3 recursive pointers by default).
  , simLoopBound :: LoopBound
  -- ^ Maximum number of iterations of each program loop/maximum number of
  -- recursive calls to the same function
  , simMaxIters :: Maybe Int
  -- ^ Maximum number of iterations of the refinement loop
  , simMutGlobs :: MutableGlobalState
  -- ^ How to initialize mutable globals
  , simNoHeuristics :: Bool
  -- ^ Disable heuristics
  , simOverrides :: [FilePath]
  -- ^ User-specified overrides in Crucible S-expression syntax
  , simOverridesYaml :: [FilePath]
  -- ^ Overrides in YAML format
  , simProgPath :: FilePath
  -- ^ Path to program to simulate
  , simPltStubs :: [PltStub]
  -- ^ User-specified PLT stubs to consider in addition to the stubs that
  -- @grease@ discovers via heuristics.
  , simProfileTo :: Maybe FilePath
  -- ^ Optional directory to write profiler-related files to.
  , simReqs :: [Requirement]
  -- ^ Requirements to check
  , simRust :: Bool
  -- ^ Use simulator settings that are more likely to work for Rust programs
  , simStackArgumentSlots :: ExtraStackSlots
  -- ^ Default: 0.
  , simSolver :: Solver
  -- ^ Default: 'Yices'.
  , simTimeout :: Milliseconds
  -- ^ Timeout (implemented using 'timeout')
  , simFsRoot :: Maybe FilePath
  -- ^ File system root
  }
  deriving Show

data Opts
  = Opts
  { optsJSON :: Bool
  , optsSimOpts :: SimOpts
  , optsVerbosity :: Severity
  }
  deriving Show
