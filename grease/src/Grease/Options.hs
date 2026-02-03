{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Options (
  PathStrategy (..),
  LoopBound (..),
  defaultLoopBound,
  defaultTimeout,
  defaultSolverTimeout,
  defaultTypeUnrollingBound,
  TypeUnrollingBound (..),
  MutableGlobalState (..),
  ExtraStackSlots (..),
  ErrorSymbolicFunCalls (..),
  ErrorSymbolicSyscalls (..),
  SkipInvalidCallAddrs (..),
  SkipUnsupportedRelocs (..),
  BoundsOpts (..),
  DebugOpts (..),
  FsOpts (..),
  InitialPreconditionOpts (..),
  SimOpts (..),
  Opts (..),
  UseDebugInfoShapes (..),
) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity)
import Grease.Entrypoint (Entrypoint)
import Grease.Macaw.PLT (PltStub)
import Grease.Requirement (Requirement)
import Grease.Shape.Pointer (ExtraStackSlots (ExtraStackSlots))
import Grease.Shape.Simple (SimpleShape)
import Grease.Solver (Solver)
import Lang.Crucible.Utils.Seconds (Seconds)
import Lang.Crucible.Utils.Timeout (Timeout)
import What4.FunctionName (FunctionName)

data PathStrategy
  = -- | Depth-first search.
    --
    -- Never merge paths, and explore paths in a depth-first traversal.
    Dfs
  | -- | Static symbolic execution.
    --
    -- Always merge paths at control-flow join points.
    Sse
  deriving (Bounded, Enum, Show)

newtype TypeUnrollingBound = TypeUnrollingBound Int
  deriving Show

-- | See 'simTypeUnrollingBound'
defaultTypeUnrollingBound :: Int
defaultTypeUnrollingBound = 3

newtype LoopBound = LoopBound Word64
  deriving Show

defaultLoopBound :: Word64
defaultLoopBound = 32

-- | 30 seconds
defaultTimeout :: Int
defaultTimeout = 30

-- | 5 seconds
defaultSolverTimeout :: Int
defaultSolverTimeout = 5

data MutableGlobalState
  = Initialized
  | Symbolic
  | Uninitialized
  deriving (Bounded, Enum, Read, Show)

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

-- | If 'True', populate unsupported relocations with symbolic data.
newtype SkipUnsupportedRelocs
  = SkipUnsupportedRelocs {getSkipUnsupportedRelocs :: Bool}
  -- See Note [Derive Read/Show instances the with newtype strategy]
  deriving newtype (Enum, Eq, Ord, Read, Show)

-- | If 'True', throw an error if attempting a system call with a symbolic
-- number. If 'False', skip over such calls.
newtype ErrorSymbolicSyscalls
  = ErrorSymbolicSyscalls {getErrorSymbolicSyscalls :: Bool}
  -- See Note [Derive Read/Show instances the with newtype strategy]
  deriving newtype (Enum, Eq, Ord, Read, Show)

data UseDebugInfoShapes = NoDebugInfoShapes | ConservativeDebugInfoShapes | PreciseDebugInfoShapes
  deriving (Bounded, Enum, Show)

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

-- | Bounds, limits, and timeouts.
data BoundsOpts
  = BoundsOpts
  { simLoopBound :: LoopBound
  -- ^ Maximum number of iterations of each program loop/maximum number of
  -- recursive calls to the same function
  , simLoopBoundObligation :: Bool
  -- ^ Should hitting a loop bound incur a proof obligation?
  , simMaxIters :: Maybe Int
  -- ^ Maximum number of iterations of the refinement loop
  , simTimeout :: Seconds
  -- ^ Timeout (implemented using 'timeout')
  , simSolverTimeout :: Timeout
  -- ^ Solver timeout (in seconds)
  }
  deriving Show

-- | Options related to the debugger
data DebugOpts
  = DebugOpts
  { debug :: Bool
  -- ^ Start the debugger at the beginning of simulation
  , debugCmds :: [Text]
  -- ^ Commands to prepend to the debugger inputs (like GDB's @-iex@)
  }
  deriving Show

data FsOpts
  = FsOpts
  { fsRoot :: Maybe FilePath
  -- ^ File system root
  , fsStdin :: Word64
  -- ^ Symbolic bytes of stdin
  , fsSymFiles :: Map Text Integer
  -- ^ Paths and sizes of symbolic files
  }
  deriving Show

-- | Options that affect the initial precondition used for simulation
data InitialPreconditionOpts
  = InitialPreconditionOpts
  { initPrecondPath :: Maybe FilePath
  -- ^ Path containing initial function preconditions in shapes DSL
  , initPrecondUseDebugInfo :: UseDebugInfoShapes
  -- ^ Enables parsing debug info to extract initial shape types. This option is
  -- superseded by `initPrecondPath`.
  , initPrecondTypeUnrollingBound :: TypeUnrollingBound
  -- ^ Bounds the number of times a recursive pointer will be visited when building shapes from DWARF (bounded to 3 recursive pointers by default).
  , initPrecondSimpleShapes :: Map Text SimpleShape
  -- ^ Map from argument names to 'SimpleShape's for those arguments
  }
  deriving Show

-- | Options that affect simulation
data SimOpts
  = SimOpts
  { simDebugOpts :: DebugOpts
  -- ^ Run the debugger execution feature
  , simEntryPoints :: [Entrypoint]
  -- ^ Names or address of function to simulate
  , simErrorSymbolicFunCalls :: ErrorSymbolicFunCalls
  -- ^ Default: 'False'.
  , simErrorSymbolicSyscalls :: ErrorSymbolicSyscalls
  -- ^ Default: 'False'.
  , simSkipInvalidCallAddrs :: SkipInvalidCallAddrs
  -- ^ Default: 'False'.
  , simSkipUnsupportedRelocs :: SkipUnsupportedRelocs
  -- ^ Default: 'False'.
  , simMutGlobs :: MutableGlobalState
  -- ^ How to initialize mutable globals
  , simNoHeuristics :: Bool
  -- ^ Disable heuristics
  , simOverrides :: [FilePath]
  -- ^ User-specified overrides in Crucible S-expression syntax
  , simAddressOverrides :: [(Integer, FilePath)]
  -- ^ Address overrides in Crucible S-expression syntax
  , simOverridesYaml :: [FilePath]
  -- ^ Overrides in YAML format
  , simProgPath :: FilePath
  -- ^ Path to program to simulate
  , simRawBinaryMode :: Bool
  -- ^ Parse binary in raw binary mode (non-elf position dependent executable)
  , simRawBinaryOffset :: Word64
  -- ^ Load a raw binary at a given offset (will default to 0x0)
  , simPathStrategy :: PathStrategy
  -- ^ Path exploration strategy
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
  , simBoundsOpts :: BoundsOpts
  , simFsOpts :: FsOpts
  , simInitPrecondOpts :: InitialPreconditionOpts
  , simDumpCoverage :: Maybe FilePath
  , simSkipFuns :: Set FunctionName
  }
  deriving Show

data Opts
  = Opts
  { optsJSON :: Bool
  , optsSimOpts :: SimOpts
  , optsVerbosity :: Severity
  }
  deriving Show
