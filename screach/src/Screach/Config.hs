module Screach.Config (
  Config (..),
  ProgramConfig (..),
) where

import Data.Word (Word64)
import Grease.Diagnostic.Severity (Severity)
import Grease.Options qualified as GO
import Grease.Solver (Solver)
import Screach.AnalysisLoc (AnalysisLoc, EntryLoc, TargetLoc)
import Screach.Distance qualified as Dist
import Screach.RefinementOptions (AllSolutions, RefineReplay)

data ProgramConfig = ProgramConfig
  { confProgram :: FilePath
  , entryLoc :: EntryLoc
  , verbosity :: Severity
  , stackArgumentSlots :: GO.ExtraStackSlots
  }

data Config
  = Config
  { programConfig :: ProgramConfig
  , boundsOpts :: GO.BoundsOpts
  , callOpts :: GO.CallOpts
  , debugOpts :: GO.DebugOpts
  , fsOpts :: GO.FsOpts
  , initPrecondOpts :: GO.InitialPreconditionOpts
  , callgraph :: Maybe FilePath
  , targetLoc :: TargetLoc
  , globals :: GO.MutableGlobalState
  , solver :: Solver
  , refineReplay :: RefineReplay
  , explore :: Bool
  , allSolutions :: AllSolutions
  , overrides :: [FilePath]
  , overridesYaml :: [FilePath]
  , noHeuristics :: Bool
  , addrOverrides :: [(Integer, FilePath)]
  , targetOverride :: Maybe FilePath
  , defaultReturnDistance :: Dist.DefaultReturnDist
  , avoidedLocations :: [AnalysisLoc]
  , targetContainingFunction :: Maybe Word64
  , simDumpCoverage :: Maybe FilePath
  , pathSat :: Bool
  -- ^ Enable path satisfiability checking (default: True)
  , assertThenAssume :: Bool
  -- ^ Enable assert-then-assume for path-sat (default: True)
  }
