{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Cli (
  -- These individual option parsers are used in a downstream project.

  -- * Individual option parsers
  addrOverridesParser,
  boundsOptsParser,
  debugOptsParser,
  fsOptsParser,
  fsRootParser,
  globalsParser,
  initPrecondParser,
  initPrecondOptsParser,
  overridesParser,
  overridesYamlParser,
  simpleShapesParser,
  solverParser,
  stackArgSlotsParser,
  symFilesParser,
  symStdinParser,
  simDumpCoverageParser,
  simSkipFunsParser,

  -- * High-level entrypoints
  optsInfo,
  optsFromList,
  optsFromArgs,
) where

import Control.Applicative (optional, (<**>))
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String qualified as String
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import Grease.Cli.Enum qualified as GCE
import Grease.Diagnostic.Severity qualified as Sev
import Grease.Entrypoint
import Grease.Macaw.Overrides.Address (addressOverrideParser)
import Grease.Macaw.PLT
import Grease.Options (SimOpts (simDumpCoverage))
import Grease.Options qualified as GO
import Grease.Shape.Simple (SimpleShape)
import Grease.Shape.Simple qualified as Simple
import Grease.Solver (Solver (..))
import Grease.Version (verStr)
import Lang.Crucible.Utils.Seconds (secondsFromInt)
import Lang.Crucible.Utils.Timeout (Timeout (Timeout))
import Options.Applicative qualified as Opt
import Text.Megaparsec qualified as TM
import Text.Megaparsec.Char.Lexer qualified as TMCL
import What4.FunctionName (FunctionName)

------------------------------------------------------------
-- Helpers (not exported)

megaparsecReader :: TM.Parsec Void Text a -> Opt.ReadM a
megaparsecReader p = Opt.eitherReader $ \rawStr ->
  case TM.parse p "" (String.fromString rawStr) of
    Left err -> Left $ TM.errorBundlePretty err
    Right val -> Right val

------------------------------------------------------------
-- Individual option parsers

entrypointParser :: Opt.Parser Entrypoint
entrypointParser =
  Opt.parserOptionGroup "Entrypoint options" $
    addressParser
      Opt.<|> symbolParser
      Opt.<|> coreDumpParser
      Opt.<|> addressStartupOvParser
      Opt.<|> symbolStartupOvParser
 where
  addressParser =
    entrypointNoStartupOv . EntrypointAddress
      <$> Opt.strOption
        ( Opt.long "address"
            <> Opt.help "address of entrypoint"
            <> Opt.metavar "ADDR"
        )

  symbolParser =
    entrypointNoStartupOv . EntrypointSymbolName
      <$> Opt.strOption
        ( Opt.long "symbol"
            <> Opt.help "name of entrypoint symbol"
            <> Opt.metavar "SYMBOL"
        )

  coreDumpParser =
    entrypointNoStartupOv . EntrypointCoreDump
      <$> Opt.strOption
        ( Opt.long "core-dump"
            <> Opt.help "path to a core dump file obtained from the binary to simulate"
            <> Opt.metavar "PATH"
        )

  addressStartupOvParser =
    Opt.option
      (megaparsecReader entrypointAddressStartupOvParser)
      ( Opt.long "address-startup-override"
          <> Opt.metavar "ADDR:FILE"
          <> Opt.help "address of entrypoint, and the path to its startup override (in Crucible S-expression syntax)"
      )

  symbolStartupOvParser =
    Opt.option
      (megaparsecReader entrypointSymbolStartupOvParser)
      ( Opt.long "symbol-startup-override"
          <> Opt.metavar "SYMBOL:FILE"
          <> Opt.help "name of entrypoint symbol, and the path to its startup override (in Crucible S-expression syntax)"
      )

boundsOptsParser :: Opt.Parser GO.BoundsOpts
boundsOptsParser = Opt.parserOptionGroup "Bounds, limits, and timeouts" $ do
  simLoopBound <-
    GO.LoopBound
      <$> Opt.option
        Opt.auto
        ( Opt.long "loop-bound"
            <> Opt.help "maximum number of executions of each loop in the program"
            <> Opt.metavar "N"
            <> Opt.showDefault
            <> Opt.value GO.defaultLoopBound
        )
  simLoopBoundObligation <-
    Opt.flag
      True
      False -- Note: flipped to make True default
      ( Opt.long "no-loop-bound-obligation"
          <> Opt.help "Disable proof obligations for loop bounds"
      )
  simMaxIters <-
    optional $
      Opt.option
        Opt.auto
        ( Opt.long "iters"
            <> Opt.help "limit maximum number of iterations of the refinement loop"
            <> Opt.metavar "N"
        )
  simTimeout <-
    secondsFromInt
      <$> Opt.option
        Opt.auto
        ( Opt.long "timeout"
            <> Opt.help "symbolic execution timeout (in seconds)"
            <> Opt.metavar "SECS"
            <> Opt.showDefault
            <> Opt.value GO.defaultTimeout
        )
  simSolverTimeout <-
    Timeout . secondsFromInt
      <$> Opt.option
        Opt.auto
        ( Opt.long "solver-timeout"
            <> Opt.help "solver timeout (in seconds)"
            <> Opt.metavar "SECS"
            <> Opt.showDefault
            <> Opt.value GO.defaultSolverTimeout
        )
  pure GO.BoundsOpts{..}

initPrecondOptsParser :: Opt.Parser GO.InitialPreconditionOpts
initPrecondOptsParser = Opt.parserOptionGroup "Initial precondition options" $ do
  initPrecondUseDebugInfo <-
    GCE.enumParserDefault
      GO.NoDebugInfoShapes
      [ Opt.long "debug-info-types"
      , Opt.help
          "Use types in debug info to infer initial preconditions. \
          \  Can be (partially) overridden --initial-precondition, --arg-*, and/or --*-startup-override."
      ]
  initPrecondTypeUnrollingBound <-
    GO.TypeUnrollingBound
      <$> Opt.option
        Opt.auto
        ( Opt.long "type-unrolling-bound"
            <> Opt.help "Number of recursive pointers to visit during DWARF shape building"
            <> Opt.showDefault
            <> Opt.value GO.defaultTypeUnrollingBound
        )
  initPrecondPath <- initPrecondParser
  initPrecondSimpleShapes <- simpleShapesParser
  pure GO.InitialPreconditionOpts{..}

simpleShapesParser :: Opt.Parser (Map Text SimpleShape)
simpleShapesParser = fmap Map.fromList $ Opt.many $ do
  let argName = TM.takeWhile1P Nothing (\c -> Char.isAscii c && c /= ':')
  let withArgName p = megaparsecReader ((,) <$> argName <*> (TM.chunk ":" *> p))
  Opt.asum
    [ Opt.option
        (withArgName Simple.parseBufUninit)
        ( Opt.long "arg-buf-uninit"
            <> Opt.metavar "ARG:N"
            <> Opt.help "initialize argument ARG to a pointer to an uninitialized buffer of N bytes"
        )
    , Opt.option
        (withArgName Simple.parseBufInit)
        ( Opt.long "arg-buf-init"
            <> Opt.metavar "ARG:N"
            <> Opt.help "initialize argument ARG to a pointer to an initialized buffer of N symbolic bytes"
        )
    , Opt.option
        (withArgName Simple.parseArgU32)
        ( Opt.long "arg-u32"
            <> Opt.metavar "ARG:N"
            <> Opt.help "initialize argument ARG to an unsigned 32-bit integer"
        )
    , Opt.option
        (withArgName Simple.parseArgU64)
        ( Opt.long "arg-u64"
            <> Opt.metavar "ARG:N"
            <> Opt.help "initialize argument ARG to an unsigned 64-bit integer"
        )
    ]

addrOverridesParser :: Opt.Parser [(Integer, FilePath)]
addrOverridesParser =
  Opt.many $
    Opt.option
      (megaparsecReader addressOverrideParser)
      ( Opt.long "addr-override"
          <> Opt.metavar "ADDR:FILE"
          <> Opt.help "address overrides, in Crucible S-expression syntax"
      )

debugOptsParser :: Opt.Parser GO.DebugOpts
debugOptsParser = Opt.parserOptionGroup "Debugger options" $ do
  debug <- Opt.switch (Opt.long "debug" <> Opt.help "run the debugger")
  debugCmds <-
    Opt.many
      ( Opt.strOption
          ( mconcat
              [ Opt.long "debug-cmd"
              , Opt.metavar "CMD"
              , Opt.help "Command to pass to the debugger"
              ]
          )
      )
  pure GO.DebugOpts{..}

fsOptsParser :: Opt.Parser GO.FsOpts
fsOptsParser = Opt.parserOptionGroup "Symbolic I/O options" $ do
  fsRoot <- fsRootParser
  fsStdin <- symStdinParser
  fsSymFiles <- symFilesParser
  pure GO.FsOpts{..}

fsRootParser :: Opt.Parser (Maybe FilePath)
fsRootParser =
  Opt.optional
    ( Opt.strOption
        ( Opt.long "fs-root"
            <> Opt.metavar "PATH"
            <> Opt.help "The path to the symbolic filesystem"
        )
    )

globalsParser :: Opt.Parser GO.MutableGlobalState
globalsParser =
  GCE.enumParserDefault
    GO.Initialized
    [ Opt.long "globals"
    , Opt.help "how to initialize mutable global variables"
    ]

initPrecondParser :: Opt.Parser (Maybe FilePath)
initPrecondParser =
  Opt.optional $
    Opt.strOption
      ( Opt.long "initial-precondition"
          <> Opt.metavar "FILE"
          <> Opt.help "Initial precondition for use in refinement. Can be (partially) overridden by --arg-* and/or --*-startup-override."
      )

noHeuristicsParser :: Opt.Parser Bool
noHeuristicsParser =
  Opt.switch (Opt.long "no-heuristics" <> Opt.help "disable heuristics")

overridesParser :: Opt.Parser [FilePath]
overridesParser =
  Opt.many
    ( Opt.strOption
        ( Opt.long "overrides"
            <> Opt.metavar "FILE"
            <> Opt.help "overrides, in Crucible S-expression syntax"
        )
    )

overridesYamlParser :: Opt.Parser [FilePath]
overridesYamlParser =
  Opt.many
    ( Opt.strOption
        ( Opt.long "overrides-yaml"
            <> Opt.metavar "FILE"
            <> Opt.help "overrides, in YAML format"
        )
    )

solverParser :: Opt.Parser Solver
solverParser =
  GCE.enumParserDefault
    Yices
    [ Opt.long "solver"
    , Opt.help "The SMT solver to use for solving proof goals"
    ]

stackArgSlotsParser :: Opt.Parser GO.ExtraStackSlots
stackArgSlotsParser =
  Opt.option
    Opt.auto
    ( Opt.long "stack-argument-slots"
        <> Opt.metavar "NUM"
        <> Opt.value 0
        <> Opt.help "Reserve NUM slots above the stack frame for stack-spilled arguments"
    )

symFilesParser :: Opt.Parser (Map Text Integer)
symFilesParser =
  fmap Map.fromList $
    let hex :: TM.Parsec Void Text Integer
        hex = TM.single '0' >> TM.single 'x' >> TMCL.hexadecimal
        num = TMCL.decimal TM.<|> hex
        reader = do
          n <- num
          _ <- TM.chunk ":"
          path <- TM.takeRest
          pure (path, n)
     in Opt.many $
          Opt.option
            (megaparsecReader reader)
            ( Opt.long "sym-file"
                <> Opt.help "sizes and paths of symbolic files"
                <> Opt.metavar "SIZE:PATH"
            )

symStdinParser :: Opt.Parser Word64
symStdinParser =
  Opt.option
    Opt.auto
    ( Opt.long "sym-stdin"
        <> Opt.value 0
        <> Opt.showDefault
        <> Opt.metavar "N"
        <> Opt.help "populate stdin with this many symbolic bytes"
    )

simDumpCoverageParser :: Opt.Parser (Maybe FilePath)
simDumpCoverageParser =
  Opt.optional
    ( Opt.strOption $
        Opt.long "dump-coverage"
          <> Opt.metavar "FILE"
          <> Opt.help
            ( "Produces a JSON lines file of addresses of executed instructions. These addresses are relative to a section"
                ++ "index which is the first JSON array appended to the file"
            )
    )

simSkipFunsParser :: Opt.Parser (Set.Set FunctionName)
simSkipFunsParser =
  fmap Set.fromList $
    Opt.many $
      Opt.strOption
        ( Opt.long "skip"
            <> Opt.metavar "SYMBOL"
            <> Opt.help "Skip these functions during execution"
        )

simOpts :: Opt.Parser GO.SimOpts
simOpts = do
  simProgPath <- Opt.strArgument (Opt.help "filename of binary" <> Opt.metavar "FILENAME")
  simDebugOpts <- debugOptsParser
  simEntryPoints <- Opt.many entrypointParser
  simMutGlobs <- globalsParser
  simReqs <-
    Opt.many
      ( GCE.enumParser
          [ Opt.long "req"
          , Opt.help "names of requirements to test"
          ]
      )
  simNoHeuristics <- noHeuristicsParser
  simOverrides <- overridesParser
  simAddressOverrides <- addrOverridesParser
  simOverridesYaml <- overridesYamlParser
  simPathStrategy <-
    GCE.enumParserDefault
      GO.Sse
      [ Opt.long "path-strategy"
      , Opt.help "path exploration strategy"
      ]
  simPltStubs <-
    Opt.many $
      Opt.option
        (megaparsecReader pltStubParser)
        ( Opt.long "plt-stub"
            <> Opt.metavar "ADDR:NAME"
            <> Opt.help "PLT stubs to consider, in addition to those discovered via heuristics"
        )
  simProfileTo <-
    Opt.optional $
      Opt.strOption
        ( Opt.long "profile-to"
            <> Opt.metavar "DIR"
            <> Opt.help
              ( String.unlines
                  [ "Periodically log symbolic execution profiles to DIR."
                  , "Open 'DIR/profile.html' to view an HTML report of the profiles."
                  ]
              )
        )
  simStackArgumentSlots <- stackArgSlotsParser
  simSolver <- solverParser
  simRawBinaryMode <-
    Opt.switch
      (Opt.long "raw-binary" <> Opt.help "load binary as a position dependent non-elf")
  simErrorSymbolicFunCalls <-
    fmap GO.ErrorSymbolicFunCalls $
      Opt.parserOptionGroup callOptionsGroup $
        Opt.switch
          ( Opt.long "error-symbolic-fun-calls"
              <> Opt.help
                ( String.unlines
                    [ "Throw an error if attempting to call a symbolic function handle or pointer"
                    , "(by default, these calls will be skipped)"
                    ]
                )
          )
  simRawBinaryOffset <-
    Opt.option
      Opt.auto
      ( Opt.long "load-base"
          <> Opt.metavar "NUM"
          <> Opt.value 0
          <> Opt.help "The load base for a raw binary"
      )
  simErrorSymbolicSyscalls <-
    fmap GO.ErrorSymbolicSyscalls $
      Opt.parserOptionGroup callOptionsGroup $
        Opt.switch
          ( Opt.long "error-symbolic-syscalls"
              <> Opt.help
                ( String.unlines
                    [ "Throw an error if attempting to make a syscall with a symbolic number"
                    , "(by default, these calls will be skipped)"
                    ]
                )
          )
  simSkipInvalidCallAddrs <-
    fmap GO.SkipInvalidCallAddrs $
      Opt.parserOptionGroup callOptionsGroup $
        Opt.switch
          ( Opt.long "skip-invalid-call-addrs"
              <> Opt.help
                ( String.unlines
                    [ "Skip calls to invalid addresses."
                    , "(by default, these calls will result in an error)"
                    ]
                )
          )
  simSkipUnsupportedRelocs <-
    fmap GO.SkipUnsupportedRelocs $
      Opt.switch
        ( Opt.long "skip-unsupported-relocs"
            <> Opt.help "Populate unsupported relocations with symbolic data"
        )
  simRust <-
    Opt.switch
      ( Opt.long "rust"
          <> Opt.help "Use simulator settings that are more likely to work for Rust programs"
      )

  simFsOpts <- fsOptsParser
  simInitPrecondOpts <- initPrecondOptsParser
  simBoundsOpts <- boundsOptsParser
  simDumpCoverage <- simDumpCoverageParser
  simSkipFuns <- simSkipFunsParser
  pure GO.SimOpts{..}
 where
  callOptionsGroup = "Call options"

processSimOpts :: GO.SimOpts -> GO.SimOpts
processSimOpts sOpts =
  sOpts
    { GO.simBoundsOpts =
        (GO.simBoundsOpts sOpts)
          { GO.simTimeout =
              -- TODO(#37): Fully disable timeout if --debug is passed
              if GO.debug (GO.simDebugOpts sOpts)
                then secondsFromInt maxBound
                else GO.simTimeout (GO.simBoundsOpts sOpts)
          }
    }

------------------------------------------------------------
-- High-level entrypoints

opts :: Opt.Parser GO.Opts
opts = do
  optsSimOpts <- processSimOpts <$> simOpts
  optsJSON <- Opt.switch (Opt.long "json" <> Opt.help "output JSON")

  let minSeverity = Sev.severityToNat Sev.Info
  -- count the `-v`s
  optsVerbosity <-
    Sev.natToSeverity . (+ minSeverity) . fromIntegral . List.length
      <$> Opt.many (Opt.flag' () (Opt.short 'v'))
  pure GO.Opts{..}

-- | Parse 'Opts'
optsInfo :: Opt.ParserInfo GO.Opts
optsInfo =
  Opt.info
    (opts <**> Opt.helper <**> versionP)
    ( Opt.fullDesc
        <> Opt.header
          "Check properties of binaries using under-constrained symbolic execution"
    )
 where
  versionP =
    Opt.infoOption
      verStr
      ( Opt.long "version"
          <> Opt.short 'V'
          <> Opt.help "Print version information"
      )

-- | Parse 'Opts' from a list of 'String's
optsFromList :: [String] -> IO GO.Opts
optsFromList = Opt.handleParseResult . Opt.execParserPure Opt.defaultPrefs optsInfo

-- | Parse 'Opts' from the command-line
optsFromArgs :: IO GO.Opts
optsFromArgs = Opt.execParser optsInfo
