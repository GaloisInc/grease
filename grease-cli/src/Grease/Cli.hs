{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Cli (
  optsInfo,
  optsFromList,
  optsFromArgs,
) where

import Control.Applicative (optional, (<**>))
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import Data.String qualified as String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Grease.Diagnostic.Severity qualified as Sev
import Grease.Entrypoint
import Grease.Macaw.PLT
import Grease.Options (SimOpts (simEnableDebugInfoPreconditions))
import Grease.Options qualified as GO
import Grease.Panic (panic)
import Grease.Requirement (displayReq, reqParser)
import Grease.Solver (Solver (..))
import Grease.Version (verStr)
import Options.Applicative qualified as Opt
import Text.Megaparsec qualified as TM

megaparsecReader :: TM.Parsec Void Text a -> Opt.ReadM a
megaparsecReader p = Opt.eitherReader $ \rawStr ->
  case TM.parse p "" (String.fromString rawStr) of
    Left err -> Left $ TM.errorBundlePretty err
    Right val -> Right val

-- Given an enumeration type, construct an @optparse-applicative@ metavar that
-- displays all possible variants. For example, given @data Letter = A | B | C@,
-- this would produce the metavar @(A|B|C)@.
boundedEnumMetavar ::
  forall a f proxy.
  (Bounded a, Enum a, Opt.HasMetavar f, Show a) =>
  proxy a ->
  Opt.Mod f a
boundedEnumMetavar _ = Opt.metavar $ varShowS ""
 where
  -- Use ShowS (i.e., a difference list of strings) below to amortize the cost
  -- of appending strings.
  varShowS :: ShowS
  varShowS =
    showParen True $
      List.foldr (.) id $
        List.intersperse (showChar '|') $
          List.map shows [minBound @a .. maxBound @a]

entrypointParser :: Opt.Parser Entrypoint
entrypointParser =
  inGroup addressParser
    Opt.<|> inGroup symbolParser
    Opt.<|> inGroup coreDumpParser
    Opt.<|> inGroup addressStartupOvParser
    Opt.<|> inGroup symbolStartupOvParser
 where
  inGroup = Opt.parserOptionGroup "Entrypoint options"

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

simOpts :: Opt.Parser GO.SimOpts
simOpts = do
  simProgPath <- Opt.strArgument (Opt.help "filename of binary" <> Opt.metavar "FILENAME")
  simDebug <- Opt.switch (Opt.long "debug" <> Opt.help "run the debugger")
  simEntryPoints <- Opt.many entrypointParser
  simMutGlobs <-
    Opt.option
      Opt.auto
      ( Opt.long "globals"
          <> Opt.help ("how to initialize mutable global variables " List.++ describeOptions allMutableGlobalStateStrs)
          <> Opt.value GO.Initialized
          <> Opt.showDefault
          <> Opt.completeWith allMutableGlobalStateStrs
      )
  simReqs <-
    Opt.many
      ( Opt.option
          (megaparsecReader reqParser)
          ( Opt.long "req"
              <> Opt.help ("names of requirements to test " List.++ describeOptions allRequirementStrs)
              <> Opt.metavar "REQS"
              <> Opt.completeWith allRequirementStrs
          )
      )
  simMaxIters <-
    optional $
      Opt.option
        Opt.auto
        ( Opt.long "iters"
            <> Opt.help "limit maximum number of iterations of the refinement loop"
            <> Opt.metavar "N"
        )
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
  simNoHeuristics <- Opt.switch (Opt.long "no-heuristics" <> Opt.help "disable heuristics")
  simOverrides <-
    Opt.many
      ( Opt.strOption
          ( Opt.long "overrides"
              <> Opt.metavar "FILE"
              <> Opt.help "overrides, in Crucible S-expression syntax"
          )
      )
  simOverridesYaml <-
    Opt.many
      ( Opt.strOption
          ( Opt.long "overrides-yaml"
              <> Opt.metavar "FILE"
              <> Opt.help "overrides, in YAML format"
          )
      )
  simTimeout <-
    GO.Milliseconds
      <$> Opt.option
        Opt.auto
        ( Opt.long "timeout"
            <> Opt.help "timeout (in milliseconds)"
            <> Opt.metavar "MILLIS"
            <> Opt.showDefault
            <> Opt.value GO.defaultTimeout
        )
  simPltStubs <-
    Opt.many $
      Opt.option
        (megaparsecReader pltStubParser)
        ( Opt.long "plt-stub"
            <> Opt.metavar "ADDR:NAME"
            <> Opt.help "PLT stubs to consider, in addition to those discovered via heuristics"
        )
  simInitialPreconditions <-
    Opt.optional $
      Opt.strOption
        ( Opt.long "initial-precondition"
            <> Opt.metavar "FILE"
            <> Opt.help "Initial precondition for use in refinement"
        )
  simTypeUnrollingBound <-
    GO.TypeUnrollingBound
      <$> Opt.option
        Opt.auto
        ( Opt.long "type-unrolling-bound"
            <> Opt.help "Number of recursive pointers to visit during DWARF shape building"
            <> Opt.showDefault
            <> Opt.value GO.defaultTypeUnrollingBound
        )
  simEnableDebugInfoPreconditions <-
    Opt.switch (Opt.long "use-debug-info-types" <> Opt.help "Use types in debug info to infer initial preconditions. Superseded by --initial-precondition.")
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
  simStackArgumentSlots <-
    Opt.option
      Opt.auto
      ( Opt.long "stack-argument-slots"
          <> Opt.metavar "NUM"
          <> Opt.value 0
          <> Opt.help "Reserve NUM slots above the stack frame for stack-spilled arguments"
      )
  simSolver <-
    Opt.option
      Opt.auto
      ( Opt.long "solver"
          <> boundedEnumMetavar (Proxy @Solver)
          <> Opt.value Yices
          <> Opt.showDefault
          <> Opt.help "The SMT solver to use for solving proof goals"
      )
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
  simRust <-
    Opt.switch
      ( Opt.long "rust"
          <> Opt.help "Use simulator settings that are more likely to work for Rust programs"
      )
  simFsRoot <-
    Opt.optional
      ( Opt.strOption
          ( Opt.long "fs-root"
              <> Opt.metavar "PATH"
              <> Opt.help "The path to the symbolic filesystem"
          )
      )
  pure GO.SimOpts{..}
 where
  callOptionsGroup = "Call options"

  allMutableGlobalStateStrs :: [String]
  allMutableGlobalStateStrs = List.map show GO.allMutableGlobalStates

  allRequirementStrs :: [String]
  allRequirementStrs =
    List.map (Text.unpack . displayReq) [minBound .. maxBound]

  -- Format a list of two or more possible values for a command-line
  -- option by separating each value with a comma (if necessary) and
  -- parenthesizing the results.
  --
  -- Precondition: the list contains two or more values.
  describeOptions :: [String] -> String
  describeOptions ls = showParen True description ""
   where
    description :: ShowS
    description =
      case unsnoc (List.map showString ls) of
        Just ([x], y) ->
          x . showString " or " . y
        Just (xs, y) ->
          List.foldr (.) id (List.intersperse (showString ", ") xs)
            . showString ", or "
            . y
        _ ->
          panic "opts.describeOptions" $
            [ "Precondition violated (list contains fewer than two values:"
            ]
              List.++ ls

  -- This was introduced in `base` in `base-4.19.0.0` (GHC 9.8), so we
  -- backport its definition here for backwards compatibility.
  unsnoc :: forall a. [a] -> Maybe ([a], a)
  unsnoc = List.foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
  {-# INLINEABLE unsnoc #-}

processSimOpts :: GO.SimOpts -> GO.SimOpts
processSimOpts sOpts =
  sOpts
    { GO.simTimeout =
        -- TODO(#37): Fully disable timeout if --debug is passed
        if GO.simDebug sOpts
          then GO.Milliseconds maxBound
          else GO.simTimeout sOpts
    }

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
