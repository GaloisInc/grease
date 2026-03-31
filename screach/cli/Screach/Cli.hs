{-# LANGUAGE ApplicativeDo #-}

module Screach.Cli (
  cli,
  cliFromList,
  cliFromArgs,
  programConfig,
) where

import Control.Applicative (Alternative ((<|>)))
import Data.List qualified as List
import Data.String qualified as String
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Num (Natural)
import Grease.Cli qualified as GC
import Grease.Diagnostic.Severity qualified as Sev
import Options.Applicative qualified as Opts
import Screach.AnalysisLoc (
  AnalysisLoc (..),
  EntryLoc (..),
  TargetLoc (..),
  entryAddressStartupOvParser,
  entrySymbolStartupOvParser,
 )
import Screach.Config qualified as Conf
import Screach.Distance qualified as Dist
import Screach.RefinementOptions (AllSolutions (..), RefineReplay (..))
import Text.Megaparsec qualified as TM
import What4.FunctionName qualified as WFN

minSeverity :: Natural
minSeverity = Sev.severityToNat Sev.Info

-- | Parse an 'AnalysisLocAddress' value.
analysisLocAddressParser ::
  Opts.Mod Opts.OptionFields Word64 -> Opts.Parser AnalysisLoc
analysisLocAddressParser m =
  AnalysisLocAddress <$> Opts.option Opts.auto m

-- | Parse an 'AnalysisLocSymbol' value.
analysisLocSymbolParser ::
  Opts.Mod Opts.OptionFields WFN.FunctionName -> Opts.Parser AnalysisLoc
analysisLocSymbolParser m =
  AnalysisLocSymbol <$> Opts.strOption m

avoidLocParser :: Opts.Parser AnalysisLoc
avoidLocParser =
  analysisLocAddressParser (avoidMod "avoid-addr" "ADDR" "Address of an instruction")
    <|> analysisLocSymbolParser (avoidMod "avoid-symbol" "SYMBOL" "Symbol name of a function entrypoint")
 where
  avoidMod :: String -> String -> String -> Opts.Mod Opts.OptionFields a
  avoidMod long metavar what =
    Opts.long long
      <> Opts.metavar metavar
      <> Opts.help
        ( what
            ++ " to avoid executing in SDSE. This address will always be assumed to be infinite distance from the target and returns (meaning any path hitting this instruction will be scheduled last)."
        )

entryLocParser :: Opts.Parser EntryLoc
entryLocParser =
  entryLocNoStartupOv
    <$> analysisLocAddressParser (entryMod "entry-addr" "ADDR" "Address")
    <|> entryLocNoStartupOv
      <$> analysisLocSymbolParser (entryMod "entry-symbol" "SYMBOL" "Symbol name")
    <|> Opts.option
      (megaparsecReader entryAddressStartupOvParser)
      (entryModStartupOv "entry-addr-startup-override" "ADDR:FILE" "Address")
    <|> Opts.option
      (megaparsecReader entrySymbolStartupOvParser)
      (entryModStartupOv "entry-symbol-startup-override" "SYMBOL:FILE" "Symbol")
 where
  entryMod :: String -> String -> String -> Opts.Mod Opts.OptionFields a
  entryMod long metavar what =
    Opts.long long
      <> Opts.metavar metavar
      <> Opts.help (what ++ " of the entrypoint function")

  entryModStartupOv :: String -> String -> String -> Opts.Mod Opts.OptionFields a
  entryModStartupOv long metavar what =
    Opts.long long
      <> Opts.metavar metavar
      <> Opts.help
        ( what
            ++ " of the entrypoint function, and the path to its startup override (in Crucible S-expression syntax)"
        )

  entryLocNoStartupOv :: AnalysisLoc -> EntryLoc
  entryLocNoStartupOv aloc =
    EntryLoc
      { entryAnalysisLoc = aloc
      , entryStartupOvPath = Nothing
      }

targetLocParser :: Opts.Parser TargetLoc
targetLocParser =
  TargetLoc
    <$> ( analysisLocAddressParser (entryMod "target-addr" "ADDR" "Address")
            <|> analysisLocSymbolParser (entryMod "target-symbol" "SYMBOL" "Symbol name")
        )
 where
  entryMod :: String -> String -> String -> Opts.Mod Opts.OptionFields a
  entryMod long metavar what =
    Opts.long long
      <> Opts.metavar metavar
      <> Opts.help (what ++ " of the reachability target")

-- | Helper, not exported
megaparsecReader :: TM.Parsec Void Text a -> Opts.ReadM a
megaparsecReader p = Opts.eitherReader $ \rawStr ->
  case TM.parse p "" (String.fromString rawStr) of
    Left err -> Left $ TM.errorBundlePretty err
    Right val -> Right val

programConfig :: Opts.Parser Conf.ProgramConfig
programConfig =
  Conf.ProgramConfig
    <$> Opts.argument
      Opts.str
      (Opts.metavar "PROG" <> Opts.help "Program to analyze")
    <*> entryLocParser
    <*> ( Sev.natToSeverity . (+ minSeverity) . fromIntegral . List.length
            <$> Opts.many (Opts.flag' () (Opts.short 'v'))
        )
    <*> Opts.option
      Opts.auto
      ( Opts.long "stack-argument-slots"
          <> Opts.metavar "NUM"
          <> Opts.value 0
          <> Opts.help "Reserve NUM slots above the stack frame for stack-spilled arguments"
      )

conf :: Opts.Parser Conf.Config
conf =
  Conf.Config
    <$> programConfig
    <*> GC.boundsOptsParser
    <*> GC.callOptsParser
    <*> GC.debugOptsParser
    <*> GC.fsOptsParser
    <*> GC.initPrecondOptsParser
    <*> Opts.optional
      ( Opts.strOption
          ( Opts.long "callgraph"
              <> Opts.metavar "PATH"
              <> Opts.help
                "The path to the callgraph csv. This file is required when using --explore in order to measure distances."
          )
      )
    <*> targetLocParser
    <*> GC.globalsParser
    <*> GC.solverParser
    <*> ( RefineReplay
            <$> Opts.flag
              True
              False -- Note: flipped to make True default
              ( Opts.long "no-refine-replay"
                  <> Opts.help "Disable trace replay when refining"
              )
        )
    <*> Opts.flag
      False
      True
      ( Opts.long "explore"
          <> Opts.help "Use path-based exploration instead of path-merging, requires --callgraph."
      )
    <*> ( AllSolutions
            <$> Opts.flag
              False
              True
              ( Opts.long "all-solutions"
                  <> Opts.help "Continue searching paths even after the first path reaching the target is found"
              )
        )
    <*> GC.overridesParser
    <*> GC.overridesYamlParser
    <*> Opts.switch
      ( Opts.long "no-heuristics"
          <> Opts.help "disable heuristics"
      )
    <*> GC.addrOverridesParser
    <*> Opts.optional
      ( Opts.strOption
          ( Opts.long "target-override"
              <> Opts.metavar "FILE"
              <> Opts.help "Custom reachability assertion"
          )
      )
    <*> ( Dist.DefaultReturnDist
            <$> Opts.option
              Opts.auto
              ( Opts.long "default-return-dist"
                  <> Opts.help
                    "An assumed distance to a return when a call cannot be resolved when trying to determine the shortest path to a target"
                  <> Opts.value 10
              )
        )
    <*> Opts.many avoidLocParser
    <*> Opts.optional
      ( Opts.option
          Opts.auto
          ( Opts.long "target-containing-function-address"
              <> Opts.help
                "The containing function address for the target. This option is required for SDSE with a non function target e.g. --target-addr"
          )
      )
    <*> GC.simDumpCoverageParser
    <*> Opts.flag
          True
          False  -- Note: flipped to make True default
          ( Opts.long "no-path-sat"
              <> Opts.help "Disable path satisfiability checking"
          )
    <*> Opts.flag
          True
          False  -- Note: flipped to make True default
          ( Opts.long "no-assert-then-assume"
              <> Opts.help "Disable assert-then-assume (assertions don't make paths unsatisfiable)"
          )

-- | Parse a 'Conf.Config'
cli :: Opts.ParserInfo Conf.Config
cli =
  Opts.info (conf Opts.<**> Opts.helper) $
    mconcat
      [ Opts.fullDesc
      , Opts.header "screach - symbolic compositional reachability analyzer"
      ]

-- | Parse a 'Conf.Config' from a list of strings
cliFromList :: [String] -> IO Conf.Config
cliFromList = Opts.handleParseResult . Opts.execParserPure Opts.defaultPrefs cli

-- | Parse a 'Conf.Config' from the command-line
cliFromArgs :: IO Conf.Config
cliFromArgs = Opts.execParser cli
