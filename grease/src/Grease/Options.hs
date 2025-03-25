{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Grease.Options
  ( LoopBound(..)
  , Milliseconds(..)
  , MutableGlobalState(..)
  , ExtraStackSlots(..)
  , ErrorSymbolicFunCalls(..)
  , Opts(..)
  , optsInfo
  ) where

import Prelude (Bounded(..), Enum, Integral, Num(..), Real, fromIntegral)

import System.FilePath (FilePath)

import Control.Applicative ((<**>), optional, pure)

import Data.Bool (Bool(..))
import Data.Either (Either(..))
import Data.Eq (Eq)
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.Int (Int)
import Data.Maybe (Maybe (..), maybe)
import Data.Ord (Ord)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import qualified Data.String as String
import Data.String (String)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)

import qualified Text.Megaparsec as TM
import Text.Read (Read)
import Text.Show (Show(..), ShowS, showChar, showParen, showString, shows)

import qualified Options.Applicative as Opt

import           Grease.Diagnostic.Severity (Severity)
import           Grease.Entrypoint
import qualified Grease.Diagnostic.Severity as Sev
import           Grease.Macaw.PLT
import           Grease.Requirement (Requirement, displayReq, reqParser)
import           Grease.Panic (panic)
import           Grease.Solver (Solver(..))
import           Grease.Version (verStr)

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

megaparsecReader :: TM.Parsec Void Text a -> Opt.ReadM a
megaparsecReader p = Opt.eitherReader $ \rawStr ->
  case TM.parse p "" (String.fromString rawStr) of
    Left err  -> Left $ TM.errorBundlePretty err
    Right val -> Right val

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

-- Given an enumeration type, construct an @optparse-applicative@ metavar that
-- displays all possible variants. For example, given @data Letter = A | B | C@,
-- this would produce the metavar @(A|B|C)@.
boundedEnumMetavar ::
  forall a f proxy. (Bounded a, Enum a, Opt.HasMetavar f, Show a) =>
  proxy a -> Opt.Mod f a
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

entrypointParser :: Opt.Parser Entrypoint
entrypointParser =
    addressParser Opt.<|> symbolParser Opt.<|> coreDumpParser Opt.<|>
    addressStartupOvParser Opt.<|> symbolStartupOvParser
  where
    addressParser =
      entrypointNoStartupOv . EntrypointAddress <$>
      Opt.strOption
        ( Opt.long "address"
          <> Opt.help "address of entrypoint"
          <> Opt.metavar "ADDR"
        )

    symbolParser =
      entrypointNoStartupOv . EntrypointSymbolName <$>
      Opt.strOption
        ( Opt.long "symbol"
          <> Opt.help "name of entrypoint symbol"
          <> Opt.metavar "SYMBOL"
        )

    coreDumpParser =
      entrypointNoStartupOv . EntrypointCoreDump <$>
      Opt.strOption
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

opts :: Opt.Parser Opts
opts = do
  optsBinaryPath <- Opt.strArgument (Opt.help "filename of binary" <> Opt.metavar "FILENAME" )
  optsDebug <- Opt.switch (Opt.long "debug" <> Opt.help "run the debugger")
  optsEntrypoints <- Opt.many entrypointParser
  optsGlobals <-
    Opt.option Opt.auto ( Opt.long "globals"
                          <> Opt.help ("how to initialize mutable global variables " List.++ describeOptions allMutableGlobalStateStrs)
                          <> Opt.value Initialized
                          <> Opt.showDefault
                          <> Opt.completeWith allMutableGlobalStateStrs)
  optsRequirement <-
    Opt.many (Opt.option (megaparsecReader reqParser)
             (Opt.long "req"
              <> Opt.help ("names of requirements to test " List.++ describeOptions allRequirementStrs)
              <> Opt.metavar "REQS"
              <> Opt.completeWith allRequirementStrs))
  optsJSON <- Opt.switch (Opt.long "json" <> Opt.help "output JSON")
  optsIterations <- optional $ Opt.option Opt.auto (Opt.long "iters" <> Opt.help "maximum number of iterations of the refinement loop" <> Opt.metavar "N")
  optsLoopBound <- LoopBound <$> Opt.option Opt.auto (Opt.long "loop-bound" <> Opt.help "maximum number of executions of each loop in the program" <> Opt.metavar "N" <> Opt.value defaultLoopBound)
  optsNoHeuristics <- Opt.switch (Opt.long "no-heuristics" <> Opt.help "disable heuristics")
  optsOverrides <-
    Opt.many (Opt.strOption ( Opt.long "overrides"
                              <> Opt.metavar "FILE"
                              <> Opt.help "function overrides, in Crucible S-expression syntax"
                              ))
  optsTimeout <-  Milliseconds <$> Opt.option Opt.auto (Opt.long "timeout" <> Opt.help "timeout (in milliseconds, default 30000)" <> Opt.metavar "MILLIS" <> Opt.value defaultTimeout)
  let minSeverity = Sev.severityToNat Sev.Info
  -- count the `-v`s
  optsVerbosity <-
    Sev.natToSeverity . (+ minSeverity) . fromIntegral . List.length <$>
      Opt.many (Opt.flag' () (Opt.short 'v'))
  optsRust <-
    Opt.switch ( Opt.long "rust"
                 <> Opt.help "Use simulator settings that are more likely to work for Rust programs"
               )
  optsPltStubs <-
    Opt.many $
    Opt.option (megaparsecReader pltStubParser)
               ( Opt.long "plt-stub"
                 <> Opt.metavar "ADDR:NAME"
                 <> Opt.help "PLT stubs to consider, in addition to those discovered via heuristics"
                 )
  optsPrecond <-
    Opt.optional $
    Opt.strOption ( Opt.long "initial-precondition"
                 <> Opt.metavar "FILE"
                 <> Opt.help "Initial precondition for use in refinement")
  optsProfileTo <-
    Opt.optional $
    Opt.strOption ( Opt.long "profile-to"
                 <> Opt.metavar "DIR"
                 <> Opt.help (String.unlines
                      [ "Periodically log symbolic execution profiles to DIR."
                      , "Open 'DIR/profile.html' to view an HTML report of the profiles."
                      ]))
  optsStackArgumentSlots <-
    Opt.option Opt.auto ( Opt.long "stack-argument-slots"
                          <> Opt.metavar "NUM"
                          <> Opt.value 0
                          <> Opt.help "Reserve NUM slots above the stack frame for stack-spilled arguments"
                        )
  optsSolver <-
    Opt.option Opt.auto ( Opt.long "solver"
                          <> boundedEnumMetavar (Proxy @Solver)
                          <> Opt.value Yices
                          <> Opt.showDefault
                          <> Opt.help "The SMT solver to use for solving proof goals"
                        )
  optsErrorSymbolicFunCalls <-
    ErrorSymbolicFunCalls <$>
    Opt.switch ( Opt.long "error-symbolic-fun-calls"
                 <> Opt.help
                      (String.unlines
                        [ "Throw an error if attempting to call a symbolic function handle or pointer"
                        , "(by default, these calls will be skipped)"
                        ]))
  pure Opts{..}
    where
      allMutableGlobalStateStrs :: [String]
      allMutableGlobalStateStrs = List.map show allMutableGlobalStates

      allRequirementStrs :: [String]
      allRequirementStrs =
        List.map (Text.unpack . displayReq) [minBound .. maxBound]

      -- | Format a list of two or more possible values for a command-line
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
                List.foldr (.) id (List.intersperse (showString ", ") xs) .
                showString ", or " . y
              _ ->
                panic "opts.describeOptions" $
                  [ "Precondition violated (list contains fewer than two values:"
                  ] List.++ ls

      -- This was introduced in `base` in `base-4.19.0.0` (GHC 9.8), so we
      -- backport its definition here for backwards compatibility.
      unsnoc :: forall a. [a] -> Maybe ([a], a)
      unsnoc = List.foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
      {-# INLINABLE unsnoc #-}

optsInfo :: Opt.ParserInfo Opts
optsInfo =
  Opt.info
    (opts <**> Opt.helper <**> versionP)
    (  Opt.fullDesc
    <> Opt.header
         "Check properties of binaries using under-constrained symbolic execution"
    )
  where
    versionP = Opt.infoOption verStr
                 (  Opt.long "version"
                 <> Opt.short 'V'
                 <> Opt.help "Print version information"
                 )
