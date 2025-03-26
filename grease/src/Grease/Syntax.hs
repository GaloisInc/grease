{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ImplicitParams #-}

module Grease.Syntax
  ( parseProgram
  , parsedProgramCfgMap
  ) where

import Control.Exception.Safe (throw)

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Text.Megaparsec as MP

-- parameterized-utils
import Data.Parameterized.Nonce (globalNonceGenerator)

-- what4
import qualified What4.FunctionName as W4

-- crucible
import qualified Lang.Crucible.CFG.Extension as C
import qualified Lang.Crucible.CFG.Reg as C.Reg
import qualified Lang.Crucible.FunctionHandle as C

-- crucible-syntax
import qualified Lang.Crucible.Syntax.Atoms as CSyn (atom)
import qualified Lang.Crucible.Syntax.Concrete as CSyn
import qualified Lang.Crucible.Syntax.ExprParse as CSyn
import qualified Lang.Crucible.Syntax.SExpr as SExpr

import Grease.Utility (GreaseException(..))

-- | Parse a program in Crucible S-expression syntax
parseProgram ::
  ( C.IsSyntaxExtension ext
  , ?parserHooks :: CSyn.ParserHooks ext
  ) =>
  C.HandleAllocator ->
  FilePath ->
  IO (CSyn.ParsedProgram ext)
parseProgram halloc path = do
  txt <- Text.IO.readFile path
  case MP.parse (SExpr.skipWhitespace *> MP.many (SExpr.sexp CSyn.atom) <* MP.eof) path txt of
    Left err -> throw (GreaseException (Text.pack (MP.errorBundlePretty err)))
    Right v -> do
      parseResult <- CSyn.top globalNonceGenerator halloc [] $ CSyn.prog v
      case parseResult of
        Left (CSyn.SyntaxParseError e) ->
          throw (GreaseException (CSyn.printSyntaxError e))
        Left err -> throw (GreaseException (Text.pack (show err)))
        Right parsedProg -> pure parsedProg

parsedProgramCfgMap ::
  CSyn.ParsedProgram ext ->
  Map Text (C.Reg.AnyCFG ext)
parsedProgramCfgMap prog =
  let cfgName cfg = W4.functionName (C.handleName (C.Reg.cfgHandle cfg))
      cfgWithName c@(C.Reg.AnyCFG cfg) = (cfgName cfg, c)
  in Map.fromList (List.map cfgWithName (CSyn.parsedProgCFGs prog))
