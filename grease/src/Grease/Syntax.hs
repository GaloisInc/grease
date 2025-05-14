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
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Grease.Utility (GreaseException(..))
import Lang.Crucible.CFG.Extension qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Syntax.Atoms qualified as CSyn (atom)
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.ExprParse qualified as CSyn
import Lang.Crucible.Syntax.SExpr qualified as SExpr
import Text.Megaparsec qualified as MP
import What4.FunctionName qualified as W4

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
