{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Description      : Common functionality for parsing overrides
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Overrides (
  CantResolveOverrideCallback (..),
  OverrideNameError (..),
  partitionCfgs,
  isPublicCblFun,
) where

import Data.List qualified as List
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Prettyprinter qualified as PP
import What4.FunctionName qualified as W4

-- | Callback for handling cases where an override cannot be resolved.
newtype CantResolveOverrideCallback sym arch
  = CantResolveOverrideCallback
  { runCantResolveOverrideCallback ::
      forall p args ret rtp as r.
      W4.FunctionName ->
      C.FnHandle args ret ->
      CS.OverrideSim p sym arch rtp as r ()
  }

-- | Error type for 'partitionCfgs'.
data OverrideNameError
  = ExpectedFunctionNotFound W4.FunctionName FilePath
  | MultipleFunctionsFound W4.FunctionName FilePath

instance PP.Pretty OverrideNameError where
  pretty =
    \case
      ExpectedFunctionNotFound fnName path ->
        PP.nest 2 $
          PP.vcat
            [ "Expected to find a function named"
                PP.<+> PP.squotes (PP.pretty fnName)
            , "in" PP.<+> PP.pretty path
            , "see https://galoisinc.github.io/grease/overrides.html#override-file-naming-conventions"
            ]
      MultipleFunctionsFound fnName path ->
        PP.nest 2 $
          PP.vcat
            [ PP.hsep
                [ "Override contains multiple"
                , PP.squotes (PP.pretty fnName)
                , "functions"
                ]
            , "in" PP.<+> PP.pretty path
            , "see https://galoisinc.github.io/grease/overrides.html#override-file-naming-conventions"
            ]

-- | Partition CFGs from a parsed program into public and auxiliary functions.
-- The public function is the one whose name matches the function name.
partitionCfgs ::
  W4.FunctionName ->
  FilePath ->
  CSyn.ParsedProgram ext ->
  Either
    OverrideNameError
    (C.Reg.AnyCFG ext, [C.Reg.AnyCFG ext])
partitionCfgs fnName path prog = do
  let (publicCfgs, auxCfgs) =
        List.partition (isPublicCblFun fnName) (CSyn.parsedProgCFGs prog)
  case publicCfgs of
    [mainCfg] -> Right (mainCfg, auxCfgs)
    [] -> Left (ExpectedFunctionNotFound fnName path)
    _ : _ -> Left (MultipleFunctionsFound fnName path)

-- | Does a function have the same name as the @.cbl@ file in which it is
-- defined? That is, is a function publicly visible from the @.cbl@ file?
isPublicCblFun :: W4.FunctionName -> C.Reg.AnyCFG ext -> Bool
isPublicCblFun fnName (C.Reg.AnyCFG cfg) =
  C.handleName (C.Reg.cfgHandle cfg) == fnName
