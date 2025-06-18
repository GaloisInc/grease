{-|
Copyright        : (c) Galois, Inc. 2024
Description      : Support for LLVM overrides written in the S-expression syntax
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.LLVM.Overrides.SExp
  ( LLVMSExpOverride(..)
  , loadOverrides
  ) where

import Control.Exception.Safe (throw)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.LLVM.Overrides.Declare (mkDeclare)
import Grease.Syntax (parseProgram)
import Grease.Utility (GreaseException(..))
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Syntax (llvmParserHooks, emptyParserHooks)
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import System.FilePath (dropExtensions, takeBaseName)
import What4.FunctionName qualified as W4

-- | An LLVM function override, corresponding to a single S-expression file.
data LLVMSExpOverride p sym =
  LLVMSExpOverride
    { lsoPublicOverride :: CLLVM.SomeLLVMOverride p sym CLLVM.LLVM
      -- ^ The override for the public function, whose name matches that of the
      -- S-expression file.
    , lsoAuxiliaryOverrides :: [CLLVM.SomeLLVMOverride p sym CLLVM.LLVM]
      -- ^ Overrides for the auxiliary functions in the S-expression file.
    , lsoForwardDeclarations :: Map.Map W4.FunctionName C.SomeHandle
      -- ^ The map of names of forward declarations in the S-expression file to
      -- their handles.
    }

-- | Parse overrides in the Crucible-LLVM S-expression syntax.
loadOverrides ::
  Mem.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Seq.Seq (W4.FunctionName, LLVMSExpOverride p sym))
loadOverrides paths halloc mvar =
  traverse (\path -> loadOverride path halloc mvar) (Seq.fromList paths)

-- | Parse an override in the Crucible-LLVM S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  Mem.HasPtrWidth w =>
  FilePath ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (W4.FunctionName, LLVMSExpOverride p sym)
loadOverride path halloc mvar = do
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  let fnNameText = Text.pack $ dropExtensions $ takeBaseName path
  let fnName = W4.functionNameFromText fnNameText
  let (publicCfgs, auxCfgs) =
        List.partition (isPublicCblFun fnName) (CSyn.parsedProgCFGs prog)
  case publicCfgs of
    [publicCfg] -> do
      publicOv <- acfgToSomeLLVMOverride path publicCfg
      auxOvs <- traverse (acfgToSomeLLVMOverride path) auxCfgs
      let fwdDecs = CSyn.parsedProgForwardDecs prog
      pure
        ( fnName
        , LLVMSExpOverride
            { lsoPublicOverride = publicOv
            , lsoAuxiliaryOverrides = auxOvs
            , lsoForwardDeclarations = fwdDecs
            }
        )
    [] ->
      throw $ GreaseException $
        "Expected to find a function named `" <> fnNameText <> "` in `" <>
        Text.pack path <> "`"
    _:_ ->
      throw $ GreaseException $
        "Override `" <> Text.pack path <> "` contains multiple `" <>
        fnNameText <> "` functions"

-- | Convert an 'C.Reg.AnyCFG' for a function defined in a Crucible-LLVM
-- S-expression program to a 'CLLVM.SomeLLVMOverride' value.
acfgToSomeLLVMOverride ::
  Mem.HasPtrWidth w =>
  FilePath {- The file which defines the CFG's function.
              This is only used for error messages. -} ->
  C.Reg.AnyCFG CLLVM.LLVM ->
  IO (CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)
acfgToSomeLLVMOverride path (C.Reg.AnyCFG cfg) = do
  let argTys = C.Reg.cfgArgTypes cfg
  let retTy = C.Reg.cfgReturnType cfg
  C.SomeCFG ssa <- pure $ C.toSSA cfg
  let fnName = C.handleName (C.cfgHandle ssa)
  let name = Text.unpack (W4.functionName fnName)
  case mkDeclare name argTys retTy of
    Right decl ->
      pure $
      CLLVM.SomeLLVMOverride $
      CLLVM.LLVMOverride
        { CLLVM.llvmOverride_declare = decl
        , CLLVM.llvmOverride_args = argTys
        , CLLVM.llvmOverride_ret = retTy
        , CLLVM.llvmOverride_def =
            \_mvar args ->
              C.regValue <$> C.callCFG ssa (C.RegMap args)
        }
    Left err ->
      throw (GreaseException ("Bad override in file " <> Text.pack path <> ": " <> err))

-- | Does a function have the same name as the @.cbl@ file in which it is
-- defined? That is, is a function publicly visible from the @.cbl@ file?
isPublicCblFun :: W4.FunctionName -> C.Reg.AnyCFG ext -> Bool
isPublicCblFun fnName (C.Reg.AnyCFG cfg) =
  C.handleName (C.Reg.cfgHandle cfg) == fnName
