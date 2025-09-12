{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Description      : Support for LLVM overrides written in the S-expression syntax
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.LLVM.Overrides.SExp (
  AnyLLVMOverride (..),
  LLVMSExpOverride (..),
  LLVMSExpOverrideError (..),
  loadOverrides,
) where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.LLVM.Overrides.Declare (mkDeclare)
import Grease.Syntax (parseProgram)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as Mem
import Lang.Crucible.LLVM.Syntax (emptyParserHooks, llvmParserHooks)
import Lang.Crucible.Simulator qualified as C
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Prettyprinter qualified as PP
import System.FilePath (dropExtensions, takeBaseName)
import What4.FunctionName qualified as W4

-- | Error type for 'loadOverrides'.
data LLVMSExpOverrideError
  = ExpectedFunctionNotFound W4.FunctionName FilePath
  | MultipleFunctionsFound W4.FunctionName FilePath
  | UnsupportedType FilePath Text.Text

instance PP.Pretty LLVMSExpOverrideError where
  pretty =
    \case
      ExpectedFunctionNotFound fnName path ->
        PP.nest 2 $
          PP.vcat
            [ "Expected to find a function named"
                PP.<+> PP.squotes (PP.pretty fnName)
            , "in" PP.<+> PP.pretty path
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
            ]
      UnsupportedType path err ->
        PP.nest 2 $
          PP.vcat
            [ "Unsupported type in override in file" PP.<+> PP.pretty path
            , PP.pretty err
            ]

-- | A 'CLLVM.SomeLLVMOverride' quantified at a higher rank over @p@ and @sym@.
newtype AnyLLVMOverride
  = AnyLLVMOverride (forall p sym. CLLVM.SomeLLVMOverride p sym CLLVM.LLVM)

-- | An LLVM function override, corresponding to a single S-expression file.
data LLVMSExpOverride
  = LLVMSExpOverride
  { lsoPublicOverride :: AnyLLVMOverride
  -- ^ The override for the public function, whose name matches that of the
  -- S-expression file.
  , lsoAuxiliaryOverrides :: [AnyLLVMOverride]
  -- ^ Overrides for the auxiliary functions in the S-expression file.
  , lsoForwardDeclarations :: Map.Map W4.FunctionName C.SomeHandle
  -- ^ The map of names of forward declarations in the S-expression file to
  -- their handles.
  }

partitionCfgs ::
  W4.FunctionName ->
  FilePath ->
  CSyn.ParsedProgram CLLVM.LLVM ->
  Either
    LLVMSExpOverrideError
    (C.Reg.AnyCFG CLLVM.LLVM, [C.Reg.AnyCFG CLLVM.LLVM])
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

-- | Convert an 'C.Reg.AnyCFG' for a function defined in a Crucible-LLVM
-- S-expression program to a 'CLLVM.SomeLLVMOverride' value.
acfgToAnyLLVMOverride ::
  Mem.HasPtrWidth w =>
  FilePath {- The file which defines the CFG's function.
              This is only used for error messages. -} ->
  C.Reg.AnyCFG CLLVM.LLVM ->
  Either LLVMSExpOverrideError AnyLLVMOverride
acfgToAnyLLVMOverride path (C.Reg.AnyCFG cfg) = do
  let argTys = C.Reg.cfgArgTypes cfg
  let retTy = C.Reg.cfgReturnType cfg
  C.SomeCFG ssa <- pure $ C.toSSA cfg
  let fnName = C.handleName (C.cfgHandle ssa)
  let name = Text.unpack (W4.functionName fnName)
  case mkDeclare name argTys retTy of
    Left err -> Left (UnsupportedType path err)
    Right decl ->
      Right $
        AnyLLVMOverride $
          CLLVM.SomeLLVMOverride $
            CLLVM.LLVMOverride
              { CLLVM.llvmOverride_declare = decl
              , CLLVM.llvmOverride_args = argTys
              , CLLVM.llvmOverride_ret = retTy
              , CLLVM.llvmOverride_def =
                  \_mvar args ->
                    C.regValue <$> C.callCFG ssa (C.RegMap args)
              }

-- | Convert a parsed program to an LLVM S-expression override.
parsedProgToLLVMSExpOverride ::
  Mem.HasPtrWidth w =>
  FilePath ->
  CSyn.ParsedProgram CLLVM.LLVM ->
  Either LLVMSExpOverrideError (W4.FunctionName, LLVMSExpOverride)
parsedProgToLLVMSExpOverride path prog = do
  let fnNameText = Text.pack $ dropExtensions $ takeBaseName path
  let fnName = W4.functionNameFromText fnNameText
  (publicCfg, auxCfgs) <- partitionCfgs fnName path prog
  publicOv <- acfgToAnyLLVMOverride path publicCfg
  auxOvs <- traverse (acfgToAnyLLVMOverride path) auxCfgs
  let fwdDecs = CSyn.parsedProgForwardDecs prog
  let llvmOv =
        LLVMSExpOverride
          { lsoPublicOverride = publicOv
          , lsoAuxiliaryOverrides = auxOvs
          , lsoForwardDeclarations = fwdDecs
          }
  Right (fnName, llvmOv)

-- | Parse an override in the Crucible-LLVM S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  Mem.HasPtrWidth w =>
  FilePath ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Either LLVMSExpOverrideError (W4.FunctionName, LLVMSExpOverride))
loadOverride path halloc mvar = do
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  pure (parsedProgToLLVMSExpOverride path prog)

-- | Parse overrides in the Crucible-LLVM S-expression syntax.
loadOverrides ::
  Mem.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar Mem.Mem ->
  IO (Either LLVMSExpOverrideError (Seq.Seq (W4.FunctionName, LLVMSExpOverride)))
loadOverrides paths halloc mvar = do
  results <- traverse (\path -> loadOverride path halloc mvar) paths
  pure (Seq.fromList <$> sequence results)
