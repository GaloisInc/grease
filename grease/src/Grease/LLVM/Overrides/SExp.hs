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
  acfgToAnyLLVMOverride,
) where

import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.LLVM.Overrides.Declare (mkDeclare)
import Grease.Overrides (OverrideNameError, partitionCfgs)
import Grease.Syntax (ParseProgramError, parseProgram)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLI
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.Syntax (emptyParserHooks, llvmParserHooks)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Prettyprinter qualified as PP
import System.FilePath (dropExtensions, takeBaseName)
import What4.FunctionName qualified as WFN

-- | Error type for 'loadOverrides'.
data LLVMSExpOverrideError
  = OverrideNameError OverrideNameError
  | UnsupportedType FilePath Text.Text
  | LLVMSExpOverrideParseError ParseProgramError

instance PP.Pretty LLVMSExpOverrideError where
  pretty =
    \case
      OverrideNameError err -> PP.pretty err
      UnsupportedType path err ->
        PP.nest 2 $
          PP.vcat
            [ "Unsupported type in override in file" PP.<+> PP.pretty path
            , PP.pretty err
            ]
      LLVMSExpOverrideParseError err -> PP.pretty err

-- | A 'CLLVM.SomeLLVMOverride' quantified at a higher rank over @p@ and @sym@.
newtype AnyLLVMOverride
  = AnyLLVMOverride (forall p sym. CLI.SomeLLVMOverride p sym CLI.LLVM)

-- | An LLVM function override, corresponding to a single S-expression file.
data LLVMSExpOverride
  = LLVMSExpOverride
  { lsoPublicOverride :: AnyLLVMOverride
  -- ^ The override for the public function, whose name matches that of the
  -- S-expression file.
  , lsoAuxiliaryOverrides :: [AnyLLVMOverride]
  -- ^ Overrides for the auxiliary functions in the S-expression file.
  , lsoForwardDeclarations :: Map.Map WFN.FunctionName C.SomeHandle
  -- ^ The map of names of forward declarations in the S-expression file to
  -- their handles.
  }

-- | Convert an 'C.Reg.AnyCFG' for a function defined in a Crucible-LLVM
-- S-expression program to a 'CLLVM.SomeLLVMOverride' value.
acfgToAnyLLVMOverride ::
  CLM.HasPtrWidth w =>
  FilePath {- The file which defines the CFG's function.
              This is only used for error messages. -} ->
  C.Reg.AnyCFG CLI.LLVM ->
  Either LLVMSExpOverrideError AnyLLVMOverride
acfgToAnyLLVMOverride path (C.Reg.AnyCFG cfg) = do
  let argTys = C.Reg.cfgArgTypes cfg
  let retTy = C.Reg.cfgReturnType cfg
  C.SomeCFG ssa <- pure $ C.toSSA cfg
  let fnName = C.handleName (C.cfgHandle ssa)
  let name = Text.unpack (WFN.functionName fnName)
  case mkDeclare name argTys retTy of
    Left err -> Left (UnsupportedType path err)
    Right decl ->
      Right $
        AnyLLVMOverride $
          CLI.SomeLLVMOverride $
            CLI.LLVMOverride
              { CLI.llvmOverride_declare = decl
              , CLI.llvmOverride_args = argTys
              , CLI.llvmOverride_ret = retTy
              , CLI.llvmOverride_def =
                  \_mvar args ->
                    CS.regValue <$> CS.callCFG ssa (CS.RegMap args)
              }

-- | Convert a parsed program to an LLVM S-expression override.
parsedProgToLLVMSExpOverride ::
  CLM.HasPtrWidth w =>
  FilePath ->
  CSyn.ParsedProgram CLI.LLVM ->
  Either LLVMSExpOverrideError (WFN.FunctionName, LLVMSExpOverride)
parsedProgToLLVMSExpOverride path prog = do
  let fnNameText = Text.pack $ dropExtensions $ takeBaseName path
  let fnName = WFN.functionNameFromText fnNameText
  (publicCfg, auxCfgs) <-
    case partitionCfgs fnName path prog of
      Left err -> Left (OverrideNameError err)
      Right result -> Right result
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
  CLM.HasPtrWidth w =>
  FilePath ->
  C.HandleAllocator ->
  C.GlobalVar CLM.Mem ->
  IO (Either LLVMSExpOverrideError (WFN.FunctionName, LLVMSExpOverride))
loadOverride path halloc mvar = do
  let ?parserHooks = llvmParserHooks emptyParserHooks mvar
  progResult <- parseProgram halloc path
  case progResult of
    Left err -> pure $ Left $ LLVMSExpOverrideParseError err
    Right prog -> do
      CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
      pure $ parsedProgToLLVMSExpOverride path prog

-- | Parse overrides in the Crucible-LLVM S-expression syntax.
loadOverrides ::
  CLM.HasPtrWidth w =>
  [FilePath] ->
  C.HandleAllocator ->
  C.GlobalVar CLM.Mem ->
  IO (Either LLVMSExpOverrideError (Seq.Seq (WFN.FunctionName, LLVMSExpOverride)))
loadOverrides paths halloc mvar = do
  results <- traverse (\path -> loadOverride path halloc mvar) paths
  pure (Seq.fromList <$> sequence results)
