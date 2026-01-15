{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

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

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Grease.Overrides (OverrideNameError (..), partitionCfgs)
import Grease.Syntax (ParseProgramError, parseProgram)
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Intrinsics qualified as CLLVM
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.LLVM.Syntax (emptyParserHooks, llvmParserHooks)
import Lang.Crucible.Simulator qualified as CS
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Prettyprinter qualified as PP
import System.FilePath (dropExtensions, takeBaseName)
import Text.LLVM.AST qualified as L
import What4.FunctionName qualified as WFN

-- | Error type for 'loadOverrides'.
data LLVMSExpOverrideError
  = OverrideNameError OverrideNameError
  | LLVMSExpOverrideParseError ParseProgramError

instance PP.Pretty LLVMSExpOverrideError where
  pretty =
    \case
      OverrideNameError err -> PP.pretty err
      LLVMSExpOverrideParseError err -> PP.pretty err

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
  , lsoForwardDeclarations :: Map.Map WFN.FunctionName C.SomeHandle
  -- ^ The map of names of forward declarations in the S-expression file to
  -- their handles.
  }

-- | Convert an 'C.Reg.AnyCFG' for a function defined in a Crucible-LLVM
-- S-expression program to a 'CLLVM.SomeLLVMOverride' value.
acfgToAnyLLVMOverride ::
  CLM.HasPtrWidth w =>
  C.Reg.AnyCFG CLLVM.LLVM ->
  AnyLLVMOverride
acfgToAnyLLVMOverride (C.Reg.AnyCFG cfg) =
  let argTys = C.Reg.cfgArgTypes cfg
      retTy = C.Reg.cfgReturnType cfg
      fnName = C.handleName (C.Reg.cfgHandle cfg)
      name = Text.unpack (WFN.functionName fnName)
   in case C.toSSA cfg of
        C.SomeCFG ssa ->
          AnyLLVMOverride $
            CLLVM.SomeLLVMOverride $
              CLLVM.LLVMOverride
                { CLLVM.llvmOverride_name = L.Symbol name
                , CLLVM.llvmOverride_args = argTys
                , CLLVM.llvmOverride_ret = retTy
                , CLLVM.llvmOverride_def =
                    \_mvar args ->
                      CS.regValue <$> CS.callCFG ssa (CS.RegMap args)
                }

-- | Convert a parsed program to an LLVM S-expression override.
parsedProgToLLVMSExpOverride ::
  CLM.HasPtrWidth w =>
  FilePath ->
  CSyn.ParsedProgram CLLVM.LLVM ->
  Either LLVMSExpOverrideError (WFN.FunctionName, LLVMSExpOverride)
parsedProgToLLVMSExpOverride path prog = do
  let fnNameText = Text.pack $ dropExtensions $ takeBaseName path
  let fnName = WFN.functionNameFromText fnNameText
  (publicCfg, auxCfgs) <-
    case partitionCfgs fnName path prog of
      Left err -> Left (OverrideNameError err)
      Right result -> Right result
  let publicOv = acfgToAnyLLVMOverride publicCfg
  let auxOvs = List.map acfgToAnyLLVMOverride auxCfgs
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
