{-# LANGUAGE ImplicitParams #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Overrides.SExp (
  MacawSExpOverride (..),
  loadOverrides,
) where

import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Grease.Macaw.SimulatorState (MacawFnHandle, MacawOverride)
import Grease.Syntax (ParseProgramError, parseProgram)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Syntax (emptyParserHooks)
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Stubs.FunctionOverride qualified as Stubs
import Stubs.Wrapper qualified as Stubs

-- | A Macaw function override, corresponding to a single S-expression file.
data MacawSExpOverride p sym arch
  = MacawSExpOverride
  { msoPublicFnHandle :: MacawFnHandle arch
  -- ^ The handle for the public function, whose name matches that of the
  -- S-expression file.
  , msoPublicOverride :: MacawOverride p sym arch
  -- ^ The override for the public function, whose name matches that of the
  -- S-expression file.
  , msoSomeFunctionOverride :: Stubs.SomeFunctionOverride p sym arch
  -- ^ The 'SomeFunctionOverride' value for S-expression file. This is
  -- primarily needed to compute the 'MacawOverride' above, but it is still
  -- convenient to keep the 'SomeFunctionOverride' value around to access
  -- the argument types, result type, auxiliary function bindings, forward
  -- declarations, etc.
  }

-- | Parse overrides in the Macaw S-expression syntax.
loadOverrides ::
  Symbolic.SymArchConstraints arch =>
  [FilePath] ->
  C.HandleAllocator ->
  IO (Either ParseProgramError (Seq.Seq (Stubs.SomeFunctionOverride p sym arch)))
loadOverrides paths halloc = do
  results <- traverse (\path -> loadOverride path halloc) (Seq.fromList paths)
  pure (sequence results)

-- | Parse an override in the Macaw S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  forall sym arch p.
  Symbolic.SymArchConstraints arch =>
  FilePath ->
  C.HandleAllocator ->
  IO (Either ParseProgramError (Stubs.SomeFunctionOverride p sym arch))
loadOverride path halloc = do
  let ?parserHooks = machineCodeParserHooks Proxy emptyParserHooks
  progResult <- parseProgram halloc path
  case progResult of
    Left err -> pure $ Left err
    Right prog -> do
      CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
      override <- Stubs.parsedProgToFunctionOverride path prog
      pure $ Right override
