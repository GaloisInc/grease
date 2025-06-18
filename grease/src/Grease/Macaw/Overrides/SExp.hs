{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ImplicitParams #-}

module Grease.Macaw.Overrides.SExp
  ( MacawSExpOverride(..)
  , loadOverrides
  ) where

import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Syntax (machineCodeParserHooks)
import Data.Proxy (Proxy(..))
import Data.Sequence qualified as Seq
import Grease.Macaw.SimulatorState (MacawFnHandle, MacawOverride)
import Grease.Syntax (parseProgram)
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.Syntax (emptyParserHooks)
import Lang.Crucible.Syntax.Concrete qualified as CSyn
import Lang.Crucible.Syntax.Prog qualified as CSyn
import Stubs.FunctionOverride qualified as Stubs
import Stubs.Wrapper qualified as Stubs

-- | A Macaw function override, corresponding to a single S-expression file.
data MacawSExpOverride p sym arch =
  MacawSExpOverride
    { mfoPublicFnHandle :: MacawFnHandle arch
      -- ^ The handle for the public function, whose name matches that of the
      -- S-expression file.
    , mfoPublicOverride :: MacawOverride p sym arch
      -- ^ The override for the public function, whose name matches that of the
      -- S-expression file.
    , mfoSomeFunctionOverride :: Stubs.SomeFunctionOverride p sym arch
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
  IO (Seq.Seq (Stubs.SomeFunctionOverride p sym arch))
loadOverrides paths halloc =
  traverse
    (\path -> loadOverride path halloc)
    (Seq.fromList paths)

-- | Parse an override in the Macaw S-expression syntax. An override
-- cannot use @extern@.
loadOverride ::
  forall sym arch p.
  Symbolic.SymArchConstraints arch =>
  FilePath ->
  C.HandleAllocator ->
  IO (Stubs.SomeFunctionOverride p sym arch)
loadOverride path halloc = do
  let ?parserHooks = machineCodeParserHooks Proxy emptyParserHooks
  prog <- parseProgram halloc path
  CSyn.assertNoExterns (CSyn.parsedProgExterns prog)
  Stubs.parsedProgToFunctionOverride path prog

