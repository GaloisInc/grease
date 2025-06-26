-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch.X86.Reg (
  getX86Reg,
  putX86Reg,
  modifyX86Reg,
) where

import Control.Exception.Safe (MonadThrow, throw)
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.X86 qualified as X86 (X86_64)
import Data.Macaw.X86.Symbolic qualified as X86Sym
import Data.Macaw.X86.X86Reg qualified as X86
import Data.Parameterized.Context qualified as Ctx
import Data.Text qualified as Text
import Grease.Utility (GreaseException (..))
import Prelude hiding (mod)

-- | Retrieve the value of an x86 register, throwing a 'GreaseException' if it
-- isn\'t present.
getX86Reg ::
  MonadThrow m =>
  X86.X86Reg t ->
  Ctx.Assignment f (Symbolic.MacawCrucibleRegTypes X86.X86_64) ->
  m (f (Symbolic.ToCrucibleType t))
getX86Reg reg regs =
  case X86Sym.lookupX86Reg reg regs of
    Nothing -> throw (GreaseException (Text.pack ("Missing register: " ++ show reg)))
    Just r -> pure r

-- | Set the value of an x86 register, throwing a 'GreaseException' if it
-- isn\'t present.
putX86Reg ::
  MonadThrow m =>
  X86.X86Reg t ->
  f (Symbolic.ToCrucibleType t) ->
  Ctx.Assignment f (Symbolic.MacawCrucibleRegTypes X86.X86_64) ->
  m (Ctx.Assignment f (Symbolic.MacawCrucibleRegTypes X86.X86_64))
putX86Reg reg val regs =
  case X86Sym.updateX86Reg reg (\_ -> val) regs of
    Nothing -> throw (GreaseException (Text.pack ("Missing register: " ++ show reg)))
    Just r -> pure r

-- | Monadically modify the value of an x86 register, throwing a
-- 'GreaseException' if it isn\'t present.
modifyX86Reg ::
  MonadThrow m =>
  Ctx.Assignment f (Symbolic.MacawCrucibleRegTypes X86.X86_64) ->
  X86.X86Reg t ->
  (f (Symbolic.ToCrucibleType t) -> m (f (Symbolic.ToCrucibleType t))) ->
  m (Ctx.Assignment f (Symbolic.MacawCrucibleRegTypes X86.X86_64))
modifyX86Reg regs reg mod = do
  val <- getX86Reg reg regs
  val' <- mod val
  putX86Reg reg val' regs
