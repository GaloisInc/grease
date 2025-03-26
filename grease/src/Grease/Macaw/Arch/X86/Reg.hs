{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

module Grease.Macaw.Arch.X86.Reg
  ( getX86Reg
  , putX86Reg
  , modifyX86Reg
  ) where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.Text as Text
import Prelude hiding (mod)

import Grease.Utility (GreaseException(..))

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx

-- macaw-x86
import qualified Data.Macaw.X86 as X86 (X86_64)
import qualified Data.Macaw.X86.X86Reg as X86

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic

-- macaw-x86-symbolic
import qualified Data.Macaw.X86.Symbolic as X86Sym

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

