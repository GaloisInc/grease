{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.ParserHooks
  ( parserHooks
  ) where

import Control.Applicative (empty)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState(..))
import Control.Monad.Writer.Strict (MonadWriter(..))
import Data.Parameterized.Some (Some(Some))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vec
import GHC.Natural (Natural)
import Lang.Crucible.CFG.Expr qualified as LCE
import Lang.Crucible.CFG.Reg qualified as LCCR
import Lang.Crucible.Syntax.Atoms qualified as LCSA
import Lang.Crucible.Syntax.Concrete qualified as LCSC
import Lang.Crucible.Syntax.Monad qualified as LCSM
import Lang.Crucible.Types qualified as LCT
import What4.Interface qualified as WI
import What4.ProgramLoc qualified as WP

-- | Hooks for parsing S-expression files, with GREASE built-ins.
parserHooks ::
  (?parserHooks :: LCSC.ParserHooks ext) =>
  LCSC.ParserHooks ext
parserHooks =
  LCSC.ParserHooks
  { LCSC.extensionTypeParser = LCSC.extensionTypeParser ?parserHooks
  , LCSC.extensionParser = parseGreaseOp
  }

-- These are documented in doc/sexp.md
parseGreaseOp ::
  ( MonadIO m
  , MonadState (LCSC.SyntaxState s) m
  , MonadWriter [WP.Posd (LCCR.Stmt ext s)] m
  , LCSM.MonadSyntax LCSA.Atomic m
  , LCE.IsSyntaxExtension ext
  , ?parserHooks :: LCSC.ParserHooks ext
  ) =>
  m (Some (LCCR.Atom s))
parseGreaseOp =
  LCSM.depCons LCSC.atomName $ \name ->
    case name of
      LCSA.AtomName "fresh-vec" ->
        LCSM.depCons LCSC.string $ \nmPrefix ->
          LCSM.depCons LCSC.isType $ \(Some tp) ->
            LCSM.depCons LCSC.nat $ \len -> do
              loc <- LCSM.position
              atomVal <- freshVec nmPrefix tp len
              endAtom <- LCSC.freshAtom loc atomVal
              return (Some endAtom)
      LCSA.AtomName _ -> LCSC.extensionParser ?parserHooks

-- | Generate a vector of the given length, where each element is a fresh
-- constant of the supplied type whose name is derived from the given string.
--
-- Source: <https://github.com/GaloisInc/ambient-verifier/blob/eab04abb9750825a25ec0cbe0379add63f05f6c6/src/Ambient/FunctionOverride/Extension.hs#L1118>
freshVec ::
  forall s ext tp m.
  ( MonadIO m
  , MonadState (LCSC.SyntaxState s) m
  , MonadWriter [WP.Posd (LCCR.Stmt ext s)] m
  , LCSM.MonadSyntax LCSA.Atomic m
  , LCE.IsSyntaxExtension ext
  ) =>
  Text ->
  LCT.TypeRepr tp ->
  Natural ->
  m (LCCR.AtomValue ext s (LCT.VectorType tp))
freshVec nmPrefix tp len =
  case tp of
    LCT.FloatRepr fi ->
      mkVec (LCCR.FreshFloat fi)
    LCT.NatRepr ->
      mkVec LCCR.FreshNat
    _ | LCT.AsBaseType bt <- LCT.asBaseType tp ->
          mkVec (LCCR.FreshConstant bt)
      | otherwise ->
          empty
  where
    -- Construct an expression that looks roughly like:
    --
    --   (vector <tp> (fresh <s>_0) ... (fresh <s>_<n-1>))
    --
    -- Where the implementation of `fresh` is determined by the first argument.
    mkVec :: (Maybe WI.SolverSymbol -> LCCR.AtomValue ext s tp)
          -> m (LCCR.AtomValue ext s (LCT.VectorType tp))
    mkVec mkFresh = do
      vec <- Vec.generateM (fromIntegral len) $ \i ->
        LCSC.freshAtom WP.InternalPos $ mkFresh $ Just $ WI.safeSymbol $
        Text.unpack nmPrefix ++ "_" ++ show i
      pure $ LCCR.EvalApp $ LCE.VectorLit tp vec
