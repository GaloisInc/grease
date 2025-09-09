{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright        : (c) Galois, Inc. 2025
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Entrypoint (
  checkMacawEntrypointCfgsSignatures,
)
where

import Control.Exception.Safe (throw)
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.TraversableFC qualified as TFC
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Grease.Entrypoint qualified as GE
import Grease.Macaw (regStructRepr)
import Grease.Macaw.Arch (ArchContext)
import Grease.Utility (GreaseException (..), pshow, tshow)
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.FunctionHandle qualified as C
import Prettyprinter qualified as PP
import What4.FunctionName qualified as W4

-- | Check that the CFGs in an 'GE.EntrypointCFGs' have the signatures of valid
-- Macaw CFGs (i.e., it takes the register struct as its only argument and
-- returns it). Throw 'GreaseException' if not.
checkMacawEntrypointCfgsSignatures ::
  ArchContext arch ->
  GE.EntrypointCfgs (C.Reg.AnyCFG (Symbolic.MacawExt arch)) ->
  IO
    ( GE.EntrypointCfgs
        ( C.Reg.SomeCFG
            (Symbolic.MacawExt arch)
            (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
            (Symbolic.ArchRegStruct arch)
        )
    )
checkMacawEntrypointCfgsSignatures archCtx entrypointCfgs = do
  GE.EntrypointCfgs
    { GE.entrypointStartupOv = mbStartupOv
    , GE.entrypointCfg = C.Reg.AnyCFG entrypointCfg0
    } <-
    pure entrypointCfgs
  let entryName = W4.functionName (C.handleName (C.Reg.cfgHandle entrypointCfg0))
  let name = Text.concat ["CFG `", entryName, "`"]
  C.Reg.SomeCFG entrypointCfg' <- checkCfgSignature archCtx name entrypointCfg0
  mbStartupOvSome <- for mbStartupOv $ \startupOv -> do
    GE.StartupOv
      { GE.startupOvCfg = C.Reg.AnyCFG startupOvCfg0
      , GE.startupOvForwardDecs = fwdDecs
      } <-
      pure startupOv
    let entryName' = W4.functionName (C.handleName (C.Reg.cfgHandle startupOvCfg0))
    let name' = Text.concat ["startup override for `", entryName', "`"]
    C.Reg.SomeCFG startupOvCfg' <- checkCfgSignature archCtx name' startupOvCfg0
    pure $
      GE.StartupOv
        { GE.startupOvCfg = C.Reg.SomeCFG startupOvCfg'
        , GE.startupOvForwardDecs = fwdDecs
        }
  pure $
    GE.EntrypointCfgs
      { GE.entrypointStartupOv = mbStartupOvSome
      , GE.entrypointCfg = C.Reg.SomeCFG entrypointCfg'
      }

-- Ensure that this CFG is a well-formed Macaw CFG, i.e., it takes the
-- register struct as its only argument and returns it.
checkCfgSignature ::
  ArchContext arch ->
  Text.Text ->
  C.Reg.CFG (Symbolic.MacawExt arch) s init ret ->
  IO
    ( C.Reg.SomeCFG
        (Symbolic.MacawExt arch)
        (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
        (Symbolic.ArchRegStruct arch)
    )
checkCfgSignature archCtx name regCfg = do
  let expectedArgTys = Ctx.singleton (regStructRepr archCtx)
  let expectedRet = regStructRepr archCtx
  let argTys = C.Reg.cfgArgTypes regCfg
  let ret = C.Reg.cfgReturnType regCfg
  let url = "https://galoisinc.github.io/grease/sexp-progs.html"
  let doThrow :: Text.Text -> IO a
      doThrow msg =
        throw (GreaseException (msg <> "\n" <> "For more information, see " <> url))
  Refl <-
    case testEquality argTys expectedArgTys of
      Just r -> pure r
      Nothing ->
        let prettyArgs = TFC.toListFC PP.pretty argTys
         in doThrow $
              Text.unlines
                [ Text.unwords ["Bad argument types for", name <> ":", tshow prettyArgs]
                , "Expected a single argument, the struct of register values"
                ]
  Refl <-
    case testEquality ret expectedRet of
      Just r -> pure r
      Nothing ->
        doThrow $
          Text.unlines
            [ Text.unwords ["Bad return type for", name <> ":", pshow ret]
            , "Expected the struct of register values"
            ]
  pure (C.Reg.SomeCFG regCfg)
