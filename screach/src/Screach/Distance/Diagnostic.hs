{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Screach.Distance.Diagnostic (Diagnostic (..), severity) where

import Data.Macaw.Memory qualified as MM
import Data.Macaw.Symbolic qualified as Symbolic
import Grease.Diagnostic.Severity (Severity (Debug, Info, Warn))
import Grease.Macaw.Arch (ArchRegCFG)
import Lang.Crucible.CFG.Extension qualified as CCE
import Lang.Crucible.CFG.Reg qualified as CCR
import Prettyprinter qualified as PP
import What4.ProgramLoc qualified as WPL

data Diagnostic where
  MinTarget :: WPL.ProgramLoc -> Diagnostic
  DiscoveredCFG ::
    (CCE.PrettyExt (Symbolic.MacawExt arch)) => MM.MemWord w -> ArchRegCFG arch -> Diagnostic
  GenericDebugOutput :: (PP.Pretty a) => a -> Diagnostic
  NoCalleesForFunction :: WPL.ProgramLoc -> Diagnostic
  NoCalleesForCallsite :: WPL.ProgramLoc -> WPL.ProgramLoc -> Diagnostic
  NoAddressesForCallsite :: WPL.ProgramLoc -> WPL.ProgramLoc -> Diagnostic
  FailedToResolveAddr :: MM.MemWord w -> Diagnostic

severity :: Diagnostic -> Severity
severity =
  \case
    MinTarget{} -> Info
    DiscoveredCFG{} -> Info
    GenericDebugOutput{} -> Debug
    NoCalleesForFunction{} -> Warn
    NoCalleesForCallsite{} -> Warn
    FailedToResolveAddr{} -> Warn
    NoAddressesForCallsite{} -> Warn

instance PP.Pretty Diagnostic where
  pretty (MinTarget w) = PP.cat ["Entering solving target distance for: ", PP.pretty (WPL.plSourceLoc w)]
  pretty (DiscoveredCFG w (CCR.SomeCFG cfgReg)) =
    PP.vcat
      [ PP.cat ["Recovered CFG for:", PP.pretty w]
      , PP.pretty cfgReg
      ]
  pretty (GenericDebugOutput out) = PP.vcat ["Generic debug output from distance:", PP.pretty out]
  pretty (NoCalleesForFunction f) = PP.cat ["No callees for function:", PP.pretty (WPL.plSourceLoc f)]
  pretty (NoCalleesForCallsite callsite f) =
    PP.cat
      [ "No callees for callsite:"
      , PP.pretty (WPL.plSourceLoc callsite)
      , "attempting to lookup containing function:"
      , PP.pretty (WPL.plSourceLoc f)
      ]
  pretty (FailedToResolveAddr w) = PP.cat ["Failed to resolve address when attempting to recover CFG:", PP.pretty w]
  pretty (NoAddressesForCallsite caller callsite) =
    PP.hsep
      [ "Caller "
      , PP.pretty (WPL.plSourceLoc caller)
      , "at callsite"
      , PP.pretty (WPL.plSourceLoc callsite)
      , "does not have target addresses, may be an override."
      ]
