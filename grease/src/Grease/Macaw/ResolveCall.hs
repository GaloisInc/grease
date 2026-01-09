{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.ResolveCall (
  -- * Looking up function handles
  lookupFunctionHandle,
  LookupFunctionHandleDispatch (..),
  defaultLookupFunctionHandleDispatch,
  LookupFunctionHandleResult (..),

  -- * Looking up syscall handles
  lookupSyscallHandle,
  LookupSyscallDispatch (..),
  defaultLookupSyscallDispatch,
  LookupSyscallResult (..),

  -- * Helper functions for looking up handles
  discoverFuncAddr,
) where

import Control.Applicative (pure)
import Control.Lens (to, (%~), (.~), (^.))
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.BitVector.Sized qualified as BV
import Data.Foldable (foldl')
import Data.Function (($), (&), (.))
import Data.Int (Int)
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Macaw.BinaryLoader.ELF qualified as Loader
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Discovery qualified as Discovery
import Data.Macaw.Discovery.Classifier qualified as Discovery
import Data.Macaw.Memory.ElfLoader qualified as EL
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Concretize qualified as Symbolic
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (..))
import Data.Parameterized.Context qualified as Ctx
import Data.Parameterized.Map qualified as MapF
import Data.Parameterized.NatRepr (knownNat)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Type.Equality (type (~))
import GHC.Word (Word64)
import Grease.Concretize.ToConcretize (HasToConcretize)
import Grease.Diagnostic (Diagnostic (..), GreaseLogAction)
import Grease.Macaw.Arch
import Grease.Macaw.Discovery (discoverFunction)
import Grease.Macaw.Overrides (lookupMacawForwardDeclarationOverride)
import Grease.Macaw.Overrides.SExp (MacawSExpOverride (..))
import Grease.Macaw.ResolveCall.Diagnostic qualified as Diag
import Grease.Macaw.SimulatorState
import Grease.Macaw.SkippedCall (SkippedFunctionCall (..), SkippedSyscall (..))
import Grease.Macaw.Syscall
import Grease.Options (ErrorSymbolicFunCalls (..), ErrorSymbolicSyscalls (..), SkipInvalidCallAddrs (..))
import Grease.Syntax (ResolvedOverridesYaml (..))
import Grease.Utility (OnlineSolverAndBackend, segoffToAbsoluteAddr)
import Lang.Crucible.Analysis.Postdom qualified as C
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.CFG.SSAConversion qualified as C
import Lang.Crucible.FunctionHandle qualified as C
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Lumberjack qualified as LJ
import Stubs.FunctionOverride qualified as Stubs
import Stubs.FunctionOverride.ForwardDeclarations qualified as Stubs
import Stubs.Memory.Common qualified as Stubs
import Stubs.Syscall qualified as Stubs
import System.IO (IO)
import What4.Expr qualified as W4
import What4.FunctionName qualified as WFN
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as W4
import Prelude (Integer, fromIntegral, otherwise, toInteger, (++))

doLog :: MonadIO m => GreaseLogAction -> Diag.Diagnostic -> m ()
doLog la diag = LJ.writeLog la (ResolveCallDiagnostic diag)

regStructRepr :: ArchContext arch -> C.TypeRepr (Symbolic.ArchRegStruct arch)
regStructRepr arch = C.StructRepr . Symbolic.crucArchRegTypes $ arch ^. archVals . to Symbolic.archFunctions

-- | Create a new override that post-composes an 'OverrideSim' action with an existing one.
useComposedOverride ::
  C.HandleAllocator ->
  ArchContext arch ->
  -- | Handle for existing override
  C.FnHandle (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) ->
  -- | Implementation of existing override
  MacawOverride p sym arch ->
  -- | Simulator state
  CS.SimState p sym (Symbolic.MacawExt arch) r f a ->
  -- | Name for new override
  WFN.FunctionName ->
  -- | New action to take
  ( forall r'.
    CS.RegValue sym (Symbolic.ArchRegStruct arch) ->
    CS.OverrideSim p sym (Symbolic.MacawExt arch) r' (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) (CS.RegValue sym (Symbolic.ArchRegStruct arch))
  ) ->
  IO
    ( C.FnHandle (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)
    , CS.SimState p sym (Symbolic.MacawExt arch) r f a
    )
useComposedOverride halloc arch handle0 override0 st funcName f = do
  handle <- C.mkHandle' halloc funcName (Ctx.Empty Ctx.:> regStructRepr arch) (regStructRepr arch)
  let override = CS.mkOverride' funcName (regStructRepr arch) $ do
        args <- CS.getOverrideArgs
        regs <- CS.callOverride handle0 override0 args
        f (CS.regValue regs)
  pure $ useFnHandleAndState handle (CS.UseOverride override) st

-- | Dispatch on the result of looking up a function handle. The
-- 'lookupFunctionHandle' function invokes a continuation of this type after it
-- has looked up a function handle, so the behavior of that function can be
-- customized by performing different actions for each
-- 'LookupFunctionHandleResult'.
newtype LookupFunctionHandleDispatch p sym arch
  = LookupFunctionHandleDispatch
      ( forall rtp blocks r ctx.
        CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
        CLM.MemImpl sym ->
        Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
        LookupFunctionHandleResult p sym arch ->
        IO
          ( C.FnHandle (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)
          , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
          )
      )

-- | An reasonable default implementation of 'LookupFunctionHandleDispatch'.
-- This invokes function handles (replacing them when overrides when they can be
-- found) if they can be found, and if not, skips them by simulating them as
-- no-ops.
defaultLookupFunctionHandleDispatch ::
  ( OnlineSolverAndBackend solver sym bak t st fs
  , Symbolic.SymArchConstraints arch
  , CLM.HasLLVMAnn sym
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , HasToConcretize p
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  (WFN.FunctionName -> IO ()) ->
  LookupFunctionHandleDispatch p sym arch
defaultLookupFunctionHandleDispatch bak la halloc arch memory funOvs errCb =
  LookupFunctionHandleDispatch $ \st mem regs lfhr -> do
    let
      -- Call a function handle, unless we have an override in which case call
      -- the override instead.
      callHandle hdl mbOv st' =
        case mbOv of
          Just macawFnOv ->
            useMacawSExpOverride bak la halloc arch funOvs errCb macawFnOv st'
          Nothing ->
            pure (hdl, st')

      -- Log that we have performed a function call.
      logFunctionCall fnName fnAddrOff = do
        mbReturnAddr <-
          (arch ^. archFunctionReturnAddr) bak (arch ^. archVals) regs mem
        let fnAbsAddr = segoffToAbsoluteAddr memory fnAddrOff
        doLog la $ Diag.FunctionCall fnName fnAbsAddr mbReturnAddr

    case lfhr of
      SkippedFunctionCall reason -> do
        doLog la $ Diag.SkippedFunctionCall reason
        let funcName = WFN.functionNameFromText "_grease_external"
        handle <- C.mkHandle' halloc funcName (Ctx.Empty Ctx.:> regStructRepr arch) (regStructRepr arch)
        let override = CS.mkOverride' funcName (regStructRepr arch) $ do
              args <- CS.getOverrideArgs
              let regs' = Ctx.last $ CS.regMap args
              (arch ^. archOffsetStackPointerPostCall) (CS.regValue regs')
        pure $ useFnHandleAndState handle (CS.UseOverride override) st
      CachedFnHandle fnAddrOff hdl mbOv -> do
        logFunctionCall (C.handleName hdl) fnAddrOff
        callHandle hdl mbOv st
      DiscoveredFnHandle fnAddrOff hdl mbOv -> do
        logFunctionCall (C.handleName hdl) fnAddrOff
        callHandle hdl mbOv st
      PltStubOverride pltStubAddrOff pltStubName macawFnOv -> do
        logFunctionCall pltStubName pltStubAddrOff
        useMacawSExpOverride bak la halloc arch funOvs errCb macawFnOv st

-- | The result of looking up a function handle.
data LookupFunctionHandleResult p sym arch where
  -- | The function handle could not be found for some reason, so it is skipped.
  SkippedFunctionCall ::
    -- | The reason for skipping the function.
    SkippedFunctionCall arch ->
    LookupFunctionHandleResult p sym arch
  -- | The function handle was previously registered.
  CachedFnHandle ::
    -- | The function's resolved address.
    MC.ArchSegmentOff arch ->
    -- | The function's handle.
    MacawFnHandle arch ->
    -- | The function's override (if one exists).
    Maybe (MacawSExpOverride p sym arch) ->
    LookupFunctionHandleResult p sym arch
  -- | This is a newly discovered function handle.
  DiscoveredFnHandle ::
    -- | The function's resolved address.
    MC.ArchSegmentOff arch ->
    -- | The function's handle.
    MacawFnHandle arch ->
    -- | The function's override (if one exists).
    Maybe (MacawSExpOverride p sym arch) ->
    LookupFunctionHandleResult p sym arch
  -- | This is a PLT stub function with a corresponding override.
  PltStubOverride ::
    -- | The PLT stub's resolved address.
    MC.ArchSegmentOff arch ->
    -- | The PLT stub's name.
    WFN.FunctionName ->
    -- | The PLT stub's override.
    MacawSExpOverride p sym arch ->
    LookupFunctionHandleResult p sym arch

-- | Attempt to look up a function handle.
--
-- The behavior of this function is documented in @doc/function-calls.md@.
lookupFunctionHandleResult ::
  forall arch sym bak solver scope st fs p rtp blocks r ctx cExt.
  ( OnlineSolverAndBackend solver sym bak scope st fs
  , Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  -- | Map of dynamic function names to their addresses
  Map.Map WFN.FunctionName (MC.ArchSegmentOff arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  ResolvedOverridesYaml (MC.ArchAddrWidth arch) ->
  -- | Functions that should be skipped even if they are defined
  Set WFN.FunctionName ->
  ErrorSymbolicFunCalls ->
  SkipInvalidCallAddrs ->
  CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
  Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
  IO
    ( LookupFunctionHandleResult p sym arch
    , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
    )
lookupFunctionHandleResult bak la halloc arch memory symMap pltStubs dynFunMap funOvs funAddrOvs skipFuns errorSymbolicFunCalls skipInvalidCallAddrs st regs = do
  -- First, obtain the address contained in the instruction pointer.
  symAddr0 <- (arch ^. archGetIP) regs
  -- Next, attempt to concretize the address. We must do this because it is
  -- possible that the address was obtained from a memory read, and due to the
  -- way macaw-symbolic's memory model works, such an address would be a fresh
  -- variable that is constrained to be equal to a concrete address. As such,
  -- we can only conclude that the address is concrete by consulting an SMT
  -- solver. (See the refine/bug/symbolic_ip test case for where this technique
  -- is essential.)
  --
  -- Using resolveSymBV is somewhat overkill, as if it is given a truly symbolic
  -- function address, then it will wastefully attempt to refine the lower and
  -- upper bounds of the address. (Refining the symbolic bounds won't help here,
  -- since we can only invoke concrete addresses.) We could make the truly
  -- symbolic case cheaper by leveraging the ideas in
  -- https://github.com/GaloisInc/what4/issues/259.
  symAddr1 <- Symbolic.resolveSymBV bak C.knownNat symAddr0

  case WI.asBV symAddr1 of
    -- If asBV returns Nothing here, despite the call to resolveSymBV above,
    -- then the address is truly symbolic. By default, we skip the call
    -- entirely, but if the user passes --error-symbolic-fun-calls, then this is
    -- treated as an error.
    Nothing ->
      if getErrorSymbolicFunCalls errorSymbolicFunCalls
        then
          CB.addFailedAssertion bak $
            CS.AssertFailureSimError
              "Failed to call function"
              "Cannot resolve a symbolic function address"
        else pure (SkippedFunctionCall SymbolicAddress, st)
    Just bv ->
      let
        -- This conversion is safe iff MC.ArchAddrWidth arch <= 64
        bvWord64 = fromIntegral @Integer @Word64 (BV.asUnsigned bv)
        bvMemWord = EL.memWord bvWord64
       in
        case Loader.resolveAbsoluteAddress memory bvMemWord of
          Nothing ->
            let addrString = BV.ppHex knownNat bv
             in if getSkipInvalidCallAddrs skipInvalidCallAddrs
                  then
                    pure (SkippedFunctionCall (InvalidAddress addrString), st)
                  else
                    CB.addFailedAssertion bak $ CS.AssertFailureSimError "Failed to call function" ("Invalid address: " ++ addrString)
          Just funcAddrOff -> go funcAddrOff
 where
  lookupOv ::
    WFN.FunctionName ->
    MC.ArchSegmentOff arch ->
    Maybe (MacawSExpOverride p sym arch)
  lookupOv funcName funcAddrOff =
    case Map.lookup funcAddrOff (getResolvedOverridesYaml funAddrOvs) of
      Just ovName -> Map.lookup ovName funOvs
      Nothing -> Map.lookup funcName funOvs

  maybeUserSkip ::
    WFN.FunctionName ->
    IO
      ( LookupFunctionHandleResult p sym arch
      , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
      ) ->
    IO
      ( LookupFunctionHandleResult p sym arch
      , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
      )
  maybeUserSkip nm els =
    if Set.member nm skipFuns
      then pure (SkippedFunctionCall (UserSkip nm), st)
      else els

  -- Given a resolved function address, compute a
  -- 'LookupFunctionHandleResult'. This function is recursive because we may
  -- need to handle PLT stubs that jump to other addresses within the same
  -- binary.
  go ::
    MC.ArchSegmentOff arch ->
    IO
      ( LookupFunctionHandleResult p sym arch
      , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
      )
  go funcAddrOff
    -- First, check if this is the address of a CFG we have already
    -- discovered
    | Just hdl <- Map.lookup funcAddrOff (st ^. stateDiscoveredFnHandles) =
        let nm = C.handleName hdl
         in maybeUserSkip nm $
              pure
                ( CachedFnHandle funcAddrOff hdl $
                    lookupOv nm funcAddrOff
                , st
                )
    -- Next, check if this is a PLT stub.
    | Just pltStubName <- Map.lookup funcAddrOff pltStubs =
        maybeUserSkip pltStubName $
          if
            | -- If a PLT stub jumps to an address within the same binary
              -- or shared library, resolve it...
              Just pltCallAddr <- Map.lookup pltStubName dynFunMap ->
                do
                  doLog la $ Diag.PltCall pltStubName funcAddrOff pltCallAddr
                  go pltCallAddr
            | otherwise ->
                case lookupOv pltStubName funcAddrOff of
                  -- ...otherwise, if there is an override for the PLT stub,
                  -- use it...
                  Just macawFnOv ->
                    pure
                      ( PltStubOverride funcAddrOff pltStubName macawFnOv
                      , st
                      )
                  -- ...otherwise, skip the PLT call entirely.
                  Nothing ->
                    -- TODO(#182): Option to make this an error
                    pure
                      ( SkippedFunctionCall $
                          PltNoOverride funcAddrOff pltStubName
                      , st
                      )
    -- Finally, check if this is a function that we should explore, and if
    -- so, use Macaw's code discovery to do so. See Note [Incremental code
    -- discovery] in Grease.Macaw.SimulatorState.
    --
    -- As a simple heuristic for whether a function is worthy of
    -- exploration, we check if the segment that the address inhabits is
    -- executable. This check is important to prevent simulating functions
    -- that, say, inhabit the .data section (which is common for binaries
    -- that fail the `in-text` requirement), as Macaw will simply crash
    -- when simulating them.
    | Discovery.isExecutableSegOff funcAddrOff = do
        fixedAddr <- (arch ^. archPCFixup) bak regs funcAddrOff
        (hdl, st') <-
          discoverFuncAddr la halloc arch memory symMap pltStubs fixedAddr st
        let nm = C.handleName hdl
        maybeUserSkip nm $
          pure
            ( DiscoveredFnHandle funcAddrOff hdl $
                lookupOv nm funcAddrOff
            , st'
            )
    | otherwise =
        pure (SkippedFunctionCall (NotExecutable funcAddrOff), st)

lookupFunctionHandle ::
  ( OnlineSolverAndBackend solver sym bak t st fs
  , Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  -- | Map of dynamic function names to their addresses
  Map.Map WFN.FunctionName (MC.ArchSegmentOff arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  ResolvedOverridesYaml (MC.ArchAddrWidth arch) ->
  -- | Functions that should be skipped even if they are defined
  Set WFN.FunctionName ->
  ErrorSymbolicFunCalls ->
  SkipInvalidCallAddrs ->
  LookupFunctionHandleDispatch p sym arch ->
  Symbolic.LookupFunctionHandle p sym arch
lookupFunctionHandle bak la halloc arch memory symMap pltStubs dynFunMap funOvs funAddrOvs skipFuns errorSymbolicFunCalls skipInvalidCallAddrs lfhd = Symbolic.LookupFunctionHandle $ \st mem regs -> do
  let LookupFunctionHandleDispatch dispatch = lfhd
  (res, st') <- lookupFunctionHandleResult bak la halloc arch memory symMap pltStubs dynFunMap funOvs funAddrOvs skipFuns errorSymbolicFunCalls skipInvalidCallAddrs st regs
  dispatch st' mem regs res

-- | Dispatch on the result of looking up a syscall override. The
-- 'lookupSyscallHandle' function invokes a continuation of this type after it
-- has looked up a syscall override, so the behavior of that function can be
-- customized by performing different actions for each 'LookupSyscallResult'.
newtype LookupSyscallDispatch p sym arch
  = LookupSyscallDispatch
      ( forall rtp blocks r ctx atps rtps.
        Ctx.Assignment C.TypeRepr atps ->
        Ctx.Assignment C.TypeRepr rtps ->
        CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
        CS.RegEntry sym (C.StructType atps) ->
        LookupSyscallResult p sym arch atps rtps ->
        IO
          ( C.FnHandle atps (C.StructType rtps)
          , CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx
          )
      )

-- | An implementation of 'LookupSyscallDispatch' that attempts to invoke
-- syscall overrides if they can be found. If an override cannot be found, the
-- syscall is simulated as a no-op.
--
-- The behavior of this function is documented in @doc/syscalls.md@.
defaultLookupSyscallDispatch ::
  OnlineSolverAndBackend solver sym bak t st fs =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  LookupSyscallDispatch p sym arch
defaultLookupSyscallDispatch bak la halloc arch =
  LookupSyscallDispatch $ \atps rtps st regs lsr ->
    case lsr of
      SkippedSyscall reason -> do
        -- Treat this syscall as a no-op during simulation.
        doLog la $ Diag.SkippedSyscall reason
        let funcName = WFN.functionNameFromText "_grease_syscall"
        handle <- C.mkHandle' halloc funcName atps (C.StructRepr rtps)
        let override =
              CS.mkOverride'
                funcName
                (C.StructRepr rtps)
                ( (arch ^. archSyscallReturnRegisters)
                    C.UnitRepr
                    (pure ())
                    atps
                    regs
                    rtps
                )
        pure $ useFnHandleAndState handle (CS.UseOverride override) st
      CachedSyscallFnHandle (Stubs.SyscallFnHandle syscallHdl) ->
        pure (syscallHdl, st)
      NewSyscall syscallName syscallNum (Stubs.SomeSyscall syscallOv) -> do
        doLog la $ Diag.SyscallOverride syscallName syscallNum
        let syscallFnName = WFN.functionNameFromText syscallName
        macawSyscallHdl <- C.mkHandle' halloc syscallFnName atps (C.StructRepr rtps)
        let macawSyscallOv = macawSyscallOverride bak arch atps rtps syscallOv
        pure $ useFnHandleAndState macawSyscallHdl (CS.UseOverride macawSyscallOv) st

-- | The result of looking up a syscall override.
data LookupSyscallResult p sym arch atps rtps where
  -- | The syscall override could not be found for some reason, so it is
  -- skipped.
  SkippedSyscall ::
    -- | The reason for skipping the syscall.
    SkippedSyscall ->
    LookupSyscallResult p sym arch atps rtps
  -- | The syscall has an override that was previously registered.
  CachedSyscallFnHandle ::
    -- | The handle for the syscall override.
    Stubs.SyscallFnHandle '(atps, rtps) ->
    LookupSyscallResult p sym arch atps rtps
  -- | The syscall has an override that has not yet been registered.
  NewSyscall ::
    -- | The syscall's name.
    Text ->
    -- | The syscall's number.
    Int ->
    -- | The syscall's override.
    Stubs.SomeSyscall p sym (Symbolic.MacawExt arch) ->
    LookupSyscallResult p sym arch atps rtps

-- | Attempt to look up a syscall override.
--
-- The behavior of this function is documented in @doc/syscalls.md@.
lookupSyscallResult ::
  ( CB.IsSymBackend sym bak
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  ArchContext arch ->
  -- | Map of names of overridden syscalls to their implementations
  Map.Map
    WFN.FunctionName
    (Stubs.SomeSyscall p sym (Symbolic.MacawExt arch)) ->
  ErrorSymbolicSyscalls ->
  Ctx.Assignment C.TypeRepr atps ->
  Ctx.Assignment C.TypeRepr rtps ->
  CS.CrucibleState p sym (Symbolic.MacawExt arch) rtp blocks r ctx ->
  CS.RegEntry sym (C.StructType atps) ->
  IO (LookupSyscallResult p sym arch atps rtps)
lookupSyscallResult bak arch syscallOvs errorSymbolicSyscalls atps rtps st regs = do
  symSyscallBV <- (arch ^. archSyscallNumberRegister) bak atps regs
  case WI.asBV (CS.regValue symSyscallBV) of
    Nothing ->
      if getErrorSymbolicSyscalls errorSymbolicSyscalls
        then
          CB.addFailedAssertion bak $
            CS.AssertFailureSimError
              "Failed to execute syscall"
              "Cannot resolve a symbolic syscall number"
        else pure $ SkippedSyscall SymbolicSyscallNumber
    Just syscallBV ->
      let syscallNum = fromIntegral @Integer @Int $ BV.asUnsigned syscallBV
       in case IntMap.lookup syscallNum (arch ^. archSyscallCodeMapping) of
            Nothing ->
              pure $ SkippedSyscall $ UnknownSyscallNumber syscallNum
            Just syscallName ->
              let syscallNumRepr = Stubs.SyscallNumRepr atps rtps (toInteger syscallNum)
               in case MapF.lookup syscallNumRepr (st ^. stateSyscallHandles) of
                    Just syscallFnHandle ->
                      pure $ CachedSyscallFnHandle syscallFnHandle
                    Nothing ->
                      let syscallFnName = WFN.functionNameFromText syscallName
                       in case Map.lookup syscallFnName syscallOvs of
                            Just someSyscall ->
                              pure $ NewSyscall syscallName syscallNum someSyscall
                            Nothing ->
                              pure $ SkippedSyscall $ SyscallWithoutOverride syscallName syscallNum

-- | An implementation of 'Symbolic.LookupSyscallHandle' that attempts to look
-- up a syscall override and dispatches on the result.
lookupSyscallHandle ::
  ( CB.IsSymBackend sym bak
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  bak ->
  ArchContext arch ->
  -- | Map of names of overridden syscalls to their implementations
  Map.Map
    WFN.FunctionName
    (Stubs.SomeSyscall p sym (Symbolic.MacawExt arch)) ->
  ErrorSymbolicSyscalls ->
  -- | Dispatch on the result of looking up a syscall override.
  LookupSyscallDispatch p sym arch ->
  Symbolic.LookupSyscallHandle p sym arch
lookupSyscallHandle bak arch syscallOvs errorSymbolicSyscalls lsd =
  Symbolic.LookupSyscallHandle $ \atps rtps st regs -> do
    let LookupSyscallDispatch dispatch = lsd
    res <- lookupSyscallResult bak arch syscallOvs errorSymbolicSyscalls atps rtps st regs
    dispatch atps rtps st regs res

-- | Perform code discovery on the function address (see @Note [Incremental
-- code discovery]@ in "Grease.Macaw.SimulatorState") and bind the function
-- handle to its CFG.
discoverFuncAddr ::
  ( Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p cExt sym arch
  ) =>
  LJ.LogAction IO Diagnostic ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) WFN.FunctionName ->
  -- | The function address
  MC.ArchSegmentOff arch ->
  -- | The current Crucible state
  CS.SimState p sym (Symbolic.MacawExt arch) r f a ->
  IO
    ( MacawFnHandle arch
    , CS.SimState p sym (Symbolic.MacawExt arch) r f a
    )
discoverFuncAddr logAction halloc arch memory symMap pltStubs addr st0 = do
  C.Reg.SomeCFG regCFG <-
    discoverFunction logAction halloc arch memory symMap pltStubs addr
  C.SomeCFG funcCFG <- pure (C.toSSA regCFG)
  let cfgHdl = C.cfgHandle funcCFG
  let st1 = st0 & stateDiscoveredFnHandles %~ Map.insert addr cfgHdl
  pure $ useFnHandleAndState cfgHdl (CS.UseCFG funcCFG (C.postdomInfo funcCFG)) st1

-- | Bind the public function defined in a 'MacawSExpOverride' to its
-- 'MacawOverride', bind the auxiliary functions to their corresponding CFGs,
-- and redirect any forward declarations to their corresponding overrides.
useMacawSExpOverride ::
  ( CB.IsSymBackend sym bak
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , HasToConcretize p
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  (WFN.FunctionName -> IO ()) ->
  MacawSExpOverride p sym arch ->
  CS.SimState p sym (Symbolic.MacawExt arch) r f a ->
  IO (MacawFnHandle arch, CS.SimState p sym (Symbolic.MacawExt arch) r f a)
useMacawSExpOverride bak la halloc arch allOvs errCb mOv st0 =
  do
    MacawSExpOverride
      { msoPublicFnHandle = publicOvHdl
      , msoPublicOverride = publicOv
      , msoSomeFunctionOverride = Stubs.SomeFunctionOverride fnOv
      } <-
      pure mOv
    let CS.FnBindings fnHdlMap0 = st0 ^. CS.stateContext . CS.functionBindings
        fnOvName = Stubs.functionName fnOv
    doLog la $ Diag.FunctionOverride fnOvName
    fnHdlMap1 <- extendHandleMap bak allOvs errCb fnOv fnHdlMap0
    let st1 =
          st0
            & CS.stateContext . CS.functionBindings
              .~ CS.FnBindings fnHdlMap1
        funcName = WFN.functionNameFromText "_grease_fix_stack_ptr"
    useComposedOverride halloc arch publicOvHdl publicOv st1 funcName (arch ^. archOffsetStackPointerPostCall)

-- | Register the 'C.FnHandle's of any Macaw functions that this override may
-- invoke, be it through auxiliary functions or forward declarations. Note that
-- overrides for forward declarations may also have their own functions that
-- they transitively invoke, which means that this function must be recursive to
-- ensure that all functions that are reachable from the first are registered
-- beforehand.
extendHandleMap ::
  forall sym bak arch args ret solver scope st fs p.
  ( CB.IsSymBackend sym bak
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , CLM.HasPtrWidth (MC.ArchAddrWidth arch)
  , HasToConcretize p
  ) =>
  bak ->
  -- | Map of names of overridden functions to their implementations
  Map.Map WFN.FunctionName (MacawSExpOverride p sym arch) ->
  -- | What to do when a forward declaration cannot be resolved.
  (WFN.FunctionName -> IO ()) ->
  -- | The override that needs to be registered
  Stubs.FunctionOverride p sym args arch ret ->
  -- | The initial function handle map
  C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)) ->
  -- | The extended function handle map
  IO (C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)))
extendHandleMap bak allOvs errCb = go
 where
  -- Note the local quantification for @args'@ and @ret'@. Each recursive call
  -- to @go@ may be on a function of a different type.
  go ::
    forall args' ret'.
    Stubs.FunctionOverride p sym args' arch ret' ->
    C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)) ->
    IO (C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)))
  go fnOv fnHdlMap0 =
    let auxFns = Stubs.functionAuxiliaryFnBindings fnOv
        fwdDecs = Map.toList $ Stubs.functionForwardDeclarations fnOv
        fnHdlMap1 =
          foldl'
            ( \binds (CS.FnBinding fnHdl fnSt) ->
                C.insertHandleMap fnHdl fnSt binds
            )
            fnHdlMap0
            auxFns

        extendFwdDec ::
          C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)) ->
          (WFN.FunctionName, C.SomeHandle) ->
          IO (C.FnHandleMap (CS.FnState p sym (Symbolic.MacawExt arch)))
        extendFwdDec binds (fwdDecName, C.SomeHandle fwdDecHdl) =
          -- If the handle is already in the HandleMap, don't bother
          -- reprocessing it. This isn't just an optimization—it's important
          -- to ensure that this function terminates if it invokes a function
          -- that uses mutually recursive forward declarations.
          case C.lookupHandleMap fwdDecHdl binds of
            Just _ -> pure binds
            Nothing ->
              case Map.lookup fwdDecName allOvs of
                Nothing ->
                  case lookupMacawForwardDeclarationOverride bak allOvs fwdDecName fwdDecHdl of
                    Just ov -> pure (C.insertHandleMap fwdDecHdl (CS.UseOverride ov) binds)
                    Nothing -> do
                      errCb fwdDecName
                      pure binds
                Just
                  ( MacawSExpOverride
                      _
                      _
                      someForwardedOv@(Stubs.SomeFunctionOverride forwardedOv)
                    ) ->
                    let forwardedOvSim =
                          Stubs.mkForwardDeclarationOverride
                            bak
                            -- We don't use parent overrides, hence the []
                            (someForwardedOv NE.:| [])
                            fwdDecName
                            fwdDecHdl
                        binds' = C.insertHandleMap fwdDecHdl (CS.UseOverride forwardedOvSim) binds
                     in go forwardedOv binds'
     in foldM extendFwdDec fnHdlMap1 fwdDecs

-- | A helper function for binding a 'C.FnHandle' to its 'CS.FnState' when
-- returning in a 'Symbolic.LookupFunctionHandle'.
useFnHandleAndState ::
  C.FnHandle args ret ->
  CS.FnState p sym ext args ret ->
  CS.SimState p sym ext r f a ->
  (C.FnHandle args ret, CS.SimState p sym ext r f a)
useFnHandleAndState fnHdl fnState crucState =
  (fnHdl, Stubs.insertFunctionHandle crucState fnHdl fnState)
