{-|
Copyright        : (c) Galois, Inc. 2024
Maintainer       : GREASE Maintainers <grease@galois.com>
-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Grease.Macaw.ResolveCall
  ( lookupFunctionHandle
  , lookupSyscallHandle
  ) where

import Prelude (Integer, fromIntegral, otherwise, toInteger)

import Control.Applicative (pure)
import Control.Lens ((^.), (%~), (.~), to)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (foldl')
import Data.Function (($), (&), (.))
import Data.Int (Int)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe(..))
import Data.Type.Equality (type (~))
import GHC.Word (Word64)
import qualified Lumberjack as LJ
import System.IO (IO)

import qualified Data.BitVector.Sized as BV

-- parameterized-utils
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF
import Data.Parameterized.NatRepr (knownNat)

-- what4
import qualified What4.Expr as W4
import qualified What4.FunctionName as W4
import qualified What4.Interface as W4
import qualified What4.Protocol.Online as W4

-- crucible
import qualified Lang.Crucible.Analysis.Postdom as C
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Backend.Online as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.SSAConversion as C
import qualified Lang.Crucible.CFG.Reg as C.Reg
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.MemModel as Mem

-- macaw-base
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as Discovery
import qualified Data.Macaw.Discovery.Classifier as Discovery
import qualified Data.Macaw.Memory.ElfLoader as EL

-- macaw-loader
import qualified Data.Macaw.BinaryLoader.ELF as Loader

-- macaw-symbolic
import qualified Data.Macaw.Symbolic as Symbolic
import qualified Data.Macaw.Symbolic.Concretize as Symbolic

-- stubs
import qualified Stubs.FunctionOverride as Stubs
import qualified Stubs.FunctionOverride.ForwardDeclarations as Stubs
import qualified Stubs.Memory.Common as Stubs
import qualified Stubs.Syscall as Stubs

import Grease.Diagnostic (Diagnostic(..), GreaseLogAction)
import Grease.Macaw.Arch
import Grease.Macaw.Discovery (discoverFunction)
import Grease.Macaw.FunctionOverride
import qualified Grease.Macaw.ResolveCall.Diagnostic as Diag
import Grease.Macaw.SkippedCall (SkippedCall(..))
import Grease.Macaw.SimulatorState
import Grease.Macaw.Syscall
import Grease.Options (ErrorSymbolicFunCalls(..))
import Grease.Utility (declaredFunNotFound)

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
  C.SimState p sym (Symbolic.MacawExt arch) r f a ->
  -- | Name for new override
  W4.FunctionName ->
  -- | New action to take
  (forall r'. C.RegValue sym (Symbolic.ArchRegStruct arch) ->
    C.OverrideSim p sym (Symbolic.MacawExt arch) r' (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch) (C.RegValue sym (Symbolic.ArchRegStruct arch))) ->
  IO ( C.FnHandle (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch) (Symbolic.ArchRegStruct arch)
     , C.SimState p sym (Symbolic.MacawExt arch) r f a
     )
useComposedOverride halloc arch handle0 override0 st funcName f = do
  handle <- C.mkHandle' halloc funcName (Ctx.Empty Ctx.:> regStructRepr arch) (regStructRepr arch)
  let override = C.mkOverride' funcName (regStructRepr arch) $ do
        args <- C.getOverrideArgs
        regs <- C.callOverride handle0 override0 args
        f (C.regValue regs)
  pure $ useFnHandleAndState handle (C.UseOverride override) st

lookupFunctionHandle ::
  forall arch sym bak solver scope st fs p.
  ( C.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , Mem.HasLLVMAnn sym
  , HasGreaseSimulatorState p sym arch
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) W4.FunctionName ->
  -- | Map of dynamic function names to their addresses
  Map.Map W4.FunctionName (MC.ArchSegmentOff arch) ->
  -- | Map of names of overridden functions to their implementations
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
  ErrorSymbolicFunCalls ->
  Symbolic.LookupFunctionHandle p sym arch
lookupFunctionHandle bak la halloc arch memory symMap pltStubs dynFunMap funOvs errorSymbolicFunCalls = Symbolic.LookupFunctionHandle $ \st mem regs -> do
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

  let -- Treat an external function as a no-op during simulation.
      skipExternalCall reason = do
        doLog la $ Diag.SkippedCall reason
        let funcName = W4.functionNameFromText "_grease_external"
        handle <- C.mkHandle' halloc funcName (Ctx.Empty Ctx.:> regStructRepr arch) (regStructRepr arch)
        let override = C.mkOverride' funcName (regStructRepr arch) $ do
              args <- C.getOverrideArgs
              let regs' = Ctx.last $ C.regMap args
              (arch ^. archOffsetStackPointerPostCall) (C.regValue regs')
        pure $ useFnHandleAndState handle (C.UseOverride override) st

  case W4.asBV symAddr1 of
    -- If asBV returns Nothing here, despite the call to resolveSymBV above,
    -- then the address is truly symbolic. By default, we skip the call
    -- entirely, but if the user passes --error-symbolic-fun-calls, then this is
    -- treated as an error.
    Nothing ->
      if getErrorSymbolicFunCalls errorSymbolicFunCalls
        then C.addFailedAssertion bak $
             C.AssertFailureSimError
               "Failed to call function"
               "Cannot resolve a symbolic function address"
        else skipExternalCall SymbolicAddress
    Just bv ->
      let -- This conversion is safe iff MC.ArchAddrWidth arch <= 64
          bvWord64 = fromIntegral @Integer @Word64 (BV.asUnsigned bv)

          -- Call a function handle, unless we have an override in which case call
          -- the override instead.
          callHandle hdl st' =
            case Map.lookup (C.handleName hdl) funOvs of
              Just macawFnOv ->
                useMacawFunctionOverride bak la halloc arch funOvs macawFnOv st'
              Nothing ->
                pure (hdl, st')

          -- Log that we have performed a function call.
          logFunctionCall fnName = do
            mbReturnAddr <-
              (arch ^. archFunctionReturnAddr) bak (arch ^. archVals) regs mem
            doLog la $ Diag.FunctionCall fnName bvMemWord mbReturnAddr

          hdls = st ^. stateDiscoveredFnHandles
          bvMemWord = EL.memWord bvWord64

          dispatchFuncAddrOff funcAddrOff
            -- First, check if this is the address of a CFG we have already
            -- discovered
            | Just hdl <- Map.lookup funcAddrOff hdls = do
              logFunctionCall (C.handleName hdl)
              callHandle hdl st

            -- Next, check if this is a PLT stub.
            | Just pltStubName <- Map.lookup funcAddrOff pltStubs = do
              if |  -- If a PLT stub jumps to an address within the same binary
                    -- or shared library, resolve it...
                    Just pltCallAddr <- Map.lookup pltStubName dynFunMap
                 -> do doLog la $ Diag.PltCall pltStubName funcAddrOff pltCallAddr
                       dispatchFuncAddrOff pltCallAddr
                 |  otherwise
                 -> case Map.lookup pltStubName funOvs of
                      -- ...otherwise, if there is an override for the PLT stub,
                      -- use it...
                      Just macawFnOv -> do
                        logFunctionCall pltStubName
                        useMacawFunctionOverride bak la halloc arch funOvs macawFnOv st
                      -- ...otherwise, skip the PLT call entirely.
                      Nothing -> do
                        logFunctionCall pltStubName
                        skipExternalCall (PltNoOverride @arch funcAddrOff pltStubName)

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
              (hdl, st') <-
                discoverFuncAddr la halloc arch memory symMap pltStubs funcAddrOff st
              logFunctionCall (C.handleName hdl)
              callHandle hdl st'

            | otherwise =
              skipExternalCall (NotExecutable funcAddrOff)

      in case Loader.resolveAbsoluteAddress memory bvMemWord of
        Nothing -> skipExternalCall (InvalidAddress (BV.ppHex knownNat bv))
        Just funcAddrOff -> dispatchFuncAddrOff funcAddrOff

-- | An implementation of 'Symbolic.LookupSyscallHandle' that attempts to look
-- up an override for the syscall, and if one is found, invokes it. If one
-- cannot be found, the syscall is simulated as a no-op.
lookupSyscallHandle ::
  forall arch sym bak solver scope st fs p.
  ( C.IsSymBackend sym bak
  , Symbolic.SymArchConstraints arch
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  , HasGreaseSimulatorState p sym arch
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  -- | Map of names of overridden syscalls to their implementations
  Map.Map
    W4.FunctionName
    (Stubs.SomeSyscall p sym (Symbolic.MacawExt arch)) ->
  Symbolic.LookupSyscallHandle p sym arch
lookupSyscallHandle bak la halloc arch syscallOvs = Symbolic.LookupSyscallHandle $ \atps rtps st regs -> do
  symSyscallBV <- (arch ^. archSyscallNumberRegister) bak atps regs

  let -- Treat this syscall as a no-op during simulation.
      skipCall reason = do
        doLog la $ Diag.SkippedCall reason
        let funcName = W4.functionNameFromText "_grease_syscall"
        handle <- C.mkHandle' halloc funcName atps (C.StructRepr rtps)
        let override =
              C.mkOverride'
                funcName
                (C.StructRepr rtps)
                ((arch ^. archSyscallReturnRegisters)
                   C.UnitRepr (pure ()) atps regs rtps)
        pure $ useFnHandleAndState handle (C.UseOverride override) st

  case W4.asBV (C.regValue symSyscallBV) of
    Nothing ->
      skipCall SymbolicSyscallNumber
    Just syscallBV ->
      let syscallNum = fromIntegral @Integer @Int $ BV.asUnsigned syscallBV in
      case IntMap.lookup syscallNum (arch ^. archSyscallCodeMapping) of
        Nothing ->
          skipCall $ UnknownSyscallNumber syscallNum
        Just syscallName ->
          let syscallNumRepr = Stubs.SyscallNumRepr atps rtps (toInteger syscallNum) in
          case MapF.lookup syscallNumRepr (st ^. stateSyscallHandles) of
            Just (Stubs.SyscallFnHandle syscallHdl) ->
              pure (syscallHdl, st)
            Nothing ->
              let syscallFnName = W4.functionNameFromText syscallName in
              case Map.lookup syscallFnName syscallOvs of
                Just (Stubs.SomeSyscall syscallOv) -> do
                  doLog la $ Diag.SyscallOverride syscallName syscallNum
                  macawSyscallHdl <- C.mkHandle' halloc syscallFnName atps (C.StructRepr rtps)
                  let macawSyscallOv = macawSyscallOverride bak arch atps rtps syscallOv
                  pure $ useFnHandleAndState macawSyscallHdl (C.UseOverride macawSyscallOv) st
                Nothing ->
                  skipCall $ SyscallWithoutOverride syscallName syscallNum

-- | Perform code discovery on the function address (see @Note [Incremental
-- code discovery]@ in "Grease.Macaw.SimulatorState") and bind the function
-- handle to its CFG.
discoverFuncAddr ::
  ( Symbolic.SymArchConstraints arch
  , HasGreaseSimulatorState p sym arch
  ) =>
  LJ.LogAction IO Diagnostic ->
  C.HandleAllocator ->
  ArchContext arch ->
  EL.Memory (MC.ArchAddrWidth arch) ->
  -- | Map of entrypoint addresses to their names
  Discovery.AddrSymMap (MC.ArchAddrWidth arch) ->
  -- | Map of addresses to PLT stub names
  Map.Map (MC.ArchSegmentOff arch) W4.FunctionName ->
  -- | The function address
  MC.ArchSegmentOff arch ->
  -- | The current Crucible state
  C.SimState p sym (Symbolic.MacawExt arch) r f a ->
  IO ( MacawFnHandle arch
     , C.SimState p sym (Symbolic.MacawExt arch) r f a
     )
discoverFuncAddr logAction halloc arch memory symMap pltStubs addr st0 = do
  C.Reg.SomeCFG regCFG <-
    discoverFunction logAction halloc arch memory symMap pltStubs addr
  C.SomeCFG funcCFG <- pure (C.toSSA regCFG)
  let cfgHdl = C.cfgHandle funcCFG
  let st1 = st0 & stateDiscoveredFnHandles %~ Map.insert addr cfgHdl
  pure $ useFnHandleAndState cfgHdl (C.UseCFG funcCFG (C.postdomInfo funcCFG)) st1

-- | Bind the public function defined in a 'MacawFunctionOverride' to its
-- 'MacawOverride', bind the auxiliary functions to their corresponding CFGs,
-- and redirect any forward declarations to their corresponding overrides.
useMacawFunctionOverride ::
  ( C.IsSymBackend sym bak
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  GreaseLogAction ->
  C.HandleAllocator ->
  ArchContext arch ->
  -- | Map of names of overridden functions to their implementations
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
  MacawFunctionOverride p sym arch ->
  C.SimState p sym (Symbolic.MacawExt arch) r f a ->
  IO (MacawFnHandle arch, C.SimState p sym (Symbolic.MacawExt arch) r f a)
useMacawFunctionOverride bak la halloc arch allOvs
      (MacawFunctionOverride publicOvHdl publicOv (Stubs.SomeFunctionOverride fnOv)) st0 =
 do let C.FnBindings fnHdlMap0 = st0 ^. C.stateContext . C.functionBindings
        fnOvName = Stubs.functionName fnOv
    doLog la $ Diag.FunctionOverride fnOvName
    fnHdlMap1 <- extendHandleMap bak allOvs fnOv fnHdlMap0
    let st1 = st0 & C.stateContext . C.functionBindings
                 .~ C.FnBindings fnHdlMap1
        funcName = W4.functionNameFromText "_grease_fix_stack_ptr"
    useComposedOverride halloc arch publicOvHdl publicOv st1 funcName (arch ^. archOffsetStackPointerPostCall)

-- | Register the 'C.FnHandle's of any Macaw functions that this override may
-- invoke, be it through auxiliary functions or forward declarations. Note that
-- overrides for forward declarations may also have their own functions that
-- they transitively invoke, which means that this function must be recursive to
-- ensure that all functions that are reachable from the first are registered
-- beforehand.
extendHandleMap ::
  forall sym bak arch args ret solver scope st fs p.
  ( C.IsSymBackend sym bak
  , W4.OnlineSolver solver
  , sym ~ W4.ExprBuilder scope st fs
  , bak ~ C.OnlineBackend solver scope st fs
  ) =>
  bak ->
  -- | Map of names of overridden functions to their implementations
  Map.Map W4.FunctionName (MacawFunctionOverride p sym arch) ->
  -- | The override that needs to be registered
  Stubs.FunctionOverride p sym args arch ret ->
  -- | The initial function handle map
  C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)) ->
  -- | The extended function handle map
  IO (C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)))
extendHandleMap bak allOvs = go
  where
    -- Note the local quantification for @args'@ and @ret'@. Each recursive call
    -- to @go@ may be on a function of a different type.
    go ::
      forall args' ret'.
      Stubs.FunctionOverride p sym args' arch ret' ->
      C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)) ->
      IO (C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)))
    go fnOv fnHdlMap0 =
      let auxFns = Stubs.functionAuxiliaryFnBindings fnOv
          fwdDecs = Map.toList $ Stubs.functionForwardDeclarations fnOv
          fnHdlMap1 =
            foldl'
              (\binds (C.FnBinding fnHdl fnSt) ->
                C.insertHandleMap fnHdl fnSt binds)
              fnHdlMap0 auxFns

          extendFwdDec ::
            C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)) ->
            (W4.FunctionName, C.SomeHandle) ->
            IO (C.FnHandleMap (C.FnState p sym (Symbolic.MacawExt arch)))
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
                    declaredFunNotFound fwdDecName
                  Just (MacawFunctionOverride _ _
                         someForwardedOv@(Stubs.SomeFunctionOverride forwardedOv)) ->
                    let forwardedOvSim =
                          Stubs.mkForwardDeclarationOverride
                            bak
                            -- We don't use parent overrides, hence the []
                            (someForwardedOv NE.:| [])
                            fwdDecName fwdDecHdl
                        binds' = C.insertHandleMap fwdDecHdl (C.UseOverride forwardedOvSim) binds in
                    go forwardedOv binds' in

      foldM extendFwdDec fnHdlMap1 fwdDecs

-- | A helper function for binding a 'C.FnHandle' to its 'C.FnState' when
-- returning in a 'Symbolic.LookupFunctionHandle'.
useFnHandleAndState ::
  C.FnHandle args ret ->
  C.FnState p sym ext args ret ->
  C.SimState p sym ext r f a ->
  (C.FnHandle args ret, C.SimState p sym ext r f a)
useFnHandleAndState fnHdl fnState crucState =
  (fnHdl, Stubs.insertFunctionHandle crucState fnHdl fnState)
