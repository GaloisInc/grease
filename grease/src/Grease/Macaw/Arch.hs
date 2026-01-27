{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO(#162)
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- |
-- Copyright        : (c) Galois, Inc. 2024
-- Maintainer       : GREASE Maintainers <grease@galois.com>
module Grease.Macaw.Arch (
  ArchRegs,
  ArchRegCFG,
  ArchRegCFGMap,
  ArchCFG,
  ArchResult,
  ArchReloc,
  ArchContext (..),
  archGetIP,
  archInfo,
  archPcReg,
  archVals,
  archRelocSupported,
  archIntegerArguments,
  archIntegerReturnRegisters,
  archFunctionReturnAddr,
  archSyscallArgumentRegisters,
  archSyscallNumberRegister,
  archSyscallReturnRegisters,
  archSyscallCodeMapping,
  archStackPtrShape,
  archInitGlobals,
  archRegOverrides,
  archOffsetStackPointerPostCall,
  archABIParams,
  archPCFixup,
  defaultPCFixup,
) where

import Control.Lens.TH (makeLenses)
import Data.BitVector.Sized qualified as BV
import Data.Data (Proxy)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Macaw.Architecture.Info qualified as MI
import Data.Macaw.CFG qualified as MC
import Data.Macaw.Memory qualified as Symbolic
import Data.Macaw.Symbolic qualified as Symbolic
import Data.Macaw.Symbolic.Memory as SymbolicMemory
import Data.Macaw.Types (BVType)
import Data.Map (Map)
import Data.Parameterized.Context qualified as Ctx
import Data.Text (Text)
import Grease.Macaw.Load.Relocation (RelocType)
import Grease.Macaw.RegName (RegName)
import Grease.Shape.NoTag (NoTag)
import Grease.Shape.Pointer (PtrShape)
import Lang.Crucible.Backend qualified as CB
import Lang.Crucible.Backend.Online qualified as C
import Lang.Crucible.CFG.Core qualified as C
import Lang.Crucible.CFG.Reg qualified as C.Reg
import Lang.Crucible.LLVM.MemModel qualified as CLM
import Lang.Crucible.Simulator qualified as CS
import Stubs.Common qualified as Stubs
import Stubs.FunctionOverride qualified as Stubs
import What4.Expr qualified as WE
import What4.Interface qualified as WI
import What4.Protocol.Online qualified as WPO

type ArchRegs sym arch = Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch)

type ArchRegCFG arch =
  C.Reg.SomeCFG
    (Symbolic.MacawExt arch)
    (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
    (Symbolic.ArchRegStruct arch)

type ArchRegCFGMap arch = Map (Symbolic.MemSegmentOff (MC.ArchAddrWidth arch)) (ArchRegCFG arch)

type ArchCFG arch =
  C.SomeCFG
    (Symbolic.MacawExt arch)
    (Ctx.EmptyCtx Ctx.::> Symbolic.ArchRegStruct arch)
    (Symbolic.ArchRegStruct arch)

type ArchResult p sym arch =
  CS.ExecResult
    p
    sym
    (Symbolic.MacawExt arch)
    (CS.RegEntry sym (Symbolic.ArchRegStruct arch))

-- | The ELF relocation data type for a specific architecture.
type family ArchReloc arch :: Type

-- | Architecture-specific information and operations. This is similar to the
-- 'Stubs.FunctionABI' data type from @stubs-common@, but it is different enough
-- to warrant being a separate data type.
data ArchContext arch = ArchContext
  { _archInfo :: MI.ArchitectureInfo arch
  , _archGetIP ::
      forall sym.
      CB.IsSymInterface sym =>
      ArchRegs sym arch ->
      IO (WI.SymExpr sym (WI.BaseBVType (MC.ArchAddrWidth arch)))
  , _archPcReg :: MC.ArchReg arch (BVType (MC.ArchAddrWidth arch))
  , _archVals :: Symbolic.GenArchVals Symbolic.LLVMMemory arch
  , -- Check if @grease@ supports a particular relocation type. This should
    -- return 'Nothing' if it is unsupported and 'Just' if it is supported.
    _archRelocSupported :: ArchReloc arch -> Maybe RelocType
  , -- Given a full register state, extract all of the arguments we need for the
    -- function call.
    _archIntegerArguments ::
      forall sym bak atps.
      ( CB.IsSymBackend sym bak
      , CLM.HasLLVMAnn sym
      ) =>
      bak ->
      C.CtxRepr atps ->
      {- Types of arguments -}
      Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
      {- Argument register values -}
      CLM.MemImpl sym ->
      {- The memory state at the time of the function call -}
      IO (Ctx.Assignment (CS.RegEntry sym) atps, Stubs.GetVarArg sym)
  , {- A pair containing the function argument values and a callback for
    retrieving variadic arguments. -}
    -- Build an OverrideSim action with appropriate return register types.
    _archIntegerReturnRegisters ::
      forall sym bak p t r args rtp mem.
      CB.IsSymBackend sym bak =>
      bak ->
      Symbolic.GenArchVals mem arch ->
      {- Architecture-specific information -}
      C.TypeRepr t ->
      {- Function return type -}
      Stubs.OverrideResult sym arch t ->
      {- Function's return value -}
      CS.RegValue sym (Symbolic.ArchRegStruct arch) ->
      {- Argument register values from before function execution -}
      CS.OverrideSim p sym (Symbolic.MacawExt arch) r args rtp (CS.RegValue sym (Symbolic.ArchRegStruct arch))
  , {- OverrideSim action with return type matching system return register type -}
    -- If the return address for the function being called can be determined,
    -- then return Just that address. Otherwise, return Nothing. Some ABIs
    -- store this information directly in a register, while other ABIs store
    -- this information on the stack, so we provide both registers and the stack
    -- as arguments.
    _archFunctionReturnAddr ::
      forall sym bak solver scope st fs mem.
      ( CB.IsSymBackend sym bak
      , sym ~ WE.ExprBuilder scope st fs
      , bak ~ C.OnlineBackend solver scope st fs
      , WPO.OnlineSolver solver
      , CLM.HasLLVMAnn sym
      ) =>
      bak ->
      Symbolic.GenArchVals mem arch ->
      Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
      {- Registers for the given architecture -}
      CLM.MemImpl sym ->
      {- The memory state at the time of the function call -}
      IO (Maybe (MC.MemWord (MC.ArchAddrWidth arch)))
  , -- Given a full register state, extract all of the arguments we need for
    -- the system call.
    _archSyscallArgumentRegisters ::
      forall sym bak args atps.
      CB.IsSymBackend sym bak =>
      bak ->
      C.CtxRepr atps ->
      {- Types of argument registers -}
      CS.RegEntry sym (C.StructType atps) ->
      {- Argument register values -}
      C.CtxRepr args ->
      {- Types of syscall arguments -}
      IO (Ctx.Assignment (CS.RegEntry sym) args)
  , {- Syscall argument values -}
    -- Extract the syscall number from the register state.
    _archSyscallNumberRegister ::
      forall sym bak atps.
      CB.IsSymBackend sym bak =>
      bak ->
      Ctx.Assignment C.TypeRepr atps ->
      {- Types of argument registers -}
      CS.RegEntry sym (C.StructType atps) ->
      {- Argument register values -}
      IO (CS.RegEntry sym (C.BVType (MC.ArchAddrWidth arch)))
  , {- Extracted syscall number -}
    -- Build an OverrideSim action with appropriate return register types from
    -- a given OverrideSim action.
    _archSyscallReturnRegisters ::
      forall sym p t ext r args rtps atps.
      C.TypeRepr t ->
      {- Syscall return type -}
      CS.OverrideSim p sym ext r args (C.StructType rtps) (CS.RegValue sym t) ->
      {- OverrideSim action producing the syscall's return value -}
      C.CtxRepr atps ->
      {- Argument register types -}
      CS.RegEntry sym (C.StructType atps) ->
      {- Argument register values from before syscall execution -}
      C.CtxRepr rtps ->
      {- Return register types -}
      CS.OverrideSim p sym ext r args (C.StructType rtps) (CS.RegValue sym (C.StructType rtps))
  , {- OverrideSim action with return type matching system return register
       type -}
    -- A mapping from syscall numbers to names.
    _archSyscallCodeMapping :: IntMap Text
  , _archStackPtrShape :: PtrShape (Symbolic.MacawExt arch) (MC.ArchAddrWidth arch) NoTag (CLM.LLVMPointerType (MC.ArchAddrWidth arch))
  -- ^ Shape of the stack pointer
  --
  -- A function that writes to the stack on architectures where it grows down
  -- (including armv7l, PowerPC, and x86_64) will generally subtract an offset
  -- from the stack pointer to do so. If we treated the stack pointer like any
  -- other register, the resulting behavior when encountering a write would be
  -- that the heuristics would make it point to the base of a fresh
  -- allocation. When the function subtracts an offset to write to it, it
  -- would wrap around, making the offset huge and resulting in
  -- non-termination (see gitlab#53).
  --
  -- Instead, we point the stack pointer to the end of a large, fresh memory
  -- allocation. On x86_64, we additionally initialize a pointer-sized chunk at
  -- the end of the stack, as x86_64 reads the return address from the stack.
  , -- Initialize architecture-specific global variables before simulation
    -- begins. Currently, this is only used to initialize thread-local state.
    -- See Note [Coping with stack protection] for how this is used.
    _archInitGlobals ::
      forall sym.
      ( CLM.HasLLVMAnn sym
      , SymbolicMemory.MacawProcessAssertion sym
      ) =>
      Stubs.Sym sym ->
      CLM.MemImpl sym ->
      CS.SymGlobalState sym ->
      IO (CLM.MemImpl sym, CS.SymGlobalState sym)
  , _archRegOverrides :: Map RegName (BV.BV (MC.ArchAddrWidth arch))
  -- ^ When setting up the initial register values just before starting
  -- simulation, override the default values for the following registers and
  -- replace them with the 'BV.BV' values corresponding to each register's
  -- 'RegName'. Currently, this is only used to ensure that the initial value
  -- in certain architectures' link register (which stores the return address)
  -- is a value within the @.text@ section, which helps satisfy @grease@'s
  -- @in-text@ requirement.
  , _archOffsetStackPointerPostCall ::
      forall sym p ext rtp a r.
      CB.IsSymInterface sym =>
      ArchRegs sym arch ->
      CS.OverrideSim p sym ext rtp a r (ArchRegs sym arch)
  -- ^ On certain architectures, invoking a function will push the return
  -- address onto the stack (e.g., x86-64's @call@ instruction). This
  -- generally comes with the expectation that the invoked function will pop
  -- the return address and increment the stack pointer accordingly (e.g.,
  -- x86-64's @ret@ instruction). When skipping a function or using an
  -- override, however, no instruction will pop the return address (and
  -- increment the stack pointer accordingly), so
  -- '_archOffsetStackPointerPostCall' simulates that effect.
  --
  -- Currently, x86-64 is the only supported architecture that does something
  -- non-trivial here. All other architectures can simply leave the stack
  -- pointer by implementing @'_archOffsetStackPointerPostCall' = 'pure'@.
  , _archABIParams :: [C.Some (MC.ArchReg arch)]
  -- ^ The ordered integer and pointer ABI registers for the architecture. I
  -- TODO(#260): In the future this should handle things like stack params.
  -- TODO(#261): In the future this should handle multiple calling conventions.
  , _archPCFixup ::
      forall sym bak solver scope st fs.
      ( CB.IsSymInterface sym
      , sym ~ WE.ExprBuilder scope st fs
      , WPO.OnlineSolver solver
      , bak ~ C.OnlineBackend solver scope st fs
      ) =>
      bak ->
      Ctx.Assignment (CS.RegValue' sym) (Symbolic.MacawCrucibleRegTypes arch) ->
      MC.ArchSegmentOff arch ->
      IO (MC.ArchSegmentOff arch)
  -- ^ A function that fixes up the address of a function (e.g. for thumb mode based on the current register context)
  }

makeLenses ''ArchContext

-- | A default PC fixup function that is a noop.
-- Requires a proxy of the arch to determine the returned address size
defaultPCFixup ::
  Proxy arch ->
  bak ->
  regs ->
  MC.ArchSegmentOff arch ->
  IO (MC.ArchSegmentOff arch)
defaultPCFixup _ _ _ addr = pure addr

{-
Note [Coping with stack protection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Many C compilers enable -fstack-protector by default, which causes the
generated assembly for certain functions to check for buffer overflows. This
Note describes how grease copes with -fstack-protector–compiled binaries on
each of the currently supported architectures.

-----
-- x86-64 (full support)
-----

x86-64 binaries compiled with -fstack-protector will generate extra code for
protected functions such that:

\* When the function is first invoked, it will load a piece of thread-local
  state via the %fs segment register.

\* Just before the function returns, it will once again load the same
  thread-local state from %fs.

\* It will then check to see if the two thread-local state values are the same.
  If they are equal, then return from the function normally. If they are not
  equal, then this is evidence that a buffer overflow has occurred, so call the
  __stack_chk_fail function (which halts execution).

The tricky part is dealing with %fs. Unlike other x86-64 registers, segment
registers (%fs and %gs) are not modeled as part of the giant register struct
that is passed around during simulation. Instead, reading from segment
registers is handled via the ReadFSBase and ReadGSBase primitive functions in
macaw-x86. By default, macaw-x86-symbolic will crash if it encounters one of
these primitive functions, so it is up to grease to give these primitive
functions meaningful semantics.

One possible approach would be to reverse-engineer the exact shape of the
thread-local state that is accessed in stack-protected functions. This is
doable but tricky to get right, and especially so when you consider that some
of this thread-local state may be compiler-specific.

Instead, we adopt a much simpler approach: initialize all of %fs with a
symbolic array. It doesn't particularly matter /what/ value is stored in %fs so
long as it is equal to itself, and a symbolic array meets that criterion. In
fact, this is exactly the same approach that stubs uses, so we are able to
piggyback on stubs' implementation. (See the relevant comments in
Grease.Macaw.Arch.X86.x86Ctx.)

-----
-- AArch32 (partial support)
-----

The story for AArch32 is similar to that of x86-64, except that instead of
loading a segment register, AArch32 binaries compiled with -fstack-protector
will load the contents of a global variable named __stack_chk_guard. The
details of how __stack_chk_guard materializes at the binary level depend on
whether the binary is statically or dynamically linked.

If the binary is statically linked, then __stack_chk_guard is defined as:

00020450 <__stack_chk_guard>:
   20450:       00000000        andeq   r0, r0, r0

And sure enough, 00000000 equals 00000000, so grease has no issues at all with
this. (In reality, the binary's startup code would modify the value of
__stack_chk_guard before calling the main() function, but we don't model the
startup code. Nor do we particularly need to for this example to work out.)

Unfortunately, life is harder when dealing with dynamically linked binaries, as
__stack_chk_guard is referenced indirectly via an R_ARM_COPY or R_ARM_GLOB_DAT
relocation. grease does not yet model the semantics of these relocation types
in full fidelity (see #22). Instead, it will populate the part of the binary
where __stack_chk_guard's relocation resides with symbolic bytes. grease will
then attempt to interpret these symbolic bytes as a pointer and read from it,
but this will fail.

If we wanted to support stack protection in dynamically linked AArch32
binaries, we could consider special-casing the code that populates
__stack_chk_guard's relocation region so that it populates it with a dummy
pointer value that points to something valid.

-----
-- PowerPC (no support)
-----

grease currently does not support stack protection on PowerPC at all.

More specifically, PowerPC binaries compiled with -fstack-protector will
attempt to access a piece of thread-local state via r2 (the table of contents
register) at an extremely large offset (e.g., -28680). This does not play well
with grease's heuristics, which will attempt to allocate space for r2 one byte
at a time, causing the refinement loop to time out well before it ever
allocates enough space to cover the large offset. If we want to support this,
we will likely need to give r2 a pre-determined PtrShape that allocates this
space ahead of time, similarly to how we deal with the stack pointer register.

-----

Regardless of which architecture is used, the stack protection code will call
out to the __stack_chk_fail function in the event of a buffer overflow.
Usually, the call to __stack_chk_fail is guarded behind a symbolic equality
check, so grease will usually simulate the definition of __stack_chk_fail (even
if the SMT solver ultimately concludes that the path condition leading up to
calling __stack_chk_fail is unsatisfiable). Unfortunately, the definition of
__stack_chk_fail is usually too gnarly for Macaw to handle. For instance, the
x86-64 version of __stack_chk_fail invokes the `hlt` instruction, which
macaw-x86-symbolic does not have semantics for.

As such, we install an override for __stack_chk_fail that simply calls the
abort() function, which grease can handle without issue. Actually, to be more
precise, we install this as an override for both __stack_chk_fail and its
cousin, the __stack_chk_fail_local function. There are two reasons for this:

\* On PowerPC, __stack_chk_fail_local is an entirely separate function that is
  defined in terms of __stack_chk_fail. We could try to simulate the definition
  of __stack_chk_fail_local, but there's not much reason to.

\* On x86-64 and AArch32, __stack_chk_fail_local is a weak alias for the
  __stack_chk_fail function, and both functions are defined at the same
  address. Due to a quirk in how macaw-loader works, however (see
  https://github.com/GaloisInc/macaw-loader/issues/25), the override will apply
  to __stack_chk_fail_local, /not/ __stack_chk_fail. As such, it is convenient
  to override both functions just in case.
-}
