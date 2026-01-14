# Copyright Galois, Inc. 2024

ARMCC   = armv7l-linux-musleabi-gcc
X64CC   = clang-14
# This binary was obtained from https://musl.cc/
PPC32CC = powerpc-linux-musl-gcc
# A reasonable default set of CC flags that aims to make binaries as small as
# possible. It is possible that this default will not work for all test cases,
# however. If you want to add a test case that changes one or more of these
# flags, consider splitting it out into its own variable.
#
# Explanations for additional flags:
# -static: Generally, we prefer self-contained test binaries.
# -no-pie: Position-independent executables are typically much larger.
CFLAGS = $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) $(CFLAGS_STATIC)
# The set of CC flags that we use across all test cases.
# -fno-stack-protector: We want to check for this kind of error in GREASE
# -Wl,--unresolved-symbols=ignore-all: No need to add main() to tests
# -nostartfiles: Make binaries smaller by not including the startup-related
#   code that GCC links in by default. The trade-off is that we have to define
#   our own, stub version of a `void _start(void)` function in each test case,
#   but this is a small price to pay for the amount of code that we get rid of.
CFLAGS_COMMON = -fno-stack-protector -Wl,--unresolved-symbols=ignore-all -nostartfiles
# A set of CC flags that makes binaries smaller by not linking against any
# standard C libraries. This is reasonable for small test cases, but it is not
# suitable for test cases that rely on functions from external libraries (e.g.,
# libc).
CFLAGS_NO_LIBS = -nodefaultlibs -nolibc -nostdlib
# A set of CC flags that makes binaries more self-contained:
# -static: Generally, we prefer self-contained test binaries.
# -no-pie: Position-independent executables are typically much larger.
CFLAGS_STATIC = -static -no-pie

DIRS = \
  $(shell find feat/ -mindepth 1 -maxdepth 1 -type d) \
  $(shell find prop/ -mindepth 3 -maxdepth 3 -type d) \
  $(shell feat/ refine/ sanity/ -mindepth 2 -maxdepth 2 -type d)

# These test cases must dynamically link against libc, so we avoid passing
# CFLAGS_NO_LIBS when compiling them. Sadly, clang will not compile something with
# -nostartfiles unless CFLAGS_NO_LIBS is also given, so we cannot provide
# x86-64 versions of these test cases just yet.
DYNAMIC_DIRS = \
  prop/no-mprotect/pos/mprotect \
  refine/bug/assert_false \
  refine/bug/networking \
  refine/pos/excluded_overrides \
  refine/pos/malloc \
  refine/pos/malloc-free-external \
  refine/pos/printf \
  sanity/pass/startup-override
DYNAMIC_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(DYNAMIC_DIRS))
DYNAMIC_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(DYNAMIC_DIRS))

# This test case requires the use of position-independent executables.
DYNAMIC_PIE_DIRS = refine/bug/null_ptr_deref_pie refine/pos/pie
DYNAMIC_PIE_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(DYNAMIC_PIE_DIRS))
DYNAMIC_PIE_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(DYNAMIC_PIE_DIRS))

# This test case requires the use of stripped binaries.
STRIPPED_STATIC_DIRS = refine/pos/stripped refine/pos/stripped_with_overrides
STRIPPED_STATIC_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(STRIPPED_STATIC_DIRS))
STRIPPED_STATIC_X64_EXES   = $(addsuffix /test.x64.elf,$(STRIPPED_STATIC_DIRS))
STRIPPED_STATIC_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(STRIPPED_STATIC_DIRS))

# These test cases depend on libc, but rely on static linking to ensure that
# all of the relevant code from libc is inlined into the binary.
#
# For instance, the sanity/pass/syscall test case crucially relies
# on invoking a system call instruction via the implementation of libc's `alarm`
# function. If we dynamically linked against libc, the call to `alarm` would turn
# into an invocation of a PLT stub, which tests a different code path.
STATIC_WITH_LIBS_DIRS = refine/neg/libpng-cve-2018-13785 refine/pos/getppid_syscall_override sanity/pass/syscall
STATIC_WITH_LIBS_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(STATIC_WITH_LIBS_DIRS))
STATIC_WITH_LIBS_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(STATIC_WITH_LIBS_DIRS))

# This test case requires the use of stack protection. This implies the need for
# libc (since that is where __stack_chk_fail and related functions come from)
# and static linking (as grease does not yet support stack protection in
# dynamically linked AArch32 binaries). Moreover, grease does not yet support
# stack protection in PowerPC binaries.
#
# Sadly, clang will balk if you attempt to combine -fstack-protector-all with
# -nostartfiles, so this test case must be compiled with startup code bloat.
STACK_PROTECTOR_DIRS = refine/pos/stack_protector
STACK_PROTECTOR_ARM_EXES = $(addsuffix /test.armv7l.elf,$(STACK_PROTECTOR_DIRS))
STACK_PROTECTOR_X64_EXES = $(addsuffix /test.x64.elf,$(STACK_PROTECTOR_DIRS))

# This test case requires a shared library dependency.
SHARED_LIBRARY_DIRS = sanity/pass/tricky_plt_stub
SHARED_LIBRARY_ARM_SOS    = $(addsuffix /libaux.armv7l.so,$(SHARED_LIBRARY_DIRS))
SHARED_LIBRARY_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(SHARED_LIBRARY_DIRS))
SHARED_LIBRARY_X64_SOS    = $(addsuffix /libaux.x64.so,$(SHARED_LIBRARY_DIRS))
SHARED_LIBRARY_X64_EXES   = $(addsuffix /test.x64.elf,$(SHARED_LIBRARY_DIRS))
SHARED_LIBRARY_PPC32_SOS  = $(addsuffix /libaux.ppc32.so,$(SHARED_LIBRARY_DIRS))
SHARED_LIBRARY_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(SHARED_LIBRARY_DIRS))

# This test case requires a shared library, but not a standalone executable
# that links against the shared library. Note that we use the file extension
# .elf rather than .so in order to make it easier for the grease test suite to
# discover the shared libraries.
STANDALONE_SHARED_LIBRARY_DIRS = sanity/pass/simulate_from_shared_library
STANDALONE_SHARED_LIBRARY_ARM_SOS    = $(addsuffix /test.armv7l.elf,$(STANDALONE_SHARED_LIBRARY_DIRS))
STANDALONE_SHARED_LIBRARY_X64_SOS    = $(addsuffix /test.x64.elf,$(STANDALONE_SHARED_LIBRARY_DIRS))
STANDALONE_SHARED_LIBRARY_PPC32_SOS  = $(addsuffix /test.ppc32.elf,$(STANDALONE_SHARED_LIBRARY_DIRS))



ASSEMBLY_ARM_DIRS = sanity/pass/push-pop
ASSEMBLY_ARM_EXES = $(addsuffix /extra/test.armv7l.elf,$(ASSEMBLY_ARM_DIRS))

ASSEMBLY_STANDALONE_SHARED_LIBRARY_DIRS = sanity/pass/addr-override-internal-reg-mod sanity/pass/addr-override-call-mod
ASSEMBLY_SHARED_LIBRARY_X64_SOS = $(addsuffix /extra/test.x64.elf,$(ASSEMBLY_STANDALONE_SHARED_LIBRARY_DIRS))

SHARED_LIBARY_DWARFV4_DIRS = sanity/pass/dwarf-shapes-dwarfv4
SHARED_LIBARY_DWARFV4_X64_SOS = $(addsuffix /test.x64.elf,$(SHARED_LIBARY_DWARFV4_DIRS))

SHARED_LIBARY_DWARFV5_DIRS = sanity/pass/dwarf-restrict-ptr-and-typedef sanity/pass/dwarf-shapes sanity/pass/dwarf-shape-recursive sanity/pass/dwarf-shape-recursive-tree sanity/pass/dwarf-fail-to-parse-log \
	sanity/pass/dwarf-array sanity/pass/dwarf-default-for-field-members-and-pointers sanity/pass/dwarf-conservative-shapes
SHARED_LIBARY_DWARFV5_X64_SOS = $(addsuffix /test.x64.elf,$(SHARED_LIBARY_DWARFV5_DIRS))


RAW_BINARY_DIRS = sanity/pass/simple-static
RAW_BINARY_EXES = $(addsuffix /test.armv7l.elf,$(RAW_BINARY_DIRS))

NON_STATIC_DIRS = $(DYNAMIC_DIRS) \
                  $(DYNAMIC_PIE_DIRS) \
                  $(STRIPPED_STATIC_DIRS) \
                  $(ASSEMBLY_ARM_DIRS) \
                  $(ASSEMBLY_STANDALONE_SHARED_LIBRARY_DIRS) \
                  $(STATIC_WITH_LIBS_DIRS) \
                  $(STACK_PROTECTOR_DIRS) \
                  $(SHARED_LIBRARY_DIRS) \
                  $(STANDALONE_SHARED_LIBRARY_DIRS) \
                  $(SHARED_LIBARY_DWARFV4_DIRS) \
                  $(SHARED_LIBARY_DWARFV5_DIRS) \
                  $(RAW_BINARY_DIRS)
STATIC_DIRS = $(filter-out $(NON_STATIC_DIRS), $(DIRS))
STATIC_ARM_EXES   = $(addsuffix /test.armv7l.elf,$(STATIC_DIRS))
# See discussion on !288 for why libpng-cve-2018-13785 appears here.
STATIC_X64_EXES   = $(addsuffix /test.x64.elf,$(STATIC_DIRS) refine/neg/libpng-cve-2018-13785)
STATIC_PPC32_EXES = $(addsuffix /test.ppc32.elf,$(STATIC_DIRS))

EXES = $(DYNAMIC_ARM_EXES) $(DYNAMIC_PPC32_EXES) \
       $(DYNAMIC_PIE_ARM_EXES) $(DYNAMIC_PIE_PPC32_EXES) \
       $(STATIC_ARM_EXES) $(STATIC_X64_EXES) $(STATIC_PPC32_EXES) \
       $(STRIPPED_STATIC_ARM_EXES) $(STRIPPED_STATIC_X64_EXES) $(STRIPPED_STATIC_PPC32_EXES) \
	   $(STATIC_WITH_LIBS_ARM_EXES) $(STATIC_WITH_LIBS_PPC32_EXES) \
	   $(STACK_PROTECTOR_ARM_EXES) $(STACK_PROTECTOR_X64_EXES) \
	   $(SHARED_LIBRARY_ARM_EXES) $(SHARED_LIBRARY_X64_EXES) $(SHARED_LIBRARY_PPC32_EXES) \
	   $(STANDALONE_SHARED_LIBRARY_ARM_SOS) $(STANDALONE_SHARED_LIBRARY_X64_SOS) $(STANDALONE_SHARED_LIBRARY_PPC32_SOS) $(SHARED_LIBARY_DWARFV4_X64_SOS) $(SHARED_LIBARY_DWARFV5_X64_SOS) \
	   $(ASSEMBLY_ARM_DIRS) $(ASSEMBLY_SHARED_LIBRARY_X64_SOS) \
	   $(RAW_BINARY_EXES)

SOS = $(SHARED_LIBRARY_ARM_SOS) $(SHARED_LIBRARY_X64_SOS) $(SHARED_LIBRARY_PPC32_EXES)

BC_DIRS = $(shell find llvm-bc/ -mindepth 1 -maxdepth 1 -type d)
BCS = $(addsuffix /test.bc,$(BC_DIRS))
STANDALONE_BCS = $(patsubst %.c, %.bc, $(wildcard llvm-bc/*.c) $(shell find feat/ -type f -name '*.llvm.c'))

.PHONY: all fmt clean

all: $(EXES) $(SOS) $(BCS) $(STANDALONE_BCS)

fmt:
	clang-format -i $(DIRS:=/test.c)

$(STATIC_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS) -o $@ $<

$(STATIC_X64_EXES): %/test.x64.elf: %/test.c
	$(X64CC) $(CFLAGS) -o $@ $<

$(STATIC_PPC32_EXES): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS) -o $@ $<

$(STRIPPED_STATIC_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS) -o $@ $<
	armv7l-linux-musleabi-strip $@

$(STRIPPED_STATIC_X64_EXES): %/test.x64.elf: %/test.c
	$(X64CC) $(CFLAGS) -o $@ $<
	strip $@

$(STRIPPED_STATIC_PPC32_EXES): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS) -o $@ $<
	powerpc-linux-musl-strip $@

$(STATIC_WITH_LIBS_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS_COMMON) $(CFLAGS_STATIC) -o $@ $<

$(STATIC_WITH_LIBS_PPC32_EXES): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS_COMMON) $(CFLAGS_STATIC) -o $@ $<

$(STACK_PROTECTOR_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) -fstack-protector-all $(CFLAGS_STATIC) -o $@ $<

$(STACK_PROTECTOR_X64_EXES): %/test.x64.elf: %/test.c
	$(X64CC) -fstack-protector-all $(CFLAGS_STATIC) -o $@ $<

$(DYNAMIC_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS_COMMON) -no-pie -o $@ $<

$(DYNAMIC_PPC32_EXES): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS_COMMON) -no-pie -o $@ $<

$(DYNAMIC_PIE_ARM_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS_COMMON) -fpic -o $@ $<

$(DYNAMIC_PIE_PPC32_EXES): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS_COMMON) -fpic -o $@ $<

$(SHARED_LIBRARY_ARM_SOS): %/libaux.armv7l.so: %/aux.c
	$(ARMCC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(SHARED_LIBRARY_ARM_EXES): %/test.armv7l.elf: %/test.c %/libaux.armv7l.so
	$(ARMCC) $(CFLAGS_COMMON) -nostartfiles -fpic -L$(dir $@) $< -laux.armv7l -o $@

$(SHARED_LIBRARY_X64_SOS): %/libaux.x64.so: %/aux.c
	$(X64CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(SHARED_LIBRARY_X64_EXES): %/test.x64.elf: %/test.c %/libaux.x64.so
	$(X64CC) $(CFLAGS_COMMON) -nostartfiles -fpic -L$(dir $@) $< -laux.x64 -o $@

$(SHARED_LIBRARY_PPC32_SOS): %/libaux.ppc32.so: %/aux.c
	$(PPC32CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(SHARED_LIBRARY_PPC32_EXES): %/test.ppc32.elf: %/test.c %/libaux.ppc32.so
	$(PPC32CC) $(CFLAGS_COMMON) -nostartfiles -fpic -L$(dir $@) $< -laux.ppc32 -o $@

$(STANDALONE_SHARED_LIBRARY_ARM_SOS): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(STANDALONE_SHARED_LIBRARY_X64_SOS): %/test.x64.elf: %/test.c
	$(X64CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(STANDALONE_SHARED_LIBRARY_PPC32_SOS): %/test.ppc32.elf: %/test.c
	$(PPC32CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -fpic -shared $< -o $@

$(SHARED_LIBARY_DWARFV4_X64_SOS): %/test.x64.elf: %/test.c
	$(X64CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -gdwarf-4 -fpic -shared $< -o $@

$(SHARED_LIBARY_DWARFV5_X64_SOS): %/test.x64.elf: %/test.c
	$(X64CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -gdwarf-5 -fpic -shared $< -o $@

$(ASSEMBLY_ARM_EXES): %/extra/test.armv7l.elf: %/test.S
	$(ARMCC) $(CFLAGS) $< -o $@

 $(ASSEMBLY_SHARED_LIBRARY_X64_SOS): %/extra/test.x64.elf: %/test.S
	$(X64CC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) -shared $< -o $@

$(RAW_BINARY_EXES): %/test.armv7l.elf: %/test.c
	$(ARMCC) $(CFLAGS_COMMON) $(CFLAGS_NO_LIBS) $(CFLAGS_STATIC) -c $< -o  $*/test.o
	objcopy -I elf32-little $*/test.o --dump-section .text=$@
	rm $*/test.o

$(BCS): %/test.bc: %/test.c
	$(X64CC) $(shell ./extract-cflags.py $<) -c -o $@ $<

feat/%.bc: feat/%.c
	$(X64CC) $(shell ./extract-cflags.py $<) -c -o $@ $<

llvm-bc/%.bc: llvm-bc/%.c
	$(X64CC) $(shell ./extract-cflags.py $<) -c -o $@ $<

clean:
	rm -f $(EXES)
