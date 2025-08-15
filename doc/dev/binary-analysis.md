# Binary analysis

## Symbolic execution of binaries

To perform symbolic execution on a binary, an analysis tool such as GREASE must:

- Parse the object file format (e.g., ELF on Linux or Mach-O for macOS)
- Locate sections of the executable that may contain machine code
- Within code sections, perform *code discovery* to locate potentially-reachable
  machine code (i.e., differentiating it from data) and *disassembly* to lift
  that machine code into assembly or a higher level intermediate representation
  for furhter analysis.
- Initialize the environment in which the code will be symbolically executed,
  e.g., by mapping data sections from the object file into symbolic memory.
- Execute the lifted machine code.

These steps are not necessarily independent nor performed in the sequence
described above. For example, GREASE performs code discovery *lazily* as
symbolic execution reaches new addresses.

References:

- [On the Decidability of Disassembling Binaries](https://link.springer.com/chapter/10.1007/978-3-031-64626-3_8)

## Executing an ELF binary on Linux

For precise analysis of machine code, GREASE must provide an accurate model of
the *environment* in which the code runs. This environment consists of:

- The initial state of memory (see [Memory model](memory-model.md))
- The run-time interaction with the program and the rest of the system:
  - Responses to library and system calls (see [Built-in overrides](builtins.md))
  - Run-time interactions with the loader (RTLD)
  - Signals (not modelled by GREASE)

In order to understand how GREASE should emulate this environment, it is
important to first understand how loading, dynamically linking, and executing a
program works on a real Linux system. The usual high-level flow is like this:

- The loading program (e.g. the shell) `fork`s (see `man 2 fork`) and then calls
  `execv` function (or similar) from libc (see `man 3 exec`), with the path to
  the binary as an argument.
- libc's `execv` issues a system call (`man 2 syscall`) to Linux's `execve`

  - On AArch32, this involves putting the syscall number in `r7`, arguments in
    `r0` through `r6`, and executing a `svc` (SuperVisor Call) instruction.
  - On x86_64, this involves putting the syscall number in `rax`, arguments in
    some other registers, and executing a `syscall` instruction.

- `execve` examines the file at the given path to determine if it is a shell
  script or a binary executable.
- If it is an ELF executable, `execve` parses the ELF header.
  - If the header indicates that the program is dynamically linked, `execve`
    [parses the program headers], [locates the `.interp` header], and executes
    the program listed there (usually `ld.so`, see `man 8 ld.so`).
  - Otherwise, the kernel maps the ELF segments into memory itself, and jumps
    directly to the program entry point.
- In either case, the kernel sets up the program stack and `.bss` segment. The
  end of the stack is [initialized] with:

    - the [ELF auxiliary vector]
    - the argument count (`argc`), arguments (`argv`), and environment (`envp`)

- Finally, the kernel yields control to the interpreter or the executable
  itself (for dynamically or statically linked programs, respectively).

[parses the program headers]: https://github.com/torvalds/linux/blob/ee94b00c1a648530333d9734200be7a45e6e00cd/fs/binfmt_elf.c#L854
[locates the `.interp` header]: https://github.com/torvalds/linux/blob/ee94b00c1a648530333d9734200be7a45e6e00cd/fs/binfmt_elf.c#L867
[initialized]: https://github.com/torvalds/linux/blob/ee94b00c1a648530333d9734200be7a45e6e00cd/fs/binfmt_elf.c#L234
[ELF auxiliary vector]: https://lwn.net/Articles/519085/
