# System calls

Most programs make system calls (syscalls) via libc. GREASE provides
[overrides](builtins.md) for many functions in libc, so it is relatively rare
for GREASE to encounter a system call during simulation. However, syscalls may
still arise from direct use of instructions like x86_64's `syscall`, especially
in statically-linked binaries. GREASE currently has rudimentary support for such
cases. As GREASE currently only supports analyzing ELF binaries, it also only
supports Linux system calls.

GREASE's algorithm for handling syscalls is as follows:


- If the syscall number is symbolic, then:
  - If `--error-symbolic-syscalls` was provided, an error is generated
  - Otherwise, the call is skipped
- Otherwise, GREASE checks for a syscall override
  - If none can be found, the call is skipped
  - If one is found, then it is invoked

No registers or memory are havoc'd if a system call is skipped (though this [may
change] in the future).

[may change]: https://github.com/GaloisInc/grease/issues/6

## Syscall overrides

GREASE currently only provides a single syscall override, for `getppid`. On
Linux, a parent PID can change at any time due to reparenting, so this override
always returns a fresh value.
