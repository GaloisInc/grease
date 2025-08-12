# I/O

## Overrides

GREASE provides [built-in overrides](builtins.md) for the I/O primitives `open`,
`close`, `read`, and `write`.

These overrides leverage Crucible's experimental symbolic I/O capabilities. In
particular, these overrides require the use of a symbolic filesystem, which must
be specified with `--fs-root <path-to-filesystem-root>` when invoking `grease`.
See [symbolic I/O] for a more detailed description of what the contents of the
symbolic filesystem should look like.

[symbolic I/O]: https://github.com/GaloisInc/crucible/tree/master/crux-llvm#symbolic-io-experimental

## Standard streams

GREASE attempts to provide a POSIX environment. In particular, when in use,
the [standard streams] `stdin`, `stdout`, and `stderr` are mapped to file
descriptors 0, 1, and 2 respectively. `stdin` is empty by default, though
`--sym-stdin N` populates it with `N` symbolic bytes. `stdout` and `stderr` do
not exist by default and their file descriptors may be reused, e.g., by `open`.
If their content is specified via `--fs-root`, then they will have their usual
file descriptor numbers.

## Sockets and networking

<!-- TODO(#197) -->

GREASE [doesn't yet][socket] provide support for socket-based networking APIs.

[socket]: https://github.com/GaloisInc/grease/issues/197
