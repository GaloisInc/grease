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

GREASE provides basic overrides for the following socket I/O functions:

- `accept`
- `bind`
- `connect`
- `listen`
- `recv`
- `send`
- `socket`

These overrides are quite limited.

- For successful execution, GREASE must execute the entire
  "chain" of socket-related function calls, e.g., for a server,
  `socket`/`bind`/`listen`/`accept`/`recv`. If GREASE only executes a suffix of
  the chain, execution will fail.
- Opening different sockets in different branches of execution is not supported.
- The socket domain, type, and path (for `AF_UNIX` sockets) or port number (for
  `AF_INET{6}` sockets) must all be concrete.
- Only domains `AF_UNIX`, `AF_INET`, and `AF_INET6` are supported.
- Only socket types `SOCK_STREAM`, `SOCK_DGRAM`, and `SOCK_SEQPACKET` are
  supported.
- Reading from sockets requires setting their contents in the symbolic
  filesystem in advance. (See GREASE's test suite for an example.)
- `accept` ignores the `addr` and `addrlen` parameters.
<!-- TODO(#387): Make these available for LLVM, too -->
- These overrides are only available when analyzing binaries.
