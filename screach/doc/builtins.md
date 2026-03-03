# Built-in overrides

In addition to [user-defined overrides](overrides.md), Screach also provides a
number of _built-in_ overrides that are built directly into the tool. Many of
the built-in overrides correspond to functions from libc. For various reasons,
these overrides are not defined using S-expression syntax. Unlike user-defined
overrides, these overrides are always enabled by default, without needing the
user to explicitly enable them.

If a user supplies an override of the same name as a built-in override, then the
user-supplied override takes precedence over the built-in override. User-defined
overrides may call built-in overrides.

Screach has many of the same built-in overrides as GREASE, see [the GREASE
documentation on built-ins][grease-builtins] for further information.

[grease-builtins]: https://galoisinc.github.io/grease/builtins.html

## I/O

Screach provides the same I/O overrides as GREASE, see [the GREASE documentation
on I/O][io].

[io]: https://galoisinc.github.io/grease/io.html

## Additional overrides

### `@reached`

Screach provides a built-in override `@reached : Bool -> Unit`.
When `@reached` is called on a satisfiable predicate, Screach will
consider a target to have been reached. `@reached` is used in [Target
overrides](./overrides.md#target-overrides).
