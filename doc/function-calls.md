# Function calls

GREASE's algorithm for handling function calls is as follows:

- If the call is through a symbolic function pointer, then:
  - If `--error-symbolic-fun-calls` was provided, an error is generated.
  - Otherwise, the call is skipped.
- Otherwise, the virtual address is resolved to an address in the binary. If
  this fails:
  - If `--skip-invalid-call-addrs` was provided, the call is skipped.
  - Otherwise, an error is generated.
- Once GREASE has an address in the binary, it checks if the address corresponds
  to the beginning of a function.
  - If so, that function is symbolically executed.
  - If not, GREASE checks if the address corresponds to a PLT stub.
    - If so, GREASE checks if it has an [override](overrides.md) for that
      function. If so, the override is executed. If not, the call is skipped.
      <!-- TODO(#182): Option to make this an error -->
    - If the address was not the beginning of a function and was not a PLT stub,
      then the call is skipped.

No registers or memory are havoc'd if a function call is skipped (though this [may
change] in the future).

[may change]: https://github.com/GaloisInc/grease/issues/6
