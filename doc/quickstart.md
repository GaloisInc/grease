# Quick start

This page will get you up and running as quickly as possible with the GREASE
CLI. For more detailed instructions, see [Installation](install.md) and
[Usage](usage.md).

First, compile a binary to analyze with GREASE:

```sh
echo 'int main() { return 0; }' > test.c
clang test.c -o test
```

Then, run GREASE in a Docker container:
```sh
docker run --rm -v "${PWD}:${PWD}" -w "${PWD}" ghcr.io/galoisinc/grease:nightly test
```

Note that GREASE only supports analysis of ELF executables, so this will work
best on Linux. If you're not using Linux, you can try running GREASE on one of
the binaries in its test suite, under `grease-exe/tests`.
