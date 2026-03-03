# Test generation infrastructure for Screach

The docker image in this directory allows building binary
test cases in the style of test cases in `screach/directed-test-data`.

The Makefile in `screach/directed-test-data` assumes that this image is available as test-gen:latest.

To build this image run `docker build -t test-gen .`


This image allows building x64 screach test cases by using [crosstool-NG](https://crosstool-ng.github.io/)
to build an x64 toolchain on the host platform. The image uses Ghidra to build callgraphs.

## Limitations

Since the callgraph is generated from Ghidra, indirect calls will be missing from the callgraph. For the callgraph to be representative the
test case must have relocations/direct calls that Ghidra can queue off of for all calls that are on the relevant path.

These test-cases are not ECFS coredumps so dependencies are not available, external functions must be modeled by overrides, otherwise Screach will skip them.
