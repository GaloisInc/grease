# `elf-edit-ecfs`

Utilities for reading and analyzing [extended core file snapshot
(ECFS)](https://github.com/elfmaster/ecfs) files using
[`elf-edit`](https://github.com/GaloisInc/elf-edit).

This package comes with the following example programs that demonstrate how to
use the API:

* `readecfs`: Parses and prints the details of an ECFS file to stdout.
* `et-flip`: Flip an ELF file from `ET_NONE` to `ET_CORE` (or vice versa).
