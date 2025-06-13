# Overview

GREASE is command-line tool, Ghidra plug-in, and Haskell library that checks properties about binaries using under-constrained symbolic execution.

`grease` supports analysis of ELF executables or shared objects containing AArch32, PowerPC, or x86_64 code. `grease` can also analyze LLVM bitcode.

<img src="logo.png" alt="GREASE logo" width="50%" />

## Demo

Consider the following function derived from `libpng`. Can you spot the bug?[^libpng]

```c
void /* PRIVATE */
png_check_chunk_length(png_const_structrp png_ptr, const unsigned int length)
{
   png_alloc_size_t limit = PNG_UINT_31_MAX;

# ifdef PNG_SET_USER_LIMITS_SUPPORTED
   if (png_ptr->user_chunk_malloc_max > 0 &&
       png_ptr->user_chunk_malloc_max < limit)
      limit = png_ptr->user_chunk_malloc_max;
# elif PNG_USER_CHUNK_MALLOC_MAX > 0
   if (PNG_USER_CHUNK_MALLOC_MAX < limit)
      limit = PNG_USER_CHUNK_MALLOC_MAX;
# endif
   if (png_ptr->chunk_name == png_IDAT)
   {
      png_alloc_size_t idat_limit = PNG_UINT_31_MAX;
      size_t row_factor =
         (png_ptr->width * png_ptr->channels * (png_ptr->bit_depth > 8? 2: 1)
          + 1 + (png_ptr->interlaced? 6: 0));
      if (png_ptr->height > PNG_UINT_32_MAX/row_factor)
         idat_limit=PNG_UINT_31_MAX;
      else
         idat_limit = png_ptr->height * row_factor;
      row_factor = row_factor > 32566? 32566 : row_factor;
      idat_limit += 6 + 5*(idat_limit/row_factor+1); /* zlib+deflate overhead */
      idat_limit=idat_limit < PNG_UINT_31_MAX? idat_limit : PNG_UINT_31_MAX;
      limit = limit < idat_limit? idat_limit : limit;
   }
   // ...
}
```

`grease` can:

```
$ clang test.c -o test
$ grease test

Finished analyzing 'png_check_chunk_length'. Possible bug(s):

At 0x100011bd:
div: denominator was zero
Concretized arguments:

rcx: 0000000000000000
rdx: 0000000000000000
rsi: 0000000000000000
rdi: 000000+0000000000000000
r8: 0000000000000000
r9: 0000000000000000
r10: 0000000000000000

000000: 54 41 44 49 01 00 00 00 f9 ff ff ff 00 00 00 00 00 80
```

This output says that `png_check_chunk_length` will divide by zero when `rdi` points to an allocation holding the bytes `54 41 44 ...`. Indeed, if we add the following `main` function:

```c
int main() {
  char data[] = {0x54, 0x41, 0x44, 0x49, 0xf9, 0x00, 0x00, 0x00, 0x01, 0xb7, 0x3e, 0x9b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80};
  png_check_chunk_length((png_const_structrp)data, 0);
  return 0;
}
```
We see exactly what `grease` described:
```
$ clang test.c -o test
$ ./test
Floating point exception (core dumped)
```

[^libpng]: This test-case is based on [CVE-2018-13785](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2018-13785). The complete example and accompanying license and copyright notice are available at `tests/refine/neg/libpng-cve-2018-13785/test.c` in the GREASE source repository.

## Algorithm

The analysis performed is roughly the following:

- The binary is disassembled and converted into a control-flow graph IR.
- `grease` symbolically simulates the entrypoint using the most general possible preconditions (all registers hold fresh symbolic variables).
- If an out-of-bounds memory read or write occurs, the precondition is refined by growing allocated / initialized memory. For example, if an out-of-bounds read occurs at an offset of the address in `r8`, we refine the precondition to make sure that `r8` points to a region of initialized memory big enough to contain the offset.
- This process of simulation and refinement repeats itself until either (1) no errors occur (in which case we move on) or (2) errors occur and we do not have heuristics to refine based on any of the errors (in which case we concretize the arguments, report an error, and exit).
- Now that we have found a memory layout that allows us to simulate the entrypoint without error, we instrument the control-flow graph with new instructions that encode further assertions.
- Finally, we run the instrumented control-flow graph with the inferred memory layout precondition. If all collected assertions pass, we report that the property holds. Otherwise, we report that the property does not hold.

For more details, see [Refinement](refinement.md).

For a capability comparison with other similar tools, see [Comparison](comparison.md).

## Acknowledgements

This material is based upon work supported by the Defense Advanced Research Projects Agency under Contract No. W31P4Q-22-C-0017 and W31P4Q-23-C-0020

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency or the U.S. Government.

## Copyright

Copyright (c) Galois, Inc. 2024.
