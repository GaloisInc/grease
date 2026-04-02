// CFLAGS: -O0
// GREASE: --symbol main --sym-stdin 16 --loop-bound 512 --solver-timeout 120
// OUTPUT: Call to abort

// CRC-32 with a runtime-populated polynomial table from zlib-cve-2022-37434.c
// (see that file for license). Unlike crc32-rodata.c, the table lives in .bss
// and is filled on first call, so reads go through the normal (non-rodata)
// memory model.

#include <stdlib.h>
#include <unistd.h>

static unsigned long crc_table[256];
static int crc_table_computed = 0;

static void make_crc_table(void) {
  for (int n = 0; n < 256; n++) {
    unsigned long c = (unsigned long)n;
    for (int k = 0; k < 8; k++) {
      c = (c & 1) ? 0xedb88320UL ^ (c >> 1) : (c >> 1);
    }
    crc_table[n] = c;
  }
  crc_table_computed = 1;
}

unsigned long crc32(unsigned long crc, const unsigned char *buf,
                    unsigned int len) {
  if (!crc_table_computed)
    make_crc_table();
  unsigned long c = crc ^ 0xffffffffUL;
  for (unsigned int n = 0; n < len; n++) {
    c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);
  }
  return c ^ 0xffffffffUL;
}

int main(void) {
  unsigned char buf[16];
  ssize_t n = read(0, buf, sizeof(buf));
  if (n <= 0)
    return 1;

  unsigned long checksum = crc32(0, buf, (unsigned int)n);
  if (checksum != 0) {
    abort();
  }

  return 0;
}
