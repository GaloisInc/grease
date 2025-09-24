#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Macros defined in OpenSSL library file 'ssl/ssl_locl.h'.
#define n2s(c,s) ((s=(((unsigned int)(c[0]))<< 8)| \
                     (((unsigned int)(c[1]))    )),c+=2)
#define s2n(s,c) ((c[0]=(unsigned char)(((s)>> 8)&0xff), \
                   c[1]=(unsigned char)(((s)    )&0xff)),c+=2)

// Macros defined in OpenSSL library file 'ssl/ssl3.h'.
#define TLS1_HB_REQUEST  1
#define TLS1_HB_RESPONSE 2

#define POC_HEARTBLEED_REAL_PAYLOAD_LEN 2
#define POC_HEARTBLEED_NOMINAL_PAYLOAD_LEN 20

// Fill in the 'buf' array with 'num' random numbers.
void RAND_pseudo_bytes(unsigned char *buf, size_t num) {
  for (size_t i = 0; i < num; ++i) {
    buf[i] = rand();
  }
}


// Adapted from `tls1_process_heartbeat` in OpenSSL file 'ssl/t1_lib.c'.
int test(unsigned char *message) {
  unsigned char *p = &message[0], *pl;
  unsigned short hbtype;
  unsigned int payload;
  unsigned int padding = 16; /* Use minimum padding */

  /* Read type and payload length first */
  hbtype = *p++;
  n2s(p, payload);
  pl = p;

  if (hbtype == TLS1_HB_REQUEST) {
    unsigned char *buffer, *bp;

    /* Allocate memory for the response, size is 1 bytes
     * message type, plus 2 bytes payload length, plus
     * payload, plus padding
     */
    buffer = (unsigned char *) malloc (1 + 2 + payload + padding);
    if (buffer == NULL) {
      printf("Failed to allocate space for the response buffer\n");
      return -1;
    }
    bp = buffer;

    /* Enter response type, length and copy payload */
    *bp++ = TLS1_HB_RESPONSE;
    s2n(payload, bp);
    memcpy(bp, pl, payload);
    bp += payload;
    /* Random padding */
    RAND_pseudo_bytes(bp, padding);

    // Instead of returning the heartbeat response, we'll just print the exposed buffer to stdout.
    for (size_t i = 0; i < payload; ++i) {
      unsigned char byte = *(buffer + 1 + 2 + i);
      printf("response payload [%zu]: %03d (0x%02x)\n", i, byte, byte);
    }

    free(buffer);
  } else {
    return -1;
  }

  return 0;
}

// all: flags {"--symbol", "test"}
// all: flags {"--no-heuristics"}
// all: flags {"--arg-buf-init", "rdi:19"}
// all: go(prog)
// all: could_not_infer()

// all: flags {"--symbol", "test"}
// all: flags {"--no-heuristics"}
// all: flags {"--initial-precondition", "tests/refine/bug/heartbleed/shapes.txt"}
// all: go(prog)
// all: must_fail()
