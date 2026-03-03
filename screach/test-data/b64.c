#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// String of base64 chars and their inverses for quick lookup for encoding and
// decoding
static const char *b64chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
int b64invs[] = {62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60,
                 61, -1, -1, -1, -1, -1, -1, -1, 0,  1,  2,  3,  4,  5,
                 6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 27,
                 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
                 42, 43, 44, 45, 46, 47, 48, 49, 50, 51};

// Calcuate the length of the final base64 encoded string
size_t b64_encoded_size(size_t inlen) {
  size_t ret;

  ret = inlen;
  if (inlen % 3 != 0)
    ret += 3 - (inlen % 3);
  ret /= 3;
  ret *= 4;

  return ret;
}

// Safely base64 encode a string given a string and its length.
// Allocates a new string so the caller must free the string when done with it
char *b64_encode(const unsigned char *in, size_t len) {
  char *out;
  size_t elen;
  size_t i;
  size_t j;
  size_t v;

  if (in == NULL || len < 0)
    return NULL;

  elen = b64_encoded_size(len);
  out = malloc(elen + 1);
  out[elen] = '\0';

  for (i = 0, j = 0; i < len; i += 3, j += 4) {
    v = in[i];
    v = i + 1 < len ? v << 8 | in[i + 1] : v << 8;
    v = i + 2 < len ? v << 8 | in[i + 2] : v << 8;

    out[j] = b64chars[(v >> 18) & 0x3F];
    out[j + 1] = b64chars[(v >> 12) & 0x3F];
    if (i + 1 < len) {
      out[j + 2] = b64chars[(v >> 6) & 0x3F];
    } else {
      out[j + 2] = '=';
    }
    if (i + 2 < len) {
      out[j + 3] = b64chars[v & 0x3F];
    } else {
      out[j + 3] = '=';
    }
  }

  return out;
}

void vuln() {
  // used to mark assertion where vulnerability occurs
  return;
}

// Unsafely base64 encode a string given a string and its length with no NULL
// check. If a NULL ptr is passed, it will be dereferenced causing a crash
char *b64_encode_unsafe(const unsigned char *in, size_t len) {
  char *out;
  size_t elen;
  size_t i;
  size_t j;
  size_t v;

  elen = b64_encoded_size(len);
  out = malloc(elen + 1);
  out[elen] = '\0';

  for (i = 0, j = 0; i < len; i += 3, j += 4) {
    if (in == NULL) {
      vuln();
    }
    v = in[i];
    v = i + 1 < len ? v << 8 | in[i + 1] : v << 8;
    v = i + 2 < len ? v << 8 | in[i + 2] : v << 8;

    out[j] = b64chars[(v >> 18) & 0x3F];
    out[j + 1] = b64chars[(v >> 12) & 0x3F];
    if (i + 1 < len) {
      out[j + 2] = b64chars[(v >> 6) & 0x3F];
    } else {
      out[j + 2] = '=';
    }
    if (i + 2 < len) {
      out[j + 3] = b64chars[v & 0x3F];
    } else {
      out[j + 3] = '=';
    }
  }

  return out;
}

// Calcuate the original length of a string before encoding
size_t b64_decoded_size(const char *in) {
  size_t len;
  size_t ret;
  size_t i;

  if (in == NULL)
    return 0;

  len = strlen(in);
  ret = len / 4 * 3;

  for (i = len; i-- > 0;) {
    if (in[i] == '=') {
      ret--;
    } else {
      break;
    }
  }

  return ret;
}

// Check vailidity of base64 strings, only accept 0-9, a-zA-Z, '+', '/',
// and '=' as padding
int b64_isvalidchar(char c) {
  if (c >= '0' && c <= '9')
    return 1;
  if (c >= 'A' && c <= 'Z')
    return 1;
  if (c >= 'a' && c <= 'z')
    return 1;
  if (c == '+' || c == '/' || c == '=')
    return 1;
  return 0;
}

// Decode a base64 encoded string given a string and its length.
// Allocates a new string so the caller must free the string when done with it.
char *b64_decode(const char *in) {
  size_t len;
  size_t outlen;
  size_t i;
  size_t j;
  int v;

  if (!in)
    return NULL;

  len = strlen(in);
  outlen = b64_decoded_size(in);
  char *out = malloc(sizeof(char) * outlen + 1);
  out[outlen] = '\0';

  for (i = 0; i < len; i++) {
    if (!b64_isvalidchar(in[i])) {
      return NULL;
    }
  }

  for (i = 0, j = 0; i < len; i += 4, j += 3) {
    v = b64invs[in[i] - 43];
    v = (v << 6) | b64invs[in[i + 1] - 43];
    v = in[i + 2] == '=' ? v << 6 : (v << 6) | b64invs[in[i + 2] - 43];
    v = in[i + 3] == '=' ? v << 6 : (v << 6) | b64invs[in[i + 3] - 43];

    out[j] = (v >> 16) & 0xFF;
    if (in[i + 2] != '=')
      out[j + 1] = (v >> 8) & 0xFF;
    if (in[i + 3] != '=')
      out[j + 2] = v & 0xFF;
  }

  return out;
}

// ==================== ADDED ====================

// compile-time configuration
#define PROXY_ENABLED 1
#define ECHO_ENABLED 0
#define BASE64_ENABLED 1

// from httpd.h and related
#define OK 0
#define DECLINED -1
#define M_POST 2
#define REQUEST_CHUNKED_DECHUNK 2
#define APR_SUCCESS 0
#define HTTP_INTERNAL_SERVER_ERROR 500

typedef size_t apr_size_t;
typedef long apr_status_t;

#define ap_rprintf printf

// -------------------- stubs --------------------

typedef struct request_rec {
  size_t read;
} request_rec;

// represent values of char* request_rec->handler
typedef enum HandlerTypes {
  NONE,
  DEFAULT,
  BALANCER_MANAGER,
  PROXY_SERVER,
  ECHO,
  BASE64,
  IMAP_MAGIC_TYPE,
  REDIRCT_HANDLER,
  FORM_LOGIN,
  FORM_LOGOUT,
  FORM_REDIRECT,
} handler_type_t;

long ap_setup_client_block(request_rec **r, long read_policy) {
  *r = (request_rec *)malloc(sizeof(request_rec));
  if (r == NULL) {
    return HTTP_INTERNAL_SERVER_ERROR;
  }
  (*r)->read = 0;
  return OK;
}

long ap_should_client_block(request_rec *r) { return 1; }

long ap_get_client_block(request_rec *r, char *buffer, apr_size_t bufsiz) {
  if (r->read < bufsiz) {
    r->read -= 1;
    return 1;
  }
  return 0;
}

// -------------------- call chain --------------------

// Modifies POST data from request header
int modify_request_header(const unsigned char *data, handler_type_t handler,
                          long method_number) {
  request_rec *r;
  if (!handler || handler != BASE64) {
    return DECLINED;
  }
  char buffer[8192];
  char err_buf[8192];

  if (method_number == M_POST) {
    // Setup client block to read
    apr_size_t len = sizeof(buffer) - 1;
    apr_size_t b64_len = 0;
    // Check that we are in a good state to recieve client data
    apr_status_t rv = ap_setup_client_block(&r, REQUEST_CHUNKED_DECHUNK);
    if (rv != APR_SUCCESS) {
      return HTTP_INTERNAL_SERVER_ERROR;
    }

    // Checks that client is going to send data
    if (ap_should_client_block(r)) {
      apr_size_t read_len = len - 1;
      // Read request data into local buf
      while ((rv = ap_get_client_block(r, buffer, read_len)) > 0) {
        b64_len += rv;
      }
    }
    ap_rprintf("Received POST data: %s\n", b64_encode_unsafe(data, b64_len));
  }

  return OK;
}

// -------------------- model of apache request handling --------------------

int ap_run_handler(size_t registered_handlers, const unsigned char *data,
                   handler_type_t handler, long method_number) {
  // only run if we have one or more registered_handlers
  if (registered_handlers) {
    return modify_request_header(data, handler, method_number);
  } else {
    return DECLINED;
  }
}

int ap_invoke_handler(handler_type_t handler, long method_number) {
  // handler set in part controlled by compile-time options
  handler_type_t handlers[] = {
      DEFAULT,
#if PROXY_ENABLED
      PROXY_SERVER,
#endif
#if ECHO_ENABLED
      ECHO,
#endif
#if BASE64_ENABLED
      BASE64,
#endif
  };
  size_t handler_count = sizeof(handlers) / sizeof(handlers[0]);
  return ap_run_handler(handler_count, NULL, handler, method_number);
}

int ap_process_request_internal(request_rec *r) {
  int access_status = DECLINED;
  access_status = OK;
  return access_status;
}

void ap_process_async_request(request_rec *r) {
  int access_status;
  access_status = ap_process_request_internal(r);

  if (access_status == OK) {
    // access_status = ap_invoke_handler(r); // TODO: input from stdin or
    // something?
  }
}

// mocks
void ap_run_process_connection(request_rec *r) { ap_process_async_request(r); }
void process_socket(request_rec *r) { ap_run_process_connection(r); }
void worker_thread(request_rec *r) { process_socket(r); }
void thread_start(request_rec *r) { worker_thread(r); }
void start_thread(request_rec *r) { thread_start(r); }

/* Requirements

- some kind of backtracking required?
- some sort of tarpit that can be avoided
- core dump? need harness to trigger from main, or just gdb dump (use forklift)

*/

int main(int argc, char *argv[]) {
  const unsigned char *data = (const unsigned char *)b64chars;
  // char *s = b64_encode_unsafe(data, strlen(b64chars));
  // puts(s);
  // free(s);
  return 0;
}

// CFLAGS: $CFLAGS_COMMON

/// flags {"--entry-symbol", "ap_invoke_handler"}
/// flags {"--target-symbol", "vuln"}
/// go(prog)
/// reached "vuln"
/// verified()

// The next two tests check that the logs contain the function name when using
// `--target-addr`.

/// flags {"--entry-symbol", "ap_invoke_handler"}
/// flags {"--target-addr", "0x6c2"}
/// go(prog)
/// check "Reached target address 0x100006c2 [in function 'vuln' at address 0x100006c2]"
/// verified()

/// flags {"--entry-symbol", "ap_invoke_handler"}
/// flags {"--target-addr", "0x6c3"}
/// go(prog)
/// check "Reached target address 0x100006c3 [in function 'vuln' at address 0x100006c2]"
/// verified()
