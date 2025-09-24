/* Copyright (c) Galois, Inc. 2025 */

// Test that the overrides for networking-related functions work as expected.

// all: flags {"--symbol", "test"}
// all: flags {"--fs-root", "tests/refine/neg/extra/server-fs"}
// all: go(prog)

#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

void test(void) {
  int sock_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (sock_fd == -1) {
    return;
  }

  struct sockaddr_in server;
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  // NB: the number 5000 becomes 34835 when converted from
  // host byte order to network byte order, which is why the
  // corresponding directory in the symbolic filesystem is
  // tests/refine/neg/extra/server-fs/root/network/AF_INET/SOCK_STREAM/34835, as
  // seen in tests/refine/neg/extra/server-fs/system-manifest.json.
  server.sin_port = htons(5000);
  int bind_rc = bind(sock_fd, (struct sockaddr *)&server, sizeof(server));
  if (bind_rc == -1) {
    return;
  }

  int listen_rc = listen(sock_fd, 5);
  if (listen_rc == -1) {
    return;
  }

  int conn_fd = accept(sock_fd, NULL, NULL);
  if (conn_fd == -1) {
    return;
  }

  char buf[4];
  memset(buf, 0, sizeof(buf));

  int recv_rc = recv(conn_fd, buf, sizeof(buf), 0);
  if (recv_rc == -1) {
    return;
  }
  if (buf[0] == 'A' && buf[1] == 'B' && buf[2] == 'C' && buf[3] == 'D') {
    close(conn_fd);
    close(sock_fd);
    abort();
  }

  int send_rc = send(conn_fd, buf, sizeof(buf), 0);
  if (send_rc == -1) {
    return;
  }

  close(conn_fd);
  close(sock_fd);
}

// all: could_not_infer()
// all: check "Call to abort"
// all: check [[
// all: Concretized filesystem:
// all:   /network/AF_INET/SOCK_STREAM/34835/0
// all:     41 42 43 44
// all: ]]
