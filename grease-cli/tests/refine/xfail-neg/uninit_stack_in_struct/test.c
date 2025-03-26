/* Copyright (c) Galois, Inc. 2024 */

#include <stdio.h>
#include <stdlib.h>

#define BUFFER_SIZE 9

int test(int index) {
  struct authentication {
    char buffer[BUFFER_SIZE];
    int authenticated;
    double end;
  } auth;
  auth.buffer[4] = 10;
  return (int)auth.buffer[index];
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("Usage: %s <number>", argv[0]);
    return 1;
  }

  return test(atoi(argv[1]));
}
