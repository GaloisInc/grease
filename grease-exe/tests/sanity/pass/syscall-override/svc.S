.global test_pop
.syntax unified
.type	test_syscall, %function
.thumb
test_syscall:
  svc #0
  bx      lr

.global _start
.syntax unified
_start:
  bl      test_syscall
