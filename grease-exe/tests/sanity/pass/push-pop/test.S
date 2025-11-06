@ Check that the semantics for the `pop` instruction does not attempt to read
@ from uninitialized parts of the stack.

.global test
.syntax unified
.type	test, %function
.thumb
test:
  push  {r7}
  pop   {r7}
  mov   r0, #1
  bx    lr

.global _start
.syntax unified
_start:
  bl    test
