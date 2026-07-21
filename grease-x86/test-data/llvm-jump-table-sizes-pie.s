# Regenerate with:
# clang -c -o /tmp/llvm-jump-table-sizes-pie.o llvm-jump-table-sizes-pie.s
# clang -nostdlib -pie -Wl,--build-id=none -o llvm-jump-table-sizes-pie.elf /tmp/llvm-jump-table-sizes-pie.o

.section .rodata,"a"
.p2align 2
jump_table:
  .long case0 - jump_table
  .long case1 - jump_table
  .long case2 - jump_table
  .long case3 - jump_table
  .long case4 - jump_table
  .long case5 - jump_table
  .long case6 - jump_table
  .long case7 - jump_table

.section .llvm_jump_table_sizes,"a",@progbits
.quad jump_table
.quad 8

.text
.globl _start
_start:
case0:
case1:
case2:
case3:
case4:
case5:
case6:
case7:
  ret
