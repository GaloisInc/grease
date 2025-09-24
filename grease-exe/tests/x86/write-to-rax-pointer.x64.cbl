; Copyright (c) Galois, Inc. 2024

; A test case that writes to a pointer stored in %rax.

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rax-ptr (get-reg rax regs))
    (let zero64 (bv 64 0))
    (pointer-write (Bitvector 64) le rax-ptr zero64)
    (return regs)))
;; ok()
