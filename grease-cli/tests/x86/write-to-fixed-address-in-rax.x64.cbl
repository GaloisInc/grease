; Copyright (c) Galois, Inc. 2025

; A test case that writes to a fixed unmapped address we should not try to refine.

;; flags {"--symbol", "test", "--arg-u64", "rax:0x0bad1deadeadbeef"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rax-ptr (get-reg rax regs))
    (let zero64 (bv 64 0))
    (pointer-write (Bitvector 64) le rax-ptr zero64)
    (return regs)))
    
;; must_fail()