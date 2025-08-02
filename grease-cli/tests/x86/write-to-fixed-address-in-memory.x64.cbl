; Copyright (c) Galois, Inc. 2025

; A test case that writes to a fixed unmapped address in memory we should not try to refine.


;; flags {"--symbol", "test", "--initial-precondition", "tests/x86/extra/make-rax-point-to-deadbeef.txt"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rax-ptr (get-reg rax regs))
    (let zero64 (bv 64 0))
    (let tgt-ptr (pointer-read (Ptr 64) le rax-ptr))
    (pointer-write (Bitvector 64) le tgt-ptr zero64)
    (return regs)))
    
;; must_fail()