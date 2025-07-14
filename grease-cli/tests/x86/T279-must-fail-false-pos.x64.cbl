; Copyright (c) Galois, Inc. 2025

; Regression test for #79, wherein the must-fail heuristic has a false positive
; on simple Macaw memory model loads.

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let ptr0 (get-reg rdi regs))
    (let offset (bv 64 4))
    (let ptr1 (pointer-add ptr0 offset))
    (let _ (pointer-read (Bitvector 8) le ptr1))
    (return regs)))

; TODO(#79): Heuristics should be able to account for this load by expanding
; *rdi.
;; must_fail()
