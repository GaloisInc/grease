; Copyright (c) Galois, Inc. 2025

; Test the behavior of pointer comparisons.
;
; Regression test for #485.

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    (let rsi (get-reg rsi regs))
    ; force them both to be pointers
    (let rdi0 (pointer-read (Ptr 8) le rdi))
    (let rsi0 (pointer-read (Ptr 8) le rsi))
    (let leq (pointer-leq rdi rsi))
    (assert! leq "TODO(#485): This should not unconditionally pass")
    (return regs)))

; TODO(#485):
; no_heuristic()
;; ok()
