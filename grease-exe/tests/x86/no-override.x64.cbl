; Copyright (c) Galois, Inc. 2025

; Test the behavior when an external function is declared but not defined.
;
; Regression test for #497.

;; flags {"--symbol", "test"}
;; go(prog)

(declare @extern () Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @extern)
    (return regs)))

; TODO(#497):
; ok()
;; must_fail()
;; check [[Global symbol "extern" has no associated allocation]]
