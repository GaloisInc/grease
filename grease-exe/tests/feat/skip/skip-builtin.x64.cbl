; Copyright (c) Galois, Inc. 2025

; Test --skip for `@declare`s of built-ins in Macaw S-expression files.

(declare @abort () Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @abort)
    (return regs)))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()

;; flags {"--skip", "abort"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
