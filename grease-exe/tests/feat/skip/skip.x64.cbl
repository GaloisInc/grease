; Copyright (c) Galois, Inc. 2025

; Test --skip for Macaw S-expression files.

(declare @abort () Unit)

(defun @bug ((regs X86Regs)) X86Regs
  (start start:
    (funcall @abort)
    (return regs)))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @bug regs)
    (return regs)))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()

;; flags {"--skip", "bug"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
