; Copyright (c) Galois, Inc. 2024

; flags: --symbol test

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (return regs)))
;; ok()
