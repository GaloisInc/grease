; Copyright (c) Galois, Inc. 2024

; flags: --symbol test

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (return regs)))
;; ok()
