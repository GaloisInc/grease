; Copyright (c) Galois, Inc. 2025

; Test --skip with a bogus function name in Macaw S-expression files.

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; flags {"--skip", "bogus"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
