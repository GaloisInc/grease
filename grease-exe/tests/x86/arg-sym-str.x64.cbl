; Copyright (c) Galois, Inc. 2025

; Ensure that the --arg-sym-str CLI flag works for x64.

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; flags {"--symbol", "test"}
;; flags {"--arg-sym-str", "rdi:10"}
;; go(prog)
;; check "Using precondition:"
;; check "rdi: 000001+"
;; check "000001: XX*9 00"
;; ok()
