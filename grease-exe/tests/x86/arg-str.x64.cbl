; Copyright (c) Galois, Inc. 2025

; Ensure that the --arg-str CLI flag works for x64.

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; flags {"--symbol", "test"}
;; flags {"--arg-str", "rdi:hello"}
;; go(prog)
;; check "Using precondition:"
;; check "rdi: 000001+"
;; check "000001: 68 65 6c 6c 6f 00"
;; ok()
