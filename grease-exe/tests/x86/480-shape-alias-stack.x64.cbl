; Copyright (c) Galois, Inc. 2025

;; flags {"--initial-precondition", "tests/x86/extra/shape-alias-stack.txt"}
;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; check "rsp: 000000+"
;; check "rdi: 000001+"
;; ok()
