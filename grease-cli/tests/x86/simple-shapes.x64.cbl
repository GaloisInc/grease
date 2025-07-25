; Copyright (c) Galois, Inc. 2025

; Ensure that simple shapes CLI flags are working for machine code.

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; flags {"--symbol", "test"}
;; flags {"--arg-buf-uninit", "rax:4"}
;; flags {"--arg-buf-init", "rcx:4"}
;; flags {"--arg-u64", "rdx:42"}
;; go(prog)
;; check "Using precondition:"
;; check [[
;; rax: 000000+0000000000000000
;; rcx: 000001+0000000000000000
;; rdx: 000000000000002a
;; ]]
;; check [[
;; 000000: ## ## ## ##
;; 000001: XX XX XX XX
;; ]]
