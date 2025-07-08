; Test JSON shape parsing works

;; flags {"--symbol", "test"}
;; flags {"--no-heuristics"}
;; flags {"--initial-precondition", "tests/x86/extra/json-shapes.json"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; check "Using precondition:"
;; check "rax: XX XX XX XX XX XX XX XX"
;; check "rcx: 000000000000002a"
;; check "rdx: 000001+0000000000000004"
;; check "000001: ## ## ## ## XX XX XX XX"

