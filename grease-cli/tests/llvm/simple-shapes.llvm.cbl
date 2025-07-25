; Copyright (c) Galois, Inc. 2025

; Ensure that simple shapes CLI flags are working for LLVM.

(defun @test ((x0 (Ptr 64))) Unit
  (start start:
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--arg-buf-uninit", "%0:4"}
;; go(prog)
;; check "Using precondition:"
;; check "%0: 000000+0000000000000000"
;; check "000000: ## ## ## ##"
