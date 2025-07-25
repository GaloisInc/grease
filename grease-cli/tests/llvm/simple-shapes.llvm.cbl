; Copyright (c) Galois, Inc. 2025

; Ensure that simple shapes CLI flags are working for LLVM.

(defun @test ((x0 (Ptr 64)) (x1 (Ptr 64)) (x2 (Ptr 32)) (x3 (Ptr 64))) Unit
  (start start:
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--arg-buf-uninit", "%0:4"}
;; flags {"--arg-buf-init", "%1:4"}
;; flags {"--arg-u32", "%2:42"}
;; flags {"--arg-u64", "%3:0x2a"}
;; go(prog)
;; check "Using precondition:"
;; check [[
;; %0: 000000+0000000000000000
;; %1: 000001+0000000000000000
;; %2: 0000002a
;; %3: 000000000000002a
;; 
;; 000000: ## ## ## ##
;; 000001: XX XX XX XX
;; ]]
