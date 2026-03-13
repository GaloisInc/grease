; Copyright (c) Galois, Inc. 2025

; Ensure that the --arg-sym-str CLI flag works for LLVM.

(defun @test ((s (Ptr 64))) Unit
  (start start:
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--arg-sym-str", "%0:10"}
;; go(prog)
;; check "Using precondition:"
;; check "%0: 000000+"
;; check "000000: XX*9 00"
;; ok()
