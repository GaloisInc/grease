; Copyright (c) Galois, Inc. 2025

; Ensure that the --arg-str CLI flag works for LLVM.

(defun @test ((s (Ptr 64))) Unit
  (start start:
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--arg-str", "%0:hello"}
;; go(prog)
;; check "Using precondition:"
;; check "%0: 000000+"
;; check "000000: 68 65 6c 6c 6f 00"
;; ok()
