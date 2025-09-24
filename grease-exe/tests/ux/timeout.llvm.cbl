; Copyright (c) Galois, Inc. 2025

; Test the UX of timeouts.

;; flags {"--symbol", "test"}
;; flags {"--loop-bound", "100000000"}
;; flags {"--timeout", "1"}
;; go(prog)

(defun @test () Unit
  (start start:
    (jump start:)))
;; check "Symbolic execution timed out!"
