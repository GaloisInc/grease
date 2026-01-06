; Copyright (c) Galois, Inc. 2025

; Test --skip with a bogus function name in LLVM S-expression files.

(defun @test () Unit
  (start start:
    (return ())))

;; flags {"--skip", "bogus"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
