; Copyright (c) Galois, Inc. 2025

; Test --skip for `@declare`s of built-in overrides LLVM S-expression files.

(declare @abort () Unit)

(defun @test () Unit
  (start start:
    (funcall @abort)
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()

;; flags {"--skip", "abort"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
