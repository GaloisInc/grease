; Copyright (c) Galois, Inc. 2025

; Test --skip for LLVM S-expression files.

;; flags {"--skip", "bug"}
;; flags {"--symbol", "test"}
;; go(prog)

(declare @abort () Unit)

(defun @bug () Unit
  (start start:
    (funcall @abort)
    (return ())))

(defun @test () Unit
  (start start:
    (funcall @bug)
    (return ())))

;; ok()
