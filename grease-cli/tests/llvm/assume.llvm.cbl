; Copyright (c) Galois, Inc. 2025

(declare @abort () Unit)

;; flags {"--symbol", "test-good"}
;; go(prog)
(defun @test-good ((b Bool)) Unit
  (start start:
;; next_line_must_fail()
    (funcall @abort)
    (return ())))

;; flags {"--symbol", "test-bad"}
;; go(prog)
(defun @test-bad ((b Bool)) Unit
  (start start:
    (assume! b "assuming b")
    (funcall @abort)
    (return ())))
;; no_heuristic()

