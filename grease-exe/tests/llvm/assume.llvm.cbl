; Copyright (c) Galois, Inc. 2025

; Regression test for #52, wherein the must-fail heuristic is not triggered in
; the presence of `assume!`s.

(declare @abort () Unit)

;; flags {"--symbol", "test-good"}
;; go(prog)
(defun @test-good ((b Bool)) Unit
  (start start:
;; next_line_must_fail()
    (funcall @abort)
    (return ())))

; TODO(#52): This should also trigger must-fail.
;; flags {"--symbol", "test-bad"}
;; go(prog)
(defun @test-bad ((b Bool)) Unit
  (start start:
    (assume! b "assuming b")
    (funcall @abort)
    (return ())))
;; no_heuristic()

