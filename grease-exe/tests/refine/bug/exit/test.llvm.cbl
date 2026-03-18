; Copyright (c) Galois, Inc. 2026

; Test exit() call

(declare @exit ((status (Bitvector 32))) Unit)

(defun @test () Unit
  (start start:
    (let exit_failure (bv 32 1))
    (funcall @exit exit_failure)
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()
