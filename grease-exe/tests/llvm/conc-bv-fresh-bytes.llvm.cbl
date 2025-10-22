; Copyright (c) Galois, Inc. 2025

; Ensure that `conc-bv-64` really concretizes its argument by passing its return
; value to `fresh-bytes`, which will abort if not passed a concrete bitvector.

;; flags {"--symbol", "test"}
;; go(prog)

(declare @abort () Unit)
(declare @conc-bv-64 ((b (Bitvector 64))) (Bitvector 64))
(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

(defun @test () Unit
  (start start:
    (let b0 (fresh (Bitvector 64)))
    (let z (bv 64 0x0))
    (assume! (equal? b0 z) "assuming b0 == 0")
    (let b (funcall @conc-bv-64 b0))
    (funcall @fresh-bytes "whatever" b)
    (return ())))

;; ok()
