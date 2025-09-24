; Copyright (c) Galois, Inc. 2025

; Ensure that the S-expression-specific string-manipulation overrides work.

;; flags {"--symbol", "test"}
;; go(prog)

(declare @read-bytes ((x (Ptr 64))) (Vector (Bitvector 8)))
(declare @read-c-string ((x (Ptr 64))) (String Unicode))
(declare @write-bytes ((dest (Ptr 64)) (src (Vector (Bitvector 8)))) Unit)
(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)

(defun @test () Unit
  (start start:

    (let p (alloca none (bv 64 6)))
    (funcall @write-c-string p "hello")
    (let s (funcall @read-c-string p))
    (assert! (equal? s "hello") "strings should be equal")

    (let q (alloca none (bv 64 4)))
    (let v (vector (bv 8 4) (bv 8 9) (bv 8 0)))
    (funcall @write-bytes q v)
    (let b (funcall @read-bytes p))

    (return ())))

;; ok()
