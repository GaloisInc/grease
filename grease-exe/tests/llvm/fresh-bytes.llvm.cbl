; Copyright (c) Galois, Inc. 2025

; Ensure that `fresh-bytes` is registered.

;; flags {"--symbol", "test"}
;; go(prog)


(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

(defun @test () (Vector (Bitvector 8))
  (start start:
    (let v (funcall @fresh-bytes "test" (bv 64 2)))
    (return v)))

;; ok()
