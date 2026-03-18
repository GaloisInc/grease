; Copyright (c) Galois, Inc. 2026

; Test dereferencing a pointer and incrementing the value

(defun @inc ((x (Bitvector 32))) (Bitvector 32)
  (start start:
    (let result (+ x (bv 32 1)))
    (return result)))

(defun @test ((x (Ptr 64))) (Bitvector 32)
  (start start:
    (let val-ptr (load none i32 x))
    (let val (ptr-offset 32 val-ptr))
    (let result (funcall @inc val))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
