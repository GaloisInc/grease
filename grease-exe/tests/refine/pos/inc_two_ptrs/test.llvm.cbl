; Copyright (c) Galois, Inc. 2026

; Test multiple pointer arguments

(defun @inc ((x (Bitvector 32))) (Bitvector 32)
  (start start:
    (let result (+ x (bv 32 1)))
    (return result)))

(defun @test ((x (Ptr 64)) (y (Ptr 64))) (Bitvector 32)
  (start start:
    (let val_x_ptr (load none i32 x))
    (let val_x (ptr-offset 32 val_x_ptr))
    (let inc_x (funcall @inc val_x))
    (let val_y_ptr (load none i32 y))
    (let val_y (ptr-offset 32 val_y_ptr))
    (let inc_y (funcall @inc val_y))
    (let result (+ inc_x inc_y))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
