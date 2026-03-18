; Copyright (c) Galois, Inc. 2026

; Test recursive factorial - should not be able to infer precondition

(defun @test ((x_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    (let x_val_ptr (load none i32 x_ptr))
    (let x (ptr-offset 32 x_val_ptr))
    (let is_zero (equal? x (bv 32 0)))
    (branch is_zero base: recursive:))
  (defblock base:
    (return (bv 32 1)))
  (defblock recursive:
    (let x_minus_1 (- x (bv 32 1)))
    ; Allocate stack space for x_minus_1 and store it
    (let x_minus_1_ptr (alloca none (bv 64 4)))
    (let x_minus_1_val_ptr (ptr 32 0 x_minus_1))
    (store none i32 x_minus_1_ptr x_minus_1_val_ptr)
    ; Recursively call with the new pointer
    (let factorial_rec (funcall @test x_minus_1_ptr))
    (let result (* x factorial_rec))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; could_not_infer()
