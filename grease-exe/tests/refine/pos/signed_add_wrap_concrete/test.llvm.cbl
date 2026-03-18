; Copyright (c) Galois, Inc. 2026

; Test signed integer overflow with concrete values

(defun @add_max_int_minus_one ((x (Bitvector 32))) (Bitvector 32)
  (start start:
    (let max_minus_one (bv 32 2147483646))
    (let result (+ x max_minus_one))
    (return result)))

(defun @test ((x_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    (let x_val_ptr (load none i32 x_ptr))
    (let x (ptr-offset 32 x_val_ptr))
    (let is_nonzero (not (equal? x (bv 32 0))))
    (branch is_nonzero then: else:))
  (defblock then:
    (let result (funcall @add_max_int_minus_one (bv 32 4)))
    (return result))
  (defblock else:
    (return (bv 32 0))))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
