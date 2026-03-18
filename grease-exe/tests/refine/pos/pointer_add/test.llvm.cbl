; Copyright (c) Galois, Inc. 2026

; Test integer addition (regression test for gitlab#232)

(defun @test ((a_ptr (Ptr 64)) (b_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    (let a_val_ptr (load none i32 a_ptr))
    (let a (ptr-offset 32 a_val_ptr))
    (let b_val_ptr (load none i32 b_ptr))
    (let b (ptr-offset 32 b_val_ptr))
    (let result (+ a b))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
