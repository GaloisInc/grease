; Copyright (c) Galois, Inc. 2026

; Test pointer to pointer

(defun @inc ((x (Bitvector 32))) (Bitvector 32)
  (start start:
    (let result (+ x (bv 32 1)))
    (return result)))

(defun @test ((x (Ptr 64))) (Bitvector 32)
  (start start:
    ; First dereference: *x gives us a pointer
    (let ptr (load none i64 x))
    ; Second dereference: *ptr gives us the int value  
    (let val_ptr (load none i32 ptr))
    (let val (ptr-offset 32 val_ptr))
    ; Call inc on the value
    (let result (funcall @inc val))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
