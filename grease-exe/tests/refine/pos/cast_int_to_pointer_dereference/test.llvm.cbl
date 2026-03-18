; Copyright (c) Galois, Inc. 2026

; Test casting integer to pointer  

(defun @test ((ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; Dereference the pointer directly
    (let val_ptr (load none i32 ptr))
    (let val (ptr-offset 32 val_ptr))
    (return val)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
