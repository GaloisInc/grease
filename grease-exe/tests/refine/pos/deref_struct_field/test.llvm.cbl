; Copyright (c) Galois, Inc. 2026

; Test struct field access

(defun @test ((pt (Ptr 64))) (Bitvector 32)
  (start start:
    ; pt->x is at offset 0, so just load from pt
    (let x_ptr (load none i32 pt))
    (let x (ptr-offset 32 x_ptr))
    (return x)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
