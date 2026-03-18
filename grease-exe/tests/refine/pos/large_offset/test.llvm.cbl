; Copyright (c) Galois, Inc. 2026

; Test large array offset

(defun @test ((x (Ptr 64))) (Bitvector 32)
  (start start:
    ; x[4096] = *(x + 4096 * 4) = *(x + 16384 bytes)
    (let offset (bv 64 16384))
    (let indexed_ptr (ptr-add-offset x offset))
    (let val_ptr (load none i32 indexed_ptr))
    (let val (ptr-offset 32 val_ptr))
    (return val)))

;; flags {"--symbol", "test"}
;; flags {"--iters", "8"}
;; go(prog)
;; ok()
