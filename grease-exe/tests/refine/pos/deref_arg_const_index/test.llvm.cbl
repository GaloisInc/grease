; Copyright (c) Galois, Inc. 2026

; Test array indexing with constant offset

(defun @test ((ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; ptr[8] means *(ptr + 8), where the offset is in elements (8 * 4 bytes = 32 bytes)
    (let offset (bv 64 32))
    (let indexed_ptr (ptr-add-offset ptr offset))
    (let val_ptr (load none i32 indexed_ptr))
    (let val (ptr-offset 32 val_ptr))
    (return val)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
