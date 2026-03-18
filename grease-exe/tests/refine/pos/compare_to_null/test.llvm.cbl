; Copyright (c) Galois, Inc. 2026

; Test comparing a pointer to null

(defun @test ((ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; Check if pointer is null by checking block and offset
    (let blk (ptr-block 64 ptr))
    (let off (ptr-offset 64 ptr))
    (let zblk (equal? 0 blk))
    (let zoff (equal? (bv 64 0) off))
    (let is_null (and zblk zoff))
    (let result (if is_null (bv 32 1) (bv 32 0)))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
