; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "memset"))
    (let h (load-handle (Ptr 64) ((Ptr 64) (Ptr 32) (Ptr 64)) g))
    (let c (ptr 32 0 (bv 32 0)))
    (let s (ptr 64 0 (bv 64 1)))
    (let _ (funcall h p c s))

    ; assert that GREASE did actually perform a memset
    (let v (load none i8 p))
    (let blk (ptr-block 8 v))
    (let off (ptr-offset 8 v))
    (assert! (equal? blk 0) "block numbers equal")
    (assert! (equal? off (bv 8 0)) "offset numbers equal")

    (return ())))
;; ok()
