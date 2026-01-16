; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test () Unit
  (start start:
    (let off0 (bv 64 1))
    (let g (resolve-global "malloc"))
    (let gblk (ptr-block 64 g))
    (assert! (not (equal? gblk 0)) "malloc block number nonzero")
    (let h (load-handle (Ptr 64) ((Bitvector 64)) g))
    (let m (funcall h off0))
    (let mblk (ptr-block 64 m))
    (assert! (not (equal? mblk 0)) "malloc'd block number nonzero")

    (return ())))
;; ok()
