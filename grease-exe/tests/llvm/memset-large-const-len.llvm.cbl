; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "memset"))
    (let h (load-handle (Ptr 64) ((Ptr 64) (Bitvector 32) (Bitvector 64)) g))
    (let c (bv 32 0))
    (let s (bv 64 4096))
    (let _ (funcall h p c s))
    (return ())))
;; ok()
