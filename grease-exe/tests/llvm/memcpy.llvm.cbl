; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((dst (Ptr 64)) (src (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "memcpy"))
    (let h (load-handle (Ptr 64) ((Ptr 64) (Ptr 64) (Bitvector 64)) g))
    (let sz (bv 64 4))
    (let _ (funcall h dst src sz))
    (return ())))
;; ok()
