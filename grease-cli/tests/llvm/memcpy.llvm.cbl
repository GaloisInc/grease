; Copyright (c) Galois, Inc. 2024

(defun @test ((dst (Ptr 64)) (src (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "memcpy"))
    (let h (load-handle (Ptr 64) ((Ptr 64) (Ptr 64) (Ptr 64)) g))
    (let sz (ptr 64 0 (bv 64 4)))
    (let _ (funcall h dst src sz))
    (return ())))
