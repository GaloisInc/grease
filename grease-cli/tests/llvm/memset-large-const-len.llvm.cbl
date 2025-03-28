; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "memset"))
    (let h (load-handle (Ptr 64) ((Ptr 64) (Ptr 32) (Ptr 64)) g))
    (let c (ptr 32 0 (bv 32 0)))
    (let s (ptr 64 0 (bv 64 4096)))
    (let _ (funcall h p c s))
    (return ())))
; CHECK: All goals passed!
