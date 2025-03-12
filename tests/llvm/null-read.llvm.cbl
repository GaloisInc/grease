; Copyright (c) Galois, Inc. 2024

(defun @test () (Ptr 8)
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
    (let b (load none i8 p))
    (return b)))
