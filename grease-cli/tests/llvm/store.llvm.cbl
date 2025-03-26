; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let blk (the Nat 0))
    (let off (bv 8 0))
    (let v (ptr 8 blk off))
    (store none i8 p v)
    (return ())))