; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
    (let b (store none i64 p p))
    (return ())))
