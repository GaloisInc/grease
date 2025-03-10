; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Bitvector 32))) (Ptr 32))

(defun @my-malloc () (Ptr 32)
  (start start:
    (let i32-size (bv 32 4))
    (let p (funcall @malloc i32-size))
    (return p)))
