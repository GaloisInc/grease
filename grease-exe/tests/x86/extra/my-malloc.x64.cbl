; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @my-malloc () (Ptr 64)
  (start start:
    (let i64-size (bv 64 8))
    (let p (funcall @malloc i64-size))
    (return p)))
