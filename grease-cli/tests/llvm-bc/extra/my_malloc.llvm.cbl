; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Ptr 64))) (Ptr 64))

(defun @my_malloc () (Ptr 64)
  (start start:
    (let i64-size-offset (bv 64 8))
    (let i64-size (ptr 64 0 i64-size-offset))
    (let p (funcall @malloc i64-size))
    (return p)))
