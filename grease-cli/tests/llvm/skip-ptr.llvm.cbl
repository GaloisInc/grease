; Copyright (c) Galois, Inc. 2024

(declare @skippable () (Ptr 64))
(defun @test () (Ptr 64)
  (start start:
    (let p (funcall @skippable))
    (return p)))
