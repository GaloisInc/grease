; Copyright (c) Galois, Inc. 2024

(declare @id-bool ((p (Ptr 1))) (Ptr 1))
(defun @test ((b (Ptr 1))) (Ptr 1)
  (start start:
    (let b2 (funcall @id-bool b))
    (return b2)))
; CHECK: All goals passed!
