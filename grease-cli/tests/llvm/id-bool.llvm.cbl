; Copyright (c) Galois, Inc. 2024

; This definition is used as an override in the user-override.llvm.cbl
; test case.
(defun @id-bool ((b (Ptr 1))) (Ptr 1)
  (start start:
    (return b)))

(defun @test ((b (Ptr 1))) (Ptr 1)
  (start start:
    (let b2 (funcall @id-bool b))
    (return b2)))
; CHECK: All goals passed!
