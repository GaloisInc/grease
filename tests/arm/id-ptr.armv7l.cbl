; Copyright (c) Galois, Inc. 2024

; This definition is used as an override in the user-override.armv7l.cbl
; test case.
(defun @id-ptr ((p (Ptr 32))) (Ptr 32)
  (start start:
    (return p)))

(defun @test ((p (Ptr 32))) (Ptr 32)
  (start start:
    (let p2 (funcall @id-ptr p))
    (return p)))
