; Copyright (c) Galois, Inc. 2024

; This definition is used as an override in the user-override.x64.cbl
; test case.
(defun @id-ptr ((p (Ptr 64))) (Ptr 64)
  (start start:
    (return p)))

(defun @test ((p (Ptr 64))) (Ptr 64)
  (start start:
    (let p2 (funcall @id-ptr p))
    (return p)))
