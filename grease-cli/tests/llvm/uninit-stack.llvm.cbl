; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (load none i8 a)
    (return ())))
; CHECK: Likely bug: uninitialized stack read
; CHECK-NEXT: Allocated at
