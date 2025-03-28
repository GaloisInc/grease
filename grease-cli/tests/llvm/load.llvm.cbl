; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (load none i8 p)
    (return ())))
; CHECK: All goals passed!
; CHECK: Final refined precondition:
; CHECK-EMPTY:
; CHECK: %0: 000000+0000000000000000
; CHECK-EMPTY:
; CHECK: 000000: XX
