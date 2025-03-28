; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let blk (the Nat 0))
    (let off (bv 8 0))
    (let v (ptr 8 blk off))
    (store none i8 p v)
    (return ())))
; CHECK: All goals passed!
; CHECK: Final refined precondition:
; CHECK-EMPTY:
; CHECK: %0: 000000+0000000000000000
; CHECK-EMPTY:
; CHECK: 000000: ##
