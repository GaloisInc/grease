; Copyright (c) Galois, Inc. 2024

; A regression test for gitlab#157.
(declare @struct-override-callee () (Struct (Bitvector 32) (Bitvector 64)))

(defun @test () (Bitvector 32)
  (start start:
    (let s (funcall @struct-override-callee))
    (let ret (get-field 0 s))
    (return ret)))
; CHECK: All goals passed!
