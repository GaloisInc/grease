; Copyright (c) Galois, Inc. 2024

(defun @struct-override-callee () (Struct (Bitvector 32) (Bitvector 64))
  (start start:
    (return (struct (bv 32 0) (bv 64 0)))))
; CHECK: All goals passed!
