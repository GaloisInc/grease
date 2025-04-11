; Copyright (c) Galois, Inc. 2024

; A variant of user-override.llvm.cbl where @id-bool is defun'd rather than
; declare'd. This serves as a regression test for gitlab#156.
(defun @id-bool ((b (Ptr 1))) (Ptr 1)
  (start start:
    (assert! #f "should be overridden")
    (let z (ptr 1 0 (bv 1 0)))
    (return z)))
(defun @test ((b (Ptr 1))) (Ptr 1)
  (start start:
    (let b2 (funcall @id-bool b))
    (return b2)))
;; ok()
