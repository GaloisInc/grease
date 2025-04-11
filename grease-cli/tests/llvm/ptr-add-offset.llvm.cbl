; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) (Ptr 64)
  (start start:
    (let one (bv 64 1))
    (let q (ptr-add-offset p one))
    (return q)))
;; ok()
