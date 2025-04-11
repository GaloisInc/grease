; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) (Ptr 64)
  (start start:
    (let kb (bv 64 4096))
    (let q (ptr-add-offset p kb))
    (return q)))
;; ok()
