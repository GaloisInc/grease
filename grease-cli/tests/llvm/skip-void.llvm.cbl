; Copyright (c) Galois, Inc. 2024

(declare @skippable () Unit)
(defun @test ((b (Ptr 1))) (Ptr 1)
  (start start:
;; check "Invoking the 'skippable' function"
    (let _ (funcall @skippable))
    (return b)))
;; ok()
