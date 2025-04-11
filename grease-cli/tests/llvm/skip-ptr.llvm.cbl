; Copyright (c) Galois, Inc. 2024

(declare @skippable () (Ptr 64))
(defun @test () (Ptr 64)
  (start start:
    ; CHECK: Invoking the 'skippable' function
    (let p (funcall @skippable))
    (return p)))
;; ok()
