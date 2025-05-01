; Copyright (c) Galois, Inc. 2024

; flags: --symbol test

(defun @test ((p (Ptr 64))) Unit
  (start start:
;; check "Skipped call to a symbolic function handle"
    (let h (load-handle Unit () p))
    (funcall h)
    (return ())))
;; ok()
