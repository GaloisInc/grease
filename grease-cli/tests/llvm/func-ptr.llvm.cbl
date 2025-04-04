; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    ; CHECK: Skipped call to a symbolic function handle
    (let h (load-handle Unit () p))
    (funcall h)
    (return ())))
; CHECK: All goals passed!
