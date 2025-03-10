; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let h (load-handle Unit () p))
    (funcall h)
    (return ())))
