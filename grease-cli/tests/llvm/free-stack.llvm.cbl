; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (let g (resolve-global "free"))
    (let h (load-handle Unit ((Ptr 64)) g))
    (funcall h a)
    (return ())))
