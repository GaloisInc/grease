; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let g (resolve-global "free"))
    (let h (load-handle Unit ((Ptr 64)) g))
    (let blk0 (the Nat 1))
    (let off0 (bv 64 0))
    (let p0 (ptr 64 blk0 off0))
    (funcall h p0)
    (return ())))
