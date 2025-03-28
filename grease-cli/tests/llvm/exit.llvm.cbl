; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let g (resolve-global "exit"))
    (let h (load-handle Unit ((Ptr 32)) g))
	(let p (ptr 32 0 (bv 32 0)))
    (funcall h p)
    (return ())))
; CHECK: All goals passed!
