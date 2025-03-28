; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ())))
; CHECK: Likely bug: unavoidable error
; CHECK: Call to abort
