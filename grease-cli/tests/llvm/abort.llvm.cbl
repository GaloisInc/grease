; Copyright (c) Galois, Inc. 2024

; CHECK: Likely bug: unavoidable error
; CHECK: Call to abort
(defun @test () Unit
  (start start:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ())))
