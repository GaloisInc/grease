; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
;; next_line_must_fail()
    (funcall h)
    (return ())))
;; check "Call to abort"
