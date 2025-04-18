; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "free"))
    (let h (load-handle Unit ((Ptr 64)) g))
    (funcall h p)
;; next_line_must_fail()
    (funcall h p)
    (return ())))
; TODO: This error mesage is not great... it doesn't mention double-frees
;; check "The free function"
