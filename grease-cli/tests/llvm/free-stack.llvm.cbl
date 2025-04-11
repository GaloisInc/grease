; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (let g (resolve-global "free"))
    ; CHECK: `free` called on pointer that didn't point to a live region of the heap
    (let h (load-handle Unit ((Ptr 64)) g))
    (funcall h a)
    (return ())))
;; must_fail()
; TODO: This error message could be improved
;; check "The free function"
