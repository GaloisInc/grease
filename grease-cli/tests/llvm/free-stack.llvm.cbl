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
; CHECK: Likely bug: unavoidable error
; TODO: This error message could be improved
; CHECK: The free function
