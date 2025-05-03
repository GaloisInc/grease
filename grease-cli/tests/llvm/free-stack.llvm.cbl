; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test () Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (let g (resolve-global "free"))
    (let h (load-handle Unit ((Ptr 64)) g))
;; check "`free` called on pointer that didn't point to a live region of the heap"
;; next_line_must_fail()
    (funcall h a)
    (return ())))
; TODO: This error message could be improved
;; check "The free function"
