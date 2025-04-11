; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let g (resolve-global "free"))
    (let h (load-handle Unit ((Ptr 64)) g))
    (let blk0 (the Nat 1))
    (let off0 (bv 64 0))
    (let p0 (ptr 64 blk0 off0))
    ; CHECK: `free` called on pointer that didn't point to a live region of the heap
    (funcall h p0)
    (return ())))
;; must_fail()
; TODO: This error message could be improved
;; check "The free function"
