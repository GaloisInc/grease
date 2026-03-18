; Copyright (c) Galois, Inc. 2026

; Test writing to null pointer

(defun @make_it_five ((ptr (Ptr 64))) Unit
  (start start:
    (let val_ptr (ptr 32 0 (bv 32 5)))
    (store none i32 ptr val_ptr)
    (return ())))

(defun @test () Unit
  (start start:
    (let null_ptr (ptr 64 0 (bv 64 0)))
    (funcall @make_it_five null_ptr)
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()
;; check [[
;; The region wasn't allocated, wasn't large enough, or was marked as readonly
;; Performing store at type: i32
;;   Via pointer: 0x0:[64]
;; ]]
