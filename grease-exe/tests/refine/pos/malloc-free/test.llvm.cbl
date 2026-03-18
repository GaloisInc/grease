; Copyright (c) Galois, Inc. 2026

; Test malloc and free with memory operations

(declare @malloc ((size (Bitvector 64))) (Ptr 64))
(declare @free ((ptr (Ptr 64))) Unit)

(defun @test () (Bitvector 32)
  (start start:
    ; Allocate sizeof(int) = 4 bytes
    (let size (bv 64 4))
    (let p (funcall @malloc size))
    ; Create a pointer with 42 as the offset and store it
    (let forty_two_ptr (ptr 32 0 (bv 32 42)))
    (store none i32 p forty_two_ptr)
    ; Load back from *p
    (let x_ptr (load none i32 p))
    (let x (ptr-offset 32 x_ptr))
    ; Free the memory
    (funcall @free p)
    ; Return the value
    (return x)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
