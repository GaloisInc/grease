; Copyright (c) Galois, Inc. 2024

(defun @test () (Ptr 8)
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
    (let b (load none i8 p))
    (return b)))
; CHECK:      Likely bug: unavoidable error
; CHECK-NEXT: No previous write to this location was found
; CHECK:      Attempting load at type: i8
; CHECK-NEXT: Performing overall load at type: i8
; CHECK:      Via pointer: 0x0:[64]
