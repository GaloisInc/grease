; Copyright (c) Galois, Inc. 2024

(defun @test () Unit
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
    (let b (store none i64 p p))
    (return ())))
; CHECK:      Likely bug: unavoidable error
; CHECK-NEXT: The region wasn't allocated, wasn't large enough, or was marked as readonly
; CHECK:      Performing store at type: i64
; CHECK-NEXT:   Via pointer: 0x0:[64]
