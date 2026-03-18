; Copyright (c) Galois, Inc. 2026

; Test malloc allocation

(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @test () (Ptr 64)
  (start start:
    (let size (bv 64 1))
    (let ptr (funcall @malloc size))
    (return ptr)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
