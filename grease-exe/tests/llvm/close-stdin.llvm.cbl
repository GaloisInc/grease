; Copyright (c) Galois, Inc. 2025

; Ensure that `close`ing `stdin` works. Regression test for #327.

;; flags {"--symbol", "test"}
;; go(prog)

; int close(int filedes)
(declare @close ((filedes (Bitvector 32))) (Bitvector 32))

(defun @test () Unit
  (start start:
    (let stdin (bv 32 0))
    (let closed (funcall @close stdin))
    (assert! (not (equal? closed (bv 32 0xffffffff))) "close() returned -1!")
    (return ())))

;; ok()
