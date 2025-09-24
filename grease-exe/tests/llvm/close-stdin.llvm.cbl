; Copyright (c) Galois, Inc. 2025

; Ensure that `close`ing `stdin` works. Regression test for #327.

;; flags {"--symbol", "test"}
;; go(prog)

; int close(int filedes)
(declare @close ((filedes (Ptr 32))) (Ptr 32))

(defun @test () Unit
  (start start:
    (let stdin (ptr 32 0 (bv 32 0)))
    (let closed (funcall @close stdin))
    (let closed-off (ptr-offset 32 closed))
    (assert! (not (equal? closed-off (bv 32 0xffffffff))) "close() returned -1!")
    (return ())))

;; ok()
