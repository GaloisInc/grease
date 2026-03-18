; Copyright (c) Galois, Inc. 2026

; This is a simplified version of the division semantics test.

(declare @malloc ((size (Bitvector 64))) (Ptr 64))
(declare @free ((ptr (Ptr 64))) Unit)

(defun @foo ((p (Ptr 64)) (x_ptr (Ptr 64))) Unit
  (start start:
    (let x_val_ptr (load none i32 x_ptr))
    (let x (ptr-offset 32 x_val_ptr))
    ; Check if x == 2
    (let is_2 (equal? x (bv 32 2)))
    (branch is_2 free1: check2:))
  (defblock free1:
    (funcall @free p)
    (jump check2:))
  (defblock check2:
    ; Check if x == 3
    (let is_3 (equal? x (bv 32 3)))
    (branch is_3 free2: done:))
  (defblock free2:
    (funcall @free p)
    (jump done:))
  (defblock done:
    (return ())))

(defun @test () Unit
  (start start:
    (let size (bv 64 8))
    (let p (funcall @malloc size))
    ; Allocate and store x = 0x01
    (let x_ptr (alloca none (bv 64 4)))
    (let x_val (ptr 32 0 (bv 32 1)))
    (store none i32 x_ptr x_val)
    ; Call foo(p, 0x01)
    (funcall @foo p x_ptr)
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
