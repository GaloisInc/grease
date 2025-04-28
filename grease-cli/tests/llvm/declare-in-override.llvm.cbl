; Copyright (c) Galois, Inc. 2024

; A test case that demonstrates that forward declarations can be used in both
; LLVM S-expression programs and their overrides (gitlab#159).

; flags: --overrides tests/llvm/extra/my-malloc.llvm.cbl

; A forward declaration to a built-in override.
(declare @free ((ptr (Ptr 64))) Unit)

; A forward declaration to a user-supplied override.
(declare @my-malloc () (Ptr 64))

(defun @test () Unit
  (start start:
    (let p (funcall @my-malloc))
    (let forty-two-offset (bv 64 42))
    (let forty-two (ptr 64 0 forty-two-offset))
    (store none i64 p forty-two)
    (funcall @free p)
    (return ())))
;; ok()
