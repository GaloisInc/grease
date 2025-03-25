; Copyright (c) Galois, Inc. 2024

; A test case that demonstrates that forward declarations can be used in both
; Macaw S-expression programs and their overrides (gitlab#159).

; A forward declaration to a built-in override.
(declare @free ((ptr (Ptr 64))) Unit)

; A forward declaration to a user-supplied override.
(declare @my-malloc () (Ptr 64))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (funcall @my-malloc))
    (let forty-two (bv 64 42))
    (pointer-write (Bitvector 64) le p forty-two)
    (funcall @free p)
    (return regs)))
