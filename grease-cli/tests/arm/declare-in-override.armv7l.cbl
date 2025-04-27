; Copyright (c) Galois, Inc. 2024

; A test case that demonstrates that forward declarations can be used in both
; Macaw S-expression programs and their overrides (gitlab#159).

; flag: --overrides tests/arm/extra/my-malloc.armv7l.cbl

; A forward declaration to a built-in override.
(declare @free ((ptr (Ptr 32))) Unit)

; A forward declaration to a user-supplied override.
(declare @my-malloc () (Ptr 32))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let p (funcall @my-malloc))
    (let forty-two (bv 32 42))
    (pointer-write (Bitvector 32) le p forty-two)
    (funcall @free p)
    (return regs)))
;; ok()
