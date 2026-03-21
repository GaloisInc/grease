; Copyright (c) Galois, Inc. 2026

; Test S-expression simulation of a call to a function defined in a shared
; library. @foo is declared but its implementation comes from --shared-lib.

(declare @foo () (Ptr 64))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (funcall @foo))
    (let v (pointer-read (Bitvector 32) le p))
    (return regs)))

; Currently: Function foo is declared but not defined (no shared lib support).
;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libfoo.x64.so"}
;; go(prog)
;; check("Global symbol \"foo\" has no associated allocation")
;; must_fail()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
; TODO(#21): ok()
