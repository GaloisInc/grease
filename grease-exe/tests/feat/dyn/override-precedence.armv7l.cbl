; Copyright (c) Galois, Inc. 2026

; ARM variant of override-precedence.x64.cbl; see that file for details.
(declare @malloc ((size (Bitvector 32))) (Ptr 32))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let sz (bv 32 8))
    (let p (funcall @malloc sz))
    (let forty-two (bv 32 42))
    (pointer-write (Bitvector 32) le p forty-two)
    (let v (pointer-read (Bitvector 32) le p))
    (return regs)))

;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libbar.armv7l.so"}
;; go(prog)
;; check("<malloc> tests/feat/dyn/override-precedence")
;; ok()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libbar")
