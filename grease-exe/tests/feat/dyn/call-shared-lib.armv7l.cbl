; Copyright (c) Galois, Inc. 2026

; ARM variant of call-shared-lib.x64.cbl; see that file for details.

(declare @foo () (Ptr 32))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let p (funcall @foo))
    (let v (pointer-read (Bitvector 32) le p))
    (return regs)))

;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libfoo.armv7l.so"}
;; go(prog)
;; check("Global symbol \"foo\" has no associated allocation")
;; must_fail()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
; TODO(#21): ok()
