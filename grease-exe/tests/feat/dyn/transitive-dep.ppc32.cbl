; Copyright (c) Galois, Inc. 2026

; PPC32 variant of transitive-dep.x64.cbl; see that file for details.

(declare @bar () (Ptr 32))

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (let p (funcall @bar))
    (let v (pointer-read (Bitvector 32) be p))
    (return regs)))

;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libbar.ppc32.so"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libfoo.ppc32.so"}
;; go(prog)
;; check("Global symbol \"bar\" has no associated allocation")
;; must_fail()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libbar")
; TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
; TODO(#21): ok()
