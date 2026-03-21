; Copyright (c) Galois, Inc. 2026

; Test Crucible-level simulation with transitive shared library dependencies.
; Function bar is declared and calls foo internally. Both implementations come
; from external --shared-lib flags, verifying that Crucible handles transitive
; dependencies (libbar.so depends on libfoo.so).

(declare @bar () (Ptr 64))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (funcall @bar))
    (let v (pointer-read (Bitvector 32) le p))
    (return regs)))

; Currently: Function bar is declared but not defined (no shared lib support).
;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libbar.x64.so"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libfoo.x64.so"}
;; go(prog)
;; check("Global symbol \"bar\" has no associated allocation")
;; must_fail()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libbar")
; TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
; TODO(#21): ok()
