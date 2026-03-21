; Copyright (c) Galois, Inc. 2026

; Test that built-in overrides (like malloc) take precedence over shared library
; exports. libbar.so exports malloc that returns NULL, but grease's built-in
; malloc override should be used instead, allowing the test to succeed.

; libbar exports malloc returning NULL. Built-in override should win.
(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let sz (bv 64 8))
    (let p (funcall @malloc sz))
    (let forty-two (bv 64 42))
    (pointer-write (Bitvector 64) le p forty-two)
    (let v (pointer-read (Bitvector 64) le p))
    (return regs)))

;; flags {"--symbol", "test"}
; TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libbar.x64.so"}
;; go(prog)
;; check("<malloc> tests/feat/dyn/override-precedence")
;; ok()
; TODO(#21): check("Loaded shared library tests/feat/dyn/libbar")
