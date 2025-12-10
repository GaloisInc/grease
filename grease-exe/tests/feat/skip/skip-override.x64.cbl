; Copyright (c) Galois, Inc. 2025

; Test --skip for user overrides in Macaw S-expression files.

(declare @bug () Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @bug)
    (return regs)))

;; flags {"--overrides", "tests/feat/skip/bug.aux.x64.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()

;; flags {"--skip", "bug"}
;; flags {"--overrides", "tests/feat/skip/bug.aux.x64.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
