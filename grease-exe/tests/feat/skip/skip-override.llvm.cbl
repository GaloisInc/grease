; Copyright (c) Galois, Inc. 2025

; Test --skip for user overrides in LLVM S-expression files.

(declare @bug () Unit)

(defun @test () Unit
  (start start:
    (funcall @bug)
    (return ())))

;; flags {"--overrides", "tests/feat/skip/bug.aux.llvm.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()

;; flags {"--skip", "bug"}
;; flags {"--overrides", "tests/feat/skip/bug.aux.llvm.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
