; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

; Ensure that GREASE properly overrides `llvm.trap`, causing inference failure.
; If it were instead skipped, then inference would succeed.
(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "llvm.trap"))
    (let h (load-handle Unit () g))
;; next_line_must_fail()
    (funcall h)
    (return ())))
;; check "llvm.trap() called"
