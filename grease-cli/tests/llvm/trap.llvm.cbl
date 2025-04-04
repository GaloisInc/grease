; Copyright (c) Galois, Inc. 2024

; Ensure that GREASE properly overrides `llvm.trap`, causing inference failure.
; If it were instead skipped, then inference would succeed.
(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let g (resolve-global "llvm.trap"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ())))
; CHECK: Likely bug: unavoidable error
; CHECK: llvm.trap() called
