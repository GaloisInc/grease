; Copyright (c) Galois, Inc. 2025

; Like the func-ptr.llvm.cbl test case, but where grease is instead invoked with
; --error-symbolic-fun-calls such that the call to a symbolic function handle
; should raise an error.

; flags: --error-symbolic-fun-calls

(defun @test ((p (Ptr 64))) Unit
  (start start:
;; check "Goal failed:"
;; check "Failed to load function handle"
;; check "Cannot resolve a symbolic pointer to a function handle"
    (let h (load-handle Unit () p))
    (funcall h)
    (return ())))
;; no_heuristic()
