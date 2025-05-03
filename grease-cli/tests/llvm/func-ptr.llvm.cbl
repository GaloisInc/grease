; Copyright (c) Galois, Inc. 2024

; Test the behavior of calls through symbolic function pointers. By default,
; such calls are simply skipped, but --error-symbolic-fun-calls turns them
; into a hard error.

;; flags {"--symbol", "test"}
;; go(prog)
;; check "Skipped call to a symbolic function handle"
;; ok()

;; flags {"--symbol", "test"}
;; flags {"--error-symbolic-fun-calls"}
;; go(prog)
;; check "Goal failed:"
;; check "Failed to load function handle"
;; check "Cannot resolve a symbolic pointer to a function handle"
;; no_heuristic()

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let h (load-handle Unit () p))
    (funcall h)
    (return ())))
