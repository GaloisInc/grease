; Copyright (c) Galois, Inc. 2026

; Test user-provided override

(declare @shouldnt_loop () Unit)

(defun @test () Unit
  (start start:
    ; Call shouldnt_loop which is overridden to return immediately
    (funcall @shouldnt_loop)
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--overrides", "tests/refine/pos/user_override/shouldnt_loop.aux.cbl"}
;; go(prog)
;; ok()
