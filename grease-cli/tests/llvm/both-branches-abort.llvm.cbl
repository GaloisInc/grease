; This test-case branches on data in its arguments. In both branches, it calls
; `abort`. This pattern should be recognized by the "one must fail" heuristic.

(defun @test ((b Bool)) Unit
  (registers
    ($h (-> Unit)))
  (start start:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (set-register! $h h)
    (branch b if: else:))
  (defblock if:
    (funcall $h)
    (return ()))
  (defblock else:
    (funcall $h)
    (return ())))
; CHECK: Likely bug: at least one bug occurs
