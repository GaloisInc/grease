; Copyright (c) Galois, Inc. 2025

; Ensure that DFS and SSE report the same overall result when one path hits
; a possible bug and the other cannot be refined (in this case, due to the loop
; bound).
;
; Regression test for #476.

(declare @abort () Unit)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let off (ptr-offset 64 p))
    (branch (equal? off (bv 64 0)) if: else:))
  (defblock if:
    (funcall @abort)
    (return ()))
  (defblock else:
    (jump else:)))

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "sse"}
;; go(prog)
;; could_not_infer()

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "dfs"}
;; go(prog)
; TODO(#476):
; could_not_infer()
;; check 'reached maximum number of loop iterations'
