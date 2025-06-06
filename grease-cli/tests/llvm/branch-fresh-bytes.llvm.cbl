; Copyright (c) Galois, Inc. 2025

; Ensure that concretized bytes from calls to `fresh-bytes` are not shown in the
; output for bugs on other branches.

;; flags {"--symbol", "test"}
;; go(prog)

(declare @abort () Unit)
(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let off (ptr-offset 64 p))
    (branch (equal? off (bv 64 0)) if: else:))
  (defblock if:
    (funcall @fresh-bytes "test" (bv 64 2))
    (return ()))
  (defblock else:
    (funcall @abort)
    (return ())))

; TODO: Check that the bytes are *not* in the output
;; check "Call to abort"
