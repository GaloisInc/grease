; Copyright (c) Galois, Inc. 2025

; Ensure that `fresh-bytes` is registered.

;; flags {"--symbol", "test"}
;; go(prog)


(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 32))) (Vector (Bitvector 8)))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let _ (funcall @fresh-bytes "test" (bv 32 2)))
    (return regs)))

;; ok()
