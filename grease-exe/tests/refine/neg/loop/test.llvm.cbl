; Copyright (c) Galois, Inc. 2026

; Test loop - should not be able to infer precondition

(defun @test ((n_ptr (Ptr 64))) (Bitvector 32)
  (registers
    ($sum (Bitvector 32))
    ($i (Bitvector 32)))
  (start start:
    (let n_val_ptr (load none i32 n_ptr))
    (let n (ptr-offset 32 n_val_ptr))
    (set-register! $sum (bv 32 0))
    (set-register! $i (bv 32 0))
    (jump loop:))
  (defblock loop:
    (let cond (< $i n))
    (branch cond body: done:))
  (defblock body:
    (let new_sum (+ $sum $i))
    (set-register! $sum new_sum)
    (let new_i (+ $i (bv 32 1)))
    (set-register! $i new_i)
    (jump loop:))
  (defblock done:
    (return $sum)))

;; flags {"--symbol", "test"}
;; go(prog)
;; could_not_infer()
