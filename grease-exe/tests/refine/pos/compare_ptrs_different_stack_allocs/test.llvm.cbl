; Copyright (c) Galois, Inc. 2026

; Test comparing pointers to different stack allocations

(defun @compare ((x (Bitvector 64)) (y (Bitvector 64))) (Bitvector 64)
  (start start:
    (let is_less (< x y))
    (let result (if is_less (bv 64 1) (bv 64 0)))
    (return result)))

(defun @test () (Bitvector 64)
  (start start:
    (let x (alloca none (bv 64 8)))
    (let y (alloca none (bv 64 8)))
    (let x_offset (ptr-offset 64 x))
    (let y_offset (ptr-offset 64 y))
    (let result (funcall @compare x_offset y_offset))
    (return result)))

;; flags {"--symbol", "test"}
;; go(prog)
;; ok()
