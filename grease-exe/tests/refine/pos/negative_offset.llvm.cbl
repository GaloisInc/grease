; Copyright (c) Galois, Inc. 2026

; Test that the heuristic correctly handles loading from a negative offset
; of a pointer argument by adjusting the pointer's Offset within the allocation.

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let neg (bv 64 18446744073709551609)) ; -7 as unsigned 64-bit
    (let q (ptr-add-offset p neg))
    (load none i8 q)
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--iters", "3"}
;; go(prog)
;; ok()
;; check '%0: 000000+0000000000000007'
;; check '000000: XX XX XX XX XX XX XX'
