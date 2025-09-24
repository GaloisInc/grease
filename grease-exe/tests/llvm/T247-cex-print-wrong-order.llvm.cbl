; Copyright (c) Galois, Inc. 2025

; Regression test for #247, wherein GREASE prints the contents of an
; allocation in the wrong order as part of a counterexample when
; `--initial-precondition` is used.

(declare @abort () Unit)

(defun @test ((p0 (Ptr 64))) Unit
  (start start:
    (let one (bv 64 1))
    (let p1 (ptr-add-offset p0 one))
    (let b0 (load none i8 p0))
    (let b1 (load none i8 p1))
    (let b0off (ptr-offset 8 b0))
    (let b1off (ptr-offset 8 b1))
    (branch (and (equal? b0off (bv 8 0)) (equal? b1off (bv 8 42))) if: else:))
  (defblock if:
    (funcall @abort)
    (return ()))
  (defblock else:
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; check [[
;; Concretized arguments:
;; 
;; %0: 000000+0000000000000000
;; 
;; 000000: 00 2a
;; ]]

; TODO(#247): Why is this printed in reverse order?
;; flags {"--symbol", "test"}
;; flags {"--initial-precondition", "tests/llvm/extra/T247-cex-print-wrong-order.txt"}
;; go(prog)
;; check [[
;; Concretized arguments:
;; 
;; %0: 000000+0000000000000000
;; 
;; 000000: 2a 00
;; ]]
