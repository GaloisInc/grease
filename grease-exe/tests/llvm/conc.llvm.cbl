; Copyright (c) Galois, Inc. 2024

; Ensure that concretization is working for LLVM.

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((x (Ptr 64))) Unit
  (start start:
    (let blk (ptr-block 64 x))
    (let off (ptr-offset 64 x))
    (let zblk (equal? 0 blk))
    (let zoff (equal? (bv 64 0) off))
    (let b (and zblk zoff))
    (branch b if: else:))
  (defblock if:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ()))
  (defblock else:
    (return ())))
;; check [[
;; Concretized arguments:
;; 
;; %0: 0000000000000000
;; ]]
