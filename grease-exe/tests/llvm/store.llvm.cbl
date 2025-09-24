; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let blk (the Nat 0))
    (let off (bv 8 0))
    (let v (ptr 8 blk off))
    (store none i8 p v)
    (return ())))
;; ok()
;; check [[
;; Final refined precondition:
;; 
;; %0: 000000+0000000000000000
;; 
;; 000000: ##
;; ]]
