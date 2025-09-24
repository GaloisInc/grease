; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (load none i8 p)
    (return ())))
;; ok()
;; check [[
;; Final refined precondition:
;; 
;; %0: 000000+0000000000000000
;; 
;; 000000: XX
;; ]]
