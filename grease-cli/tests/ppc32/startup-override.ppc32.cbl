; Copyright (c) Galois, Inc. 2024

(declare @memset ((s (Ptr 32)) (c (Bitvector 32)) (n (Ptr 32))) (Ptr 32))

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (let buf (get-reg r3 regs))
    (let a (bv 32 97)) ; 'a'
    (let sz (get-reg r4 regs))
    (funcall @memset buf a sz)
    (return regs)))
;; ok()
