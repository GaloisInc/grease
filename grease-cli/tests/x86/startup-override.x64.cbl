; Copyright (c) Galois, Inc. 2024

(declare @memset ((s (Ptr 64)) (c (Bitvector 32)) (n (Ptr 64))) (Ptr 64))

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let buf (get-reg rdi regs))
    (let a (bv 32 97)) ; 'a'
    (let sz (get-reg rsi regs))
    (funcall @memset buf a sz)
    (return regs)))
;; ok()
