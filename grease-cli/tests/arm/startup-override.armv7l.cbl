; Copyright (c) Galois, Inc. 2024

; flag: --symbol-startup-override test:tests/arm/extra/startup-override.armv7l.cbl

(declare @memset ((s (Ptr 32)) (c (Bitvector 32)) (n (Ptr 32))) (Ptr 32))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let buf (get-reg r0 regs))
    (let a (bv 32 97)) ; 'a'
    (let sz (get-reg r1 regs))
    (funcall @memset buf a sz)
    (return regs)))
;; ok()
