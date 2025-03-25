; Copyright (c) Galois, Inc. 2024

(declare @id-ptr ((p (Ptr 64))) (Ptr 64))
(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (get-field 0 regs))
    (let p2 (funcall @id-ptr p))
    (let regs2 (set-field regs 0 p2))
    (return regs2)))
