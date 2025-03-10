; Copyright (c) Galois, Inc. 2024

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (pointer-make-null))
    (pointer-read (Bitvector 8) le p)
    (return regs)))
