; Copyright (c) Galois, Inc. 2024

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (let p (get-reg r1 regs))  ;; r1, stack pointer
    (let off (bv 32 0x100000))
    (let q (pointer-add p off))
    (let _ (pointer-read (Bitvector 64) be q))
    (return regs)))
;; uninit_stack()
