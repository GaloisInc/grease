; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (let p (get-reg r1 regs))  ;; r1, stack pointer
    (let off (bv 32 0x100000))
    (let q (pointer-add p off))
    (let b (bv 64 42))
;; next_line_must_fail()
    (pointer-write (Bitvector 64) be q b)
    (return regs)))
