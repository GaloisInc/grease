; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (get-field 5 regs))  ;; rsp, stack pointer
    (let off (bv 64 0x100000))
    (let q (pointer-add p off))
    (let _ (pointer-read (Bitvector 64) le q))
    (return regs)))
;; uninit_stack()
