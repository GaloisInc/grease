; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let p (pointer-make-null))
    (pointer-read (Bitvector 8) le p)
    (return regs)))
;; must_fail()
