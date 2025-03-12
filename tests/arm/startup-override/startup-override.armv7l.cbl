; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Bitvector 32))) (Ptr 32))

(defun @startup ((regs0 AArch32Regs)) AArch32Regs
  (start start:
    (let sz (fresh (Bitvector 32)))
    (let buf (funcall @malloc sz))
    (let sz-ptr0 (pointer-make-null))
    (let sz-ptr1 (pointer-add sz-ptr0 sz))

    (let regs1 (set-reg r0 buf regs0))
    (let regs2 (set-reg r1 sz-ptr1 regs1))
    (return regs2)))
