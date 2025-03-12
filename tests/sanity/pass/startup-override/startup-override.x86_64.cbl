; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @startup ((regs0 X86Regs)) X86Regs
  (start start:
    (let sz (fresh (Bitvector 64)))
    (let buf (funcall @malloc sz))
    (let sz-ptr0 (pointer-make-null))
    (let sz-ptr1 (pointer-add sz-ptr0 sz))

    (let regs1 (set-reg rdi buf regs0))
    (let regs2 (set-reg rsi sz-ptr1 regs1))
    (return regs2)))
