(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @addr-override ((regs X86Regs)) Unit
  (start start:
    (let sz (fresh (Bitvector 64)))
    (let _ (funcall @malloc sz))
    (return ())))
