(defun @addr-override ((regs X86Regs)) Unit
  (start start:
    (let vrdi (get-reg rdi regs))
    (let vrsi (get-reg rsi regs))
    (let iseq (pointer-eq vrdi vrsi))
    (assert! iseq "assert that rsi and rdi are equal")
    (return ())))
