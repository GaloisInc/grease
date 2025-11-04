(defun @addr-override ((regs X86Regs)) Unit
  (start start:
    (let vrip (get-reg rip regs))
    (let vrsi (get-reg rsi regs))
    (assert! (pointer-eq vrip vrsi) "assert that rsi and rip are equal")
    (return ())))
