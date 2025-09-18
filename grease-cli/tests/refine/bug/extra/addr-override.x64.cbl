(defun @addr-override ((regs X86Regs)) X86Regs
  (start start:
    (assert! #f "assertion in address override, expected to fail")
    (return regs)))
