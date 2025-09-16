(defun @addr-override-branch ((regs X86Regs)) Unit
  (start start:
    (let rdi (get-reg rdi regs))
	(let null (pointer-make-null))
	(let b (pointer-eq rdi null))
	(branch b if: else:))
  (defblock if:
    (assert! #f "rdi was null")
    (return ()))
  (defblock else:
    (return ())))
