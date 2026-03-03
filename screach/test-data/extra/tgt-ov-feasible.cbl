; A target override that declares the goal reachable if rcx == 0xdeadbeef.

(declare @reached ((x Bool)) Unit)
(defun @tgt-ov-feasible ((regs X86Regs)) Unit
  (start start:
    (let rcx (get-reg rcx regs))
    (let deadbeef (bits-to-pointer (bv 64 0xdeadbeef)))
    (let b (pointer-eq rcx deadbeef))
    (funcall @reached b)
    (return ())))
