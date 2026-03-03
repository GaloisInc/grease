; A target override that declares the goal reachable if a predicate that
; should always be false is true.

(declare @reached ((x Bool)) Unit)
(defun @tgt-ov-infeasible ((regs X86Regs)) Unit
  (start start:
    (let rsp (get-reg rsp regs))
    (let deadbeef (bits-to-pointer (bv 64 0xdeadbeef)))
    (let b (pointer-eq rsp deadbeef))
    (funcall @reached b)
    (return ())))
