(defun @addr-override-set-reg ((regs0 X86Regs)) X86Regs
  (start start:
    (println "here")
    (let deadbeef (bits-to-pointer (bv 64 0xdeadbeef)))
    (let regs (set-reg rdi deadbeef regs0))
    (return regs)))
