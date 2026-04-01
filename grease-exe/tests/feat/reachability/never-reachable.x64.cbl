; A basic program where the vulnerability is /not/ reached.

;; flags {"--symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; failed_to_reach()
