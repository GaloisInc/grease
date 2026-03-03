; Test the UX of timeouts.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; flags {"--loop-bound", "100000000"}
;; flags {"--timeout", "1"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (jump start:)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; check "Failed to reach target! (Can't refine due to timeout)"
