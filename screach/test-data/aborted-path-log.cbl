; Test that aborted paths produce an informative log message.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; flags {"--loop-bound", "5"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (jump loop:))

  (defblock loop:
    (jump loop:)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; check "Execution path aborted: resource exhausted"
;; check "Result count: 0"
