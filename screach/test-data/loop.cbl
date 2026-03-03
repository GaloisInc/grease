; A program which reaches the target and then goes into an infinite loop. The
; desired behavior is for screach to exit right after finding the target,
; thereby avoiding the infinite loop.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @vuln)
    (jump loop:))

  (defblock loop:
    (jump loop:)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
