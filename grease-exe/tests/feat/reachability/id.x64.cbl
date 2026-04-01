; A basic S-expression program that checks whether grease can detect if a
; call to a function defined in the S-expression program has been reached.

;; flags {"--symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @vuln)
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
;; verified()
