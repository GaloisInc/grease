; Ensure that concretization of the symbolic filesystem works.

;; flags {"--sym-file", "1:/symb"}
;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @vuln)
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; check [[
;; Concretized filesystem:
;;   /symb
;; ]]
;; verified()
