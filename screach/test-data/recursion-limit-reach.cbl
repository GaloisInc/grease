; The target is reached first on the path; execution then recurses until the
; recursion limit is hit. Screach should report + verify reachability, finding
; the target before the bound is hit.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; flags {"--loop-bound", "3"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @vuln)
    (funcall @test regs)
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
;; verified()
