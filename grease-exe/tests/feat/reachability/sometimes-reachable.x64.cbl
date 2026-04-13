; Checks if grease can detect if a call to a function that is only reachable
; in one arm of a symbolic branch.

;; flags {"--symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let vrdi (get-reg rdi regs))
    (let x (pointer-to-bits vrdi))
    ; Check if (vrdi % 2) == 0
    (branch (equal? (mod x (bv 64 2)) (bv 64 0)) vuln: no-vuln:))

  (defblock vuln:
    (funcall @vuln)
    (return regs))

  (defblock no-vuln:
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
;; verified()
