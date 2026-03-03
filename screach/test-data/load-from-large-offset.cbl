; A test case that demonstrates the use of --initial-precondition to quickly
; abduce a precondition for a load from a large offset into a buffer.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; flags {"--initial-precondition", "test-data/extra/load-from-large-offset-initial-precond.txt"}
;; flags {"--no-heuristics"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let buf (get-reg rdi regs))
    (let large-offset (bv 64 0xffff))
    (let buf-large-offset (pointer-add buf large-offset))
    (let buf-contents (pointer-read (Bitvector 8) le buf-large-offset))
    (let arbitrary-value (bv 8 42))
    (branch (equal? buf-contents arbitrary-value) vuln: not-vuln:))

  (defblock vuln:
    (funcall @vuln)
    (return regs))

  (defblock not-vuln:
    (return regs)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
