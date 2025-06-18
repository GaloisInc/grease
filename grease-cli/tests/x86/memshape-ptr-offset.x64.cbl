; A test case to ensure that grease respects pointer offsets in memory shapes.
; This test case will only succeed if rdi (the first argument) contains a
; pointer that points to memory containing the value 42 at an offset of 8 bytes.

;; flags {"--symbol", "test"}
;; flags {"--no-heuristics"}
;; flags {"--initial-precondition", "tests/x86/extra/memshape-ptr-offset-init-precond.txt"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    (let rdi-ptr (pointer-read (Ptr 64) le rdi))
    (let rdi-ptr-offset-contents (pointer-read (Bitvector 64) le rdi-ptr))
    (assert! (equal? rdi-ptr-offset-contents (bv 64 42)) "Value not 42")
    (return regs)))

;; ok()
