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
    (let offset (bv 64 8))
    (let rdi-ptr-offset (pointer-add rdi-ptr offset))
    (let rdi-ptr-offset-contents (pointer-read (Bitvector 64) le rdi-ptr-offset))
    (assert! (equal? rdi-ptr-offset-contents (bv 64 42)) "Value not 42")
    (return regs)))

;; ok()
