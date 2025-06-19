; A test case to ensure that grease respects pointer offsets in memory shapes.
; This test case will only succeed if rdi (the first argument) contains a
; pointer that points to memory containing the value 42 at an offset of 8 bytes.

;; flags {"--symbol", "test"}
;; flags {"--no-heuristics"}
;; flags {"--initial-precondition", "tests/x86/extra/aliased-shape.txt"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    (let rsi (get-reg rsi regs))
    (let rdi-ptr (pointer-read (Ptr 64) le rdi))
    (let rsi-ptr (pointer-read (Ptr 64) le rsi))
    (let firstbyte (pointer-read (Bitvector 8) le rdi-ptr))
    (let lastbyte (pointer-read (Bitvector 8) le rsi-ptr))
    (let maybe-rsi-ptr (pointer-add rdi-ptr (bv 64 3)))
    (let is-eq-ptrs (pointer-eq rsi-ptr maybe-rsi-ptr))
    (assert! (equal? firstbyte (bv 8 222)) "Value not de")
    (assert! (equal? lastbyte (bv 8 239)) "Value not ef")
    (assert! is-eq-ptrs "The pointers should be aliased to the same block")
    (return regs)))

;; ok()
