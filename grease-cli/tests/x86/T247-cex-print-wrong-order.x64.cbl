; Copyright (c) Galois, Inc. 2025

; Regression test for #247, wherein GREASE prints the contents of an
; allocation in the wrong order as part of a counterexample when
; `--initial-precondition` is used.

;; flags {"--symbol", "test"}
;; flags {"--initial-precondition", "tests/x86/extra/T247-cex-print-wrong-order.txt"}
;; go(prog)

(declare @abort () Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (let buf (get-reg rdi regs))
    (let large-offset (bv 64 0xff))
    (let buf-large-offset (pointer-add buf large-offset))
    (let buf-contents (pointer-read (Bitvector 8) le buf-large-offset))
    (branch (equal? buf-contents (bv 8 42)) abort: no-abort:))

  (defblock abort:
    (funcall @abort)
    (return regs))

  (defblock no-abort:
    (return regs)))

; TODO(#247): Why is this printed in reverse order?
;; check [[
;; 2a ##*ff
;; ]]
