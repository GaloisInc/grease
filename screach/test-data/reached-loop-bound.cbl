; @reached fires first on the path, aborting before the loop bound is hit.
; Mirrors loop.cbl but using @reached instead of a target-symbol call.
; Screach should report + verify reachability.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "reached"}
;; flags {"--loop-bound", "3"}
;; go(prog)

(declare @reached ((x Bool)) Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @reached #t)
    (jump loop:))

  (defblock loop:
    (jump loop:)))

;; reached "reached"
;; verified()
