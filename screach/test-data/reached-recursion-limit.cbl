; @reached fires first on the path, aborting before the recursion limit is hit.
; Mirrors loop.cbl but using @reached instead of a target-symbol call, with
; recursion rather than an infinite loop.
; Screach should report + verify reachability.

;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "reached"}
;; flags {"--loop-bound", "3"}
;; go(prog)

(declare @reached ((x Bool)) Unit)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (funcall @reached #t)
    (funcall @test regs)
    (return regs)))

;; reached "reached"
;; verified()
