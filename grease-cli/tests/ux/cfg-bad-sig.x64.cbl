; Test the UX of our error messages for ill-typed S-expression programs

;; flags {"--symbol", "bad-args"}
;; go(prog)

(defun @bad-args ((b Bool)) Unit
  (start start:
    (return ())))

;; check [[
;; Exception: Bad argument types for CFG `bad-args`: [Bool]
;; Expected a single argument, the struct of register values
;; ]]

;; flags {"--symbol", "bad-ret"}
;; go(prog)

(defun @bad-ret ((regs X86Regs)) Unit
  (start start:
    (return ())))

;; check [[
;; Exception: Bad return type for CFG `bad-ret`: Unit
;; Expected the struct of register values
;; ]]
