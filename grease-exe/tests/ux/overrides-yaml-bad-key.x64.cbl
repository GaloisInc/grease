; A test case that checks the error message produced when a user provides an
; overrides-related YAML file where "function address overrides" is misspelled.

;; flags {"--symbol", "test", "--overrides-yaml", "tests/ux/overrides-yaml-bad-key.aux.yaml"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; exception()
;; check [[
;; user error: Unexpected keys in overrides.yaml file:
;; - funktion addres overides]]

