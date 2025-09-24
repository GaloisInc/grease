; A test case that checks that a user is able to pass an empty overrides-related
; YAML file and still succeed.

;; flags {"--symbol", "test", "--overrides-yaml", "tests/ux/overrides-yaml-empty.aux.yaml"}
;; go(prog)

(defun @test ((regs X86Regs)) X86Regs
  (start start:
    (return regs)))

;; ok()
