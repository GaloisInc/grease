; Test the UX of our error messages for ill-formed JSON shapes

;; flags {"--initial-precondition", "tests/ux/json-shapes-bad.aux.json"}
;; go(prog)

(defun @main () Unit
  (start start:
    (return ())))

;; check [[
;; In file tests/ux/json-shapes-bad.aux.json: Unexpected "not json\n", expecting JSON value]]
