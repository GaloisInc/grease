; Copyright (c) Galois, Inc. 2025

; Test `--debug-cmd`

;; flags {"--symbol", "test"}
;; flags {"--debug"}
;; flags {"--debug-cmd", "help"}
;; flags {"--debug-cmd", "quit"}
;; go(prog)

(defun @test () Unit
  (start start:
    (return ())))

;; check "help (h): Display help text"
;; ok()
