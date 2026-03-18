; Copyright (c) Galois, Inc. 2026

; Test assert(false)

(declare @abort () Unit)

(defun @test () Unit
  (start start:
    (funcall @abort)
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; must_fail()
;; check "Call to abort"
