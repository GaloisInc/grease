; Copyright (c) Galois, Inc. 2024

; flags: --symbol test

(defun @test () Unit
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
;; next_line_must_fail()
    (let b (store none i64 p p))
    (return ())))
;; check [[
;; The region wasn't allocated, wasn't large enough, or was marked as readonly
;; Performing store at type: i64
;;   Via pointer: 0x0:[64]
;; ]]
