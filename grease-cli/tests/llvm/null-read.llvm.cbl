; Copyright (c) Galois, Inc. 2024

; flags: --symbol test

(defun @test () (Ptr 8)
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
;; next_line_must_fail()
    (let b (load none i8 p))
    (return b)))
;; check [[
;; No previous write to this location was found
;;   Attempting load at type: i8
;; Performing overall load at type: i8
;;   Via pointer: 0x0:[64]
;; ]]
