; Copyright (c) Galois, Inc. 2024

(defun @test () (Ptr 8)
  (start start:
    (let p (ptr 64 0 (bv 64 0)))
    (let b (load none i8 p))
    (return b)))
;; must_fail()
;; check [[
;; No previous write to this location was found
;;   Attempting load at type: i8
;; Performing overall load at type: i8
;;   Via pointer: 0x0:[64]
;; ]]
