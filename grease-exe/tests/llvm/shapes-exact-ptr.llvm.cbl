; Copyright (c) Galois, Inc. 2025

; Regression test for #246, wherein GREASE fails to parse a memory shape
; consisting of exact bytes followed by a pointer.

; TODO(#246): Fix this parse error
;; flags {"--symbol", "test"}
;; flags {"--initial-precondition", "tests/llvm/extra/shapes-exact-ptr.txt"}
;; go(prog)
;; check [[
;; Exception: tests/llvm/extra/shapes-exact-ptr.txt:3:19:
;;   |
;; 3 | 00: 00 00 00 00 01+00
;;   |                   ^
;; unexpected '+'
;; expecting '*', end of input, newline, or space
;; ]]

; TODO(#246): Fix this parse error
;; flags {"--symbol", "test"}
;; flags {"--initial-precondition", "tests/llvm/extra/shapes-exact-ptr-rle.txt"}
;; go(prog)
;; check [[
;; Exception: tests/llvm/extra/shapes-exact-ptr-rle.txt:3:12:
;;   |
;; 3 | 00: 00*4 01+00
;;   |            ^
;; unexpected '+'
;; expecting '*', end of input, newline, or space
;; ]]

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (return ())))
