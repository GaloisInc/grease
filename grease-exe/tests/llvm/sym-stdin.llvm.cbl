; Copyright (c) Galois, Inc. 2025

; Ensure that `--sym-stdin` works.

; ssize_t read(int fildes, void *buf, size_t nbyte);
(declare @read ((filedes (Bitvector 32)) (buf (Ptr 64)) (nbyte (Bitvector 64))) (Bitvector 64))

(defun @test () Unit
  (start start:
    (let three (bv 64 3))
    (let buf (alloca none three))
    (let stdin (bv 32 0))
    (let read (funcall @read stdin buf three))
    (assert! (equal? read three) "read() returned something other than 3!")
    (return ())))

;; flags {"--symbol", "test"}
;; go(prog)
;; check "read() returned something other than 3!"

;; flags {"--symbol", "test"}
;; flags {"--sym-stdin", "2"}
;; go(prog)
;; check "read() returned something other than 3!"

;; flags {"--symbol", "test"}
;; flags {"--sym-stdin", "3"}
;; go(prog)
;; ok()
