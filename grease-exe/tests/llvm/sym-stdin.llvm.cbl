; Copyright (c) Galois, Inc. 2025

; Ensure that `--sym-stdin` works.

; ssize_t read(int fildes, void *buf, size_t nbyte);
(declare @read ((filedes (Ptr 32)) (buf (Ptr 64)) (nbyte (Ptr 64))) (Ptr 64))

(defun @test () Unit
  (start start:
    (let buf (alloca none (bv 64 3)))
    (let three (ptr 64 0 (bv 64 3)))
    (let stdin (ptr 32 0 (bv 32 0)))
    (let read (funcall @read stdin buf three))
    (let read-off (ptr-offset 64 read))
    (let three-off (ptr-offset 64 three))
    (assert! (equal? read-off three-off) "read() returned something other than 3!")
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
