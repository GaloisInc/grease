; Copyright (c) Galois, Inc. 2025

; Ensure that `--fs-root` and the I/O overrides work.

;; flags {"--symbol", "test"}
;; flags {"--fs-root", "tests/llvm/extra/fs"}
;; go(prog)

(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)

; int close(int filedes)
(declare @close ((filedes (Bitvector 32))) (Bitvector 32))

; int open(const char *pathname, int flags, ... /* mode_t mode */ );
(declare @open ((pathname (Ptr 64)) (flags (Bitvector 32))) (Bitvector 32))

; ssize_t read(int fildes, void *buf, size_t nbyte);
(declare @read ((filedes (Bitvector 32)) (buf (Ptr 64)) (nbyte (Bitvector 64))) (Bitvector 64))

(defun @test () Unit
  (start start:

    (let p (alloca none (bv 64 6)))
    (funcall @write-c-string p "/symb")

    (let zero (bv 32 0))
    (let fd (funcall @open p zero))

    ; should find existing file
    (assert! (not (equal? fd (bv 32 0xffffffff))) "open() returned -1!")

    (let buf (alloca none (bv 64 1)))
    (let one (bv 64 1))
    (let _ (funcall @read fd buf one))

    (let closed1 (funcall @close fd))
    (assert! (not (equal? closed1 (bv 32 0xffffffff))) "close() returned -1!")
    (let closed2 (funcall @close fd))
    (assert! (equal? closed2 (bv 32 0xffffffff)) "close() didn't return -1!")

    (return ())))

;; ok()
