; Copyright (c) Galois, Inc. 2025

; Ensure that `--fs-root` and the I/O overrides work.

;; flags {"--symbol", "test"}
;; flags {"--fs-root", "tests/llvm/extra/fs"}
;; go(prog)

(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)

(defun @test () Unit
  (start start:

    (let p (alloca none (bv 64 6)))
    (funcall @write-c-string p "/symb")

    ; int open(const char *pathname, int flags, ... /* mode_t mode */ );
    (let open-global (resolve-global "open"))
    (let open (load-handle (Ptr 32) ((Ptr 64) (Ptr 32)) open-global))
    (let zero (ptr 32 0 (bv 32 0)))
    (let fd (funcall open p zero))

    ; should find existing file
    (let off (ptr-offset 32 fd))
    (assert! (not (equal? off (bv 32 0xffffffff))) "open() returned -1!")

    ; ssize_t read(int fildes, void *buf, size_t nbyte);
    (let read-global (resolve-global "read"))
    (let read (load-handle (Ptr 64) ((Ptr 32) (Ptr 64) (Ptr 64)) read-global))
    (let buf (alloca none (bv 64 4)))
    (let four (ptr 64 0 (bv 64 4)))
    (let _ (funcall read fd buf four))

    ; int close(int filedes)
    (let close-global (resolve-global "close"))
    (let close (load-handle (Ptr 32) ((Ptr 32)) close-global))
    (let closed1 (funcall close fd))
    (let closed1-off (ptr-offset 32 closed1))
    (assert! (not (equal? closed1-off (bv 32 0xffffffff))) "close() returned -1!")
    (let closed2 (funcall close fd))
    (let closed2-off (ptr-offset 32 closed2))
    (assert! (equal? closed2-off (bv 32 0xffffffff)) "close() didn't return -1!")

    (return ())))

;; ok()
