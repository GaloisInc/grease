; Copyright (c) Galois, Inc. 2025

; Ensure that concretization of the symbolic filesystem works.

;; flags {"--symbol", "test"}
;; flags {"--fs-root", "tests/llvm/extra/fs"}
;; go(prog)

(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)

; int open(const char *pathname, int flags, ... /* mode_t mode */ );
(declare @open ((pathname (Ptr 64)) (flags (Ptr 32))) (Ptr 32))

; ssize_t read(int fildes, void *buf, size_t nbyte);
(declare @read ((filedes (Ptr 32)) (buf (Ptr 64)) (nbyte (Ptr 64))) (Ptr 64))

(defun @test () Unit
  (start start:

    (let p (alloca none (bv 64 6)))
    (funcall @write-c-string p "/symb")

    (let zero (ptr 32 0 (bv 32 0)))
    (let fd (funcall @open p zero))

    ; should find existing file
    (let off (ptr-offset 32 fd))
    (assert! (not (equal? off (bv 32 0xffffffff))) "open() returned -1!")

    (let buf (alloca none (bv 64 1)))
    (let one (ptr 64 0 (bv 64 1)))
    (let _ (funcall @read fd buf one))

    (let byte (load none i8 buf))
    (let byte-off (ptr-offset 8 byte))
    (branch (equal? byte-off (bv 8 0)) if: else:))
  (defblock if:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ()))
  (defblock else:
    (return ())))

;; check [[
;; Concretized filesystem:
;;   /symb
;;     00
;; ]]
