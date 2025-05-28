; Copyright (c) Galois, Inc. 2025

; Ensure that concretization of the symbolic filesystem works.

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
    (let buf (alloca none (bv 64 1)))
    (let one (ptr 64 0 (bv 64 1)))
    (let _ (funcall read fd buf one))

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
;; /symb
;; 00
;; ]]
