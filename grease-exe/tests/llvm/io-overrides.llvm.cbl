; Copyright (c) Galois, Inc. 2025

; Ensure that the IO overrides work.

;; flags {"--symbol", "test"}
;; go(prog)

(declare @write-c-string ((dst (Ptr 64)) (src (String Unicode))) Unit)

(defun @test () Unit
  (start start:

    (let p (alloca none (bv 64 6)))
    (funcall @write-c-string p "/file")

    ; int open(const char *pathname, int flags, ... /* mode_t mode */ );
    (let g (resolve-global "open"))
    (let h (load-handle (Ptr 32) ((Ptr 64) (Ptr 32)) g))
    (let zero (ptr 32 0 (bv 32 0)))
    (let fd (funcall h p zero))

    ; this will be -1 because open will not find an existing file
    (let off (ptr-offset 32 fd))
    (assert! (equal? off (bv 32 0xffffffff)) "open() returns -1")

    (return ())))

;; ok()
