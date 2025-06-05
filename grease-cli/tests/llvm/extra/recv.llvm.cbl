; Copyright (c) Galois, Inc. 2025

; Override for `recv` that uses `fresh-bytes`.
;
; Ignores `socket` and `flags`, always reads exactly `length` bytes.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 32)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 32))) (Ptr 64)
  (start start:
    (let length-bv (ptr-offset 64 length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))
    (let byte (vector-get bytes 0))
    (let byte-ptr (ptr 8 0 byte))
    (store none i8 buffer byte-ptr)
    (return length)))
