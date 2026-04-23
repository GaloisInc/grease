; Override for `recv` that uses `fresh-bytes` + `write-byte-vec`.
;
; Ignores `socket` and `flags`, always reads exactly `length` bytes.
;
; Appears in the docs in `doc/sexp.md`.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))
(declare @write-byte-vec ((dest (Ptr 64)) (src (Vector (Bitvector 8)))) Unit)

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 64)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 64))) (Ptr 64)
  (start start:
    (let length-bv (pointer-to-bits length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))
    (funcall @write-byte-vec buffer bytes)
    (return length)))
