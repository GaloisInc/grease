; Copyright (c) Galois, Inc. 2025

; Override for `recv` that uses `fresh-bytes`.
;
; Ignores `socket` and `flags`, always reads exactly `length` bytes.
;
; Appears in the docs in `doc/sexp.md`.

(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(defun @recv ((socket (Ptr 32)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 32))) (Ptr 64)
  ; This is a bit more awkward than it needs to be, because there are no
  ; pointer<->integer<->nat conversions in crucible-syntax.
  (registers
    ($ctr (Bitvector 64))  ; loop counter (going down)
    ($idx Nat)             ; index into vector of bytes
    ($ptr (Ptr 64)))       ; pointer to write the next byte into
  (start start:
    (let length-bv (ptr-offset 64 length))
    (let bytes (funcall @fresh-bytes "recv" length-bv))

    (set-register! $ctr length-bv)
    (set-register! $idx 0)
    (set-register! $ptr buffer)
    (jump loop:))
  (defblock loop:
    (let byte (vector-get bytes $idx))
    (let byte-ptr (ptr 8 0 byte))
    (store none i8 $ptr byte-ptr)

    (set-register! $ctr (- $ctr (bv 64 1)))
    (set-register! $idx (+ $idx 1))
    (let ptr (ptr-add-offset $ptr (bv 64 1)))
    (set-register! $ptr ptr)
    (branch (equal? $ctr (bv 64 0)) end: loop:))
  (defblock end:
    (return length)))
