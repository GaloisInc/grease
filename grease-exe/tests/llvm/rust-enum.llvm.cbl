; Copyright (c) Galois, Inc. 2024

; A regression test for gitlab#177. This distills the essence of the Rust test
; case down to the equivalent S-expression statements. Namely, we want to ensure
; that we can load from uninitialized memory without crashing when the --rust
; flags is enabled.

;; flags {"--symbol", "test"}
;; flags {"--rust"}
;; go(prog)

(defun @test () Unit
  (start start:
    ; First, allocate a pointer to a chunk of memory that is 16 bytes in size.
    (let p-size (bv 64 16))
    (let p (alloca none p-size))
    ; Next, write a value to the first 8 bytes (but not the other 8 bytes) of
    ; memory.
    (let forty-two-offset (bv 64 42))
    (let forty-two (ptr 64 0 forty-two-offset))
    (store none i64 p forty-two)
    ; Load the first 8 bytes. This should succeed regardless of whether --rust
    ; is used or not.
    (let p-fst (load none i64 p))
    ; Load the other 8 bytes. crucible-llvm treats this as undefined behavior
    ; most of the time, but since we have --rust enabled, this will instead
    ; return a fresh, symbolic value.
    (let eight (bv 64 8))
    (let p-offset (ptr-add-offset p eight))
    (let p-snd (load none i64 p-offset))
    (return ())))
;; ok()
