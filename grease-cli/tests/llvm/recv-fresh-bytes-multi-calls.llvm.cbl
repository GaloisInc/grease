; Copyright (c) Galois, Inc. 2025

; Like `recv-fresh-bytes`, but with `recv`ing multiple times.

;; flags {"--overrides", "tests/llvm/extra/recv.llvm.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
(declare @recv ((socket (Ptr 32)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 32))) (Ptr 64))

(defun @test ((socket (Ptr 32))) Unit
  (start start:
    (let buffer (alloca none (bv 64 2)))
    (let length (ptr 64 0 (bv 64 1)))
    (let flags (ptr 32 0 (bv 32 0)))
    (let r (funcall @recv socket buffer length flags))
    (let off (ptr-offset 64 r))
    (assert! (equal? off (bv 64 1)) "recv() returned something other than 1!")

    (let buffer2 (ptr-add-offset buffer (bv 64 1)))
    (let r2 (funcall @recv socket buffer2 length flags))
    (let off2 (ptr-offset 64 r))
    (assert! (equal? off2 (bv 64 1)) "recv() returned something other than 1!")

    (let byte (load none i8 buffer))
    (let byte-off (ptr-offset 8 byte))
    (let byte2 (load none i8 buffer2))
    (let byte-off2 (ptr-offset 8 byte2))
    (branch (and (equal? byte-off (bv 8 0)) (equal? byte-off2 (bv 8 1))) if: else:))
  (defblock if:
    (let g (resolve-global "abort"))
    (let h (load-handle Unit () g))
    (funcall h)
    (return ()))
  (defblock else:
    (return ())))

;; check "Call to abort"
;; check [[
;; Concretized values:
;;   recv
;;     00
;;   recv
;;     01
;; ]]
