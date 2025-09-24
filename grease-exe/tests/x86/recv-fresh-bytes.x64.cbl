; Copyright (c) Galois, Inc. 2025

; Ensure that bytes from calls to `fresh-bytes` in user-provided overrides are
; concretized and printed, using `recv` as a realistic example.

;; flags {"--overrides", "tests/x86/extra/recv.x64.cbl"}
;; flags {"--symbol", "test"}
;; go(prog)

(declare @abort () Unit)

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
;
; TODO(macaw#346): We currently have to use Ptr 64 for `socket` and `flags`
; because macaw-symbolic-syntax doesn't support truncating pointers.
(declare @recv ((socket (Ptr 64)) (buffer (Ptr 64)) (length (Ptr 64)) (flags (Ptr 64))) (Ptr 64))

(defun @test ((regs0 X86Regs)) X86Regs
  (start start:
    (let socket (get-reg rdi regs0)) ; first arg register
    (let buffer0 (get-reg rsp regs0))
    (let buffer (pointer-sub buffer0 (bv 64 1)))
    (let length0 (pointer-make-null))
    (let length (pointer-add length0 (bv 64 1)))
    (let flags (pointer-make-null))
    (let ret0 (funcall @recv socket buffer length flags))
    (let ret (pointer-to-bits ret0))
    (assert! (equal? ret (bv 64 1)) "recv() returned something other than 1!")

    (let byte (pointer-read (Bitvector 8) le buffer))
    (branch (equal? byte (bv 8 0)) if: else:))
  (defblock if:
    (funcall @abort)
    (return regs0))
  (defblock else:
    (return regs0)))

;; check "Call to abort"
;; check [[
;; Concretized values:
;;   recv
;;     00
;; ]]
