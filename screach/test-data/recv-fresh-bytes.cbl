; Ensure that bytes from calls to `fresh-bytes` in user-provided overrides are
; concretized and printed, using `recv` as a realistic example.

;; flags {"--overrides", "test-data/extra/recv.cbl"}
;; flags {"--entry-symbol", "test"}
;; flags {"--target-symbol", "vuln"}
;; go(prog)

; `man 2 recv`: ssize_t recv(int socket, void *buffer, size_t length, int flags)
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
    (funcall @vuln)
    (return regs0))
  (defblock else:
    (return regs0)))

(defun @vuln () Unit
  (start start:
    (return ())))

;; reached "vuln"
;; check [[
;; Concretized values:
;;   recv
;;   00
;; ]]
;; verified()
