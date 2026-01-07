; Copyright (c) Galois, Inc. 2025

; Test the behavior of AND on pointers.

;; flags {"--symbol", "same-ptr"}
;; go(prog)

(defun @same-ptr ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    ; force rdi to be a pointer
    (let rdi0 (pointer-read (Ptr 8) le rdi))
    (let rdi2 (pointer-and rdi rdi))
    (let e (pointer-eq rdi rdi2))
	(assert! e "Bad result from pointer AND")
    (return regs)))

;; ok()

;; ------------------------------------

;; flags {"--symbol", "ptr-null"}
;; go(prog)

(defun @ptr-null ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    ; force rdi to be a pointer
    (let rdi0 (pointer-read (Ptr 8) le rdi))
	(let n (pointer-make-null))
    (let rdin (pointer-and rdi n))
    (let e (pointer-eq rdi rdin))
	(assert! e "Bad result from pointer AND")
    (let ne (pointer-eq rdi n))
	(assert! (not ne) "Bad result from pointer AND")
    (return regs)))

;; ok()

;; ------------------------------------

;; flags {"--symbol", "different-ptrs"}
;; go(prog)

(defun @different-ptrs ((regs X86Regs)) X86Regs
  (start start:
    (let rdi (get-reg rdi regs))
    (let rsi (get-reg rsi regs))
    ; force them both to be pointers
    (let rdi0 (pointer-read (Ptr 8) le rdi))
    (let rsi0 (pointer-read (Ptr 8) le rsi))
    (let _ (pointer-and rdi rsi))
    (return regs)))

;; must_fail()
;; check "Invalid AND of two pointers"
