; Copyright (c) Galois, Inc. 2026

; Test uninitialized stack variable use

(defun @test ((arg_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; Load arg
    (let arg_val_ptr (load none i32 arg_ptr))
    (let arg (ptr-offset 32 arg_val_ptr))
    ; Allocate res
    (let res_ptr (alloca none (bv 64 4)))
    ; Load res (uninitialized!)
    (let res_val_ptr (load none i32 res_ptr))
    (let res (ptr-offset 32 res_val_ptr))
    ; res = res + arg
    (let new_res (+ res arg))
    ; Return
    (return new_res)))

;; flags {"--symbol", "test"}
;; go(prog)
;; uninit_stack()
