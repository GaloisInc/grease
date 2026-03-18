; Copyright (c) Galois, Inc. 2026

; Test uninitialized stack array element access

(defun @test ((arg_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; Load arg
    (let arg_val_ptr (load none i32 arg_ptr))
    (let arg (ptr-offset 32 arg_val_ptr))
    ; Allocate res[10] = 40 bytes
    (let res (alloca none (bv 64 40)))
    ; res[3] = arg * 2
    (let res3_offset (bv 64 12))
    (let res3_ptr (ptr-add-offset res res3_offset))
    (let arg_times_2 (* arg (bv 32 2)))
    (let arg_times_2_ptr (ptr 32 0 arg_times_2))
    (store none i32 res3_ptr arg_times_2_ptr)
    ; res[5] = arg + res[3]
    (let res5_offset (bv 64 20))
    (let res5_ptr (ptr-add-offset res res5_offset))
    (let sum (+ arg arg_times_2))
    (let sum_ptr (ptr 32 0 sum))
    (store none i32 res5_ptr sum_ptr)
    ; Load res[0] (uninitialized!)
    (let res0_ptr res)
    (let res0_val_ptr (load none i32 res0_ptr))
    (let res0 (ptr-offset 32 res0_val_ptr))
    ; Load res[5]
    (let res5_val_ptr (load none i32 res5_ptr))
    (let res5 (ptr-offset 32 res5_val_ptr))
    ; Return res[0] + res[5]
    (let rval (+ res0 res5))
    (return rval)))

;; flags {"--symbol", "test"}
;; go(prog)
;; uninit_stack()
