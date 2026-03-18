; Copyright (c) Galois, Inc. 2026

; Test uninitialized variable in conditional

(defun @test ((a_ptr (Ptr 64))) (Bitvector 32)
  (start start:
    ; Load a
    (let a_val_ptr (load none i32 a_ptr))
    (let a (ptr-offset 32 a_val_ptr))
    ; Allocate rval
    (let rval_ptr (alloca none (bv 64 4)))
    ; Check if a != 0
    (let a_nonzero (not (equal? a (bv 32 0))))
    (branch a_nonzero set_rval: load_rval:))
  (defblock set_rval:
    ; rval = 42
    (let forty_two_ptr (ptr 32 0 (bv 32 42)))
    (store none i32 rval_ptr forty_two_ptr)
    (jump load_rval:))
  (defblock load_rval:
    ; Load rval (may be uninitialized if a==0!)
    (let rval_val_ptr (load none i32 rval_ptr))
    (let rval (ptr-offset 32 rval_val_ptr))
    (return rval)))

;; flags {"--symbol", "test"}
;; go(prog)
;; could_not_infer()
