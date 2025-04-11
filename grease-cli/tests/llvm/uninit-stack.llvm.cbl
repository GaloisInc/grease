; Copyright (c) Galois, Inc. 2024

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (load none i8 a)
    (return ())))
;; check [[
;; Likely bug: uninitialized stack read at tests/llvm/uninit-stack.llvm.cbl:7:5
;; Allocated at tests/llvm/uninit-stack.llvm.cbl:6:12
;; ]]
