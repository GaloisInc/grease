; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test ((p (Ptr 64))) Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (load none i8 a)
    (return ())))
;; check [[
;; Finished analyzing 'test'. Likely bug: uninitialized stack read at tests/llvm/uninit-stack.llvm.cbl:10:5
;; Allocated at tests/llvm/uninit-stack.llvm.cbl:9:12
;; ]]
