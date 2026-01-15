; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol-startup-override", "test:tests/llvm/extra/startup-override.llvm.cbl"}
;; go(prog)

(declare @memset ((s (Ptr 64)) (c (Bitvector 32)) (n (Bitvector 64))) (Ptr 64))

(defun @test ((buf (Ptr 64)) (sz (Ptr 64))) Unit
  (start start:
    (let a (bv 32 97)) ; 'a'
    (let sz-off (ptr-offset 64 sz))
    (funcall @memset buf a sz-off)
    (return ())))
;; ok()
