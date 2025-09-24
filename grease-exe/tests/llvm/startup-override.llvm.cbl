; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol-startup-override", "test:tests/llvm/extra/startup-override.llvm.cbl"}
;; go(prog)

(declare @memset ((s (Ptr 64)) (c (Ptr 32)) (n (Ptr 64))) (Ptr 64))

(defun @test ((buf (Ptr 64)) (sz (Ptr 64))) Unit
  (start start:
    (let a (ptr 32 0 (bv 32 97))) ; 'a'
    (funcall @memset buf a sz)
    (return ())))
;; ok()
