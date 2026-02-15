;; flags {"--symbol", "test",  "--initial-precondition", "tests/llvm/extra/out-of-bound-shape.txt", "--no-heuristics"}
;; go(prog)


(defun @test ((p (Ptr 64))) (Ptr 8)
  (start start:
    (let r (load none i8 p))
    (return r)))

;; check "Concretized arguments:"
;; check "%0: 000000+0000000000000003"