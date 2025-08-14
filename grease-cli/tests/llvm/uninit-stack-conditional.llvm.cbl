; Copyright (c) Galois, Inc. 2025

; Test that SSE and DFS strategies give the same result.

(defun @test ((b Bool)) Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (branch b if: else:))
  (defblock if:
    (return ()))
  (defblock else:
    (load none i8 a)
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "Sse"}
;; go(prog)
;; uninit_stack()

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "Dfs"}
;; go(prog)
;; uninit_stack()
