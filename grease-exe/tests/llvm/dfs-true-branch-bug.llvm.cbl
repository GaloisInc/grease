; Copyright (c) Galois, Inc. 2026

; Regression test: DFS and BFS should detect a bug on the true branch (the
; first path both strategies explore) even when the false branch succeeds.

(defun @test ((b Bool)) Unit
  (start start:
    (let sz (bv 64 1))
    (let a (alloca none sz))
    (branch b if: else:))
  (defblock if:
    (load none i8 a)
    (return ()))
  (defblock else:
    (return ())))

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "dfs"}
;; go(prog)
;; uninit_stack()

;; flags {"--symbol", "test"}
;; flags {"--path-strategy", "bfs"}
;; go(prog)
;; uninit_stack()
