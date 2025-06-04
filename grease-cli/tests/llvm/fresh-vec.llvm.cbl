; Copyright (c) Galois, Inc. 2025

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test () (Vector (Bitvector 8))
  (start start:
    (let v (fresh-vec "byte" (Bitvector 8) 2))
	(return v)))
;; ok()
