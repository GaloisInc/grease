; Copyright (c) Galois, Inc. 2024

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test () Unit
  (start start:
    (let g (resolve-global "exit"))
    (let h (load-handle Unit ((Bitvector 32)) g))
	(let p (bv 32 0))
    (funcall h p)
    (return ())))
;; check "Program exited"
