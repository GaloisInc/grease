; Copyright (c) Galois, Inc. 2024

(defun @f () (Ptr 64)
  (start start:
    (let h (resolve-global "malloc"))
    (let malloc (load-handle (Ptr 64) ((Bitvector 64)) h))
    (let ptr-size (bv 64 8))
    (let p (funcall malloc ptr-size))
    (return p)))
