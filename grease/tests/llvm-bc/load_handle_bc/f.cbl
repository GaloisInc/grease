; Copyright (c) Galois, Inc. 2024

(defun @f () (Ptr 64)
  (start start:
    (let h (resolve-global "malloc"))
    (let malloc (load-handle (Ptr 64) ((Ptr 64)) h))
    (let ptr-size-offset (bv 64 8))
    (let ptr-size (ptr 64 0 ptr-size-offset))
    (let p (funcall malloc ptr-size))
    (return p)))
