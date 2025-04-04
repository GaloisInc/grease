; Copyright (c) Galois, Inc. 2024

(declare @malloc ((size (Bitvector 64))) (Ptr 64))

(defun @startup ((old_buf (Ptr 64)) (old_sz (Ptr 64))) (Struct (Ptr 64) (Ptr 64))
  (start start:
    (let sz (fresh (Bitvector 64)))
    (let buf (funcall @malloc sz))
    (let sz-ptr (ptr 64 0 sz))
    (return (struct buf sz-ptr))))
; CHECK: All goals passed!
