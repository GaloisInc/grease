; Copyright (c) Galois, Inc. 2025

; Override used in `skip-override.x64.cbl`

(declare @abort () Unit)

(defun @bug () Unit
  (start start:
    (funcall @abort)
    (return ())))
