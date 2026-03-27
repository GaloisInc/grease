; Test unique concretization for Integer type

(declare @path-neg-5 () Unit)
(declare @path-other () Unit)
(declare @unique-conc-integer ((i Integer)) Integer)

(defun @test () Unit
  (start start:
    ; Create a symbolic integer that's uniquely -5
    (let sym-val (fresh Integer))
    (assume! (equal? sym-val (negate 5)) "value is -5")

    ; Try to uniquely concretize it
    (let concrete-val (funcall @unique-conc-integer sym-val))

    ; Branch on the result
    (branch (equal? concrete-val (negate 5))
      got-neg-5:
      got-other:))
  (defblock got-neg-5:
    (funcall @path-neg-5)
    (return ()))
  (defblock got-other:
    (funcall @path-other)
    (return ())))

; unique-conc-integer: value is uniquely -5, so concretizes and takes correct path
;; flags {"--symbol", "test"}
;; flags {"--skip", "path-neg-5", "--skip", "path-other"}
;; go(prog)
;; check_not("Invoking the 'path-other' function")
;; check("Invoking the 'path-neg-5' function")
;; ok()
