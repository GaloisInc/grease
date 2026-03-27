; Test unique concretization for bitvector types

(declare @path-42 () Unit)
(declare @path-other () Unit)
(declare @unique-conc-bv-32 ((val (Bitvector 32))) (Bitvector 32))

(defun @test () Unit
  (start start:
    ; Create a symbolic bv that's uniquely 42
    (let sym-val (fresh (Bitvector 32)))
    (assume! (equal? sym-val (bv 32 42)) "value is 42")

    ; Try to uniquely concretize it
    (let concrete-val (funcall @unique-conc-bv-32 sym-val))

    ; Branch on the result
    (branch (equal? concrete-val (bv 32 42))
      got-42:
      got-other:))
  (defblock got-42:
    (funcall @path-42)
    (return ()))
  (defblock got-other:
    (funcall @path-other)
    (return ())))

; unique-conc-bv: value is uniquely 42, so concretizes and takes correct path
;; flags {"--symbol", "test"}
;; flags {"--skip", "path-42", "--skip", "path-other"}
;; go(prog)
;; check_not("Invoking the 'path-other' function")
;; check("Invoking the 'path-42' function")
;; ok()
