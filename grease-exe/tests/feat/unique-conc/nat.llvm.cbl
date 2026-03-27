; Test unique concretization for Nat type

(declare @path-7 () Unit)
(declare @path-other () Unit)
(declare @unique-conc-nat ((n Nat)) Nat)

(defun @test () Unit
  (start start:
    ; Create a symbolic nat that's uniquely 7
    (let sym-val (fresh Nat))
    (assume! (equal? sym-val 7) "value is 7")

    ; Try to uniquely concretize it
    (let concrete-val (funcall @unique-conc-nat sym-val))

    ; Branch on the result
    (branch (equal? concrete-val 7)
      got-7:
      got-other:))
  (defblock got-7:
    (funcall @path-7)
    (return ()))
  (defblock got-other:
    (funcall @path-other)
    (return ())))

; unique-conc-nat: value is uniquely 7, so concretizes and takes correct path
;; flags {"--symbol", "test"}
;; flags {"--skip", "path-7", "--skip", "path-other"}
;; go(prog)
;; check_not("Invoking the 'path-other' function")
;; check("Invoking the 'path-7' function")
;; ok()
