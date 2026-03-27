; Test unique concretization for Bool type

(declare @path-true () Unit)
(declare @path-false () Unit)
(declare @unique-conc-bool ((b Bool)) Bool)

(defun @test () Unit
  (start start:
    ; Create a symbolic bool that's uniquely True
    (let choice (fresh Bool))
    (assume! choice "choice is true")

    ; Try to uniquely concretize it
    (let concrete-choice (funcall @unique-conc-bool choice))

    ; Branch on the result
    (branch concrete-choice
      got-true:
      got-false:))
  (defblock got-true:
    (funcall @path-true)
    (return ()))
  (defblock got-false:
    (funcall @path-false)
    (return ())))

; unique-conc-bool: value is uniquely true, so concretizes and takes true path
;; flags {"--symbol", "test"}
;; flags {"--skip", "path-true", "--skip", "path-false"}
;; go(prog)
;; check_not("Invoking the 'path-false' function")
;; check("Invoking the 'path-true' function")
;; ok()
