; Test pointer concretization modes via symbolic pointer reads (LLVM)
; Each mode explores different numbers of paths:
; - ptr-conc-none: pointer stays symbolic, explores all 3 paths
; - ptr-conc-unsound: pointer concretized arbitrarily, explores only 1 path (unsound)
; - ptr-conc-unique: pointer not unique, stays symbolic, explores all 3 paths
; - ptr-conc-resolve: pointer not unique, stays symbolic, explores all 3 paths

(declare @branch-one () Unit)
(declare @branch-two () Unit)
(declare @branch-three () Unit)

(defun @test () Unit
  (start start:
    ; Allocate memory and store different values at different offsets
    (let p-base (alloca (Bitvector 8) (bv 64 256)))

    ; Store value 1 at offset 0
    (let addr0 (ptr-add-offset 64 p-base (bv 64 0)))
    (store addr0 (bv 8 1))

    ; Store value 2 at offset 16
    (let addr16 (ptr-add-offset 64 p-base (bv 64 16)))
    (store addr16 (bv 8 2))

    ; Store value 3 at offset 32
    (let addr32 (ptr-add-offset 64 p-base (bv 64 32)))
    (store addr32 (bv 8 3))

    ; Create symbolic offset constrained to be one of {0, 16, 32}
    (let sym-offset (fresh (Bitvector 64)))
    (assume! (or (equal? sym-offset (bv 64 0))
                 (or (equal? sym-offset (bv 64 16))
                     (equal? sym-offset (bv 64 32))))
             "offset is 0, 16, or 32")

    ; Create symbolic pointer - this is the concretization trigger
    (let sym-ptr (ptr-add-offset 64 p-base sym-offset))

    ; READ from symbolic pointer - this triggers concretization
    (let val (load (Bitvector 8) sym-ptr))

    ; Branch on the value read - each branch calls a distinct function
    (branch (equal? val (bv 8 1))
      got-one:
      (branch (equal? val (bv 8 2))
        got-two:
        got-three:)))

  (defblock got-one:
    ; Read value 1 from offset 0 - call branch-one
    (funcall @branch-one)
    (return ()))

  (defblock got-two:
    ; Read value 2 from offset 16 - call branch-two
    (funcall @branch-two)
    (return ()))

  (defblock got-three:
    ; Read value 3 from offset 32 - call branch-three
    (funcall @branch-three)
    (return ())))

;; flags {"--symbol", "test", "--all-solutions"}
;; flags {"--pointer-concretization", "ptr-conc-none"}
;; flags {"--skip", "branch-one", "--skip", "branch-two", "--skip", "branch-three"}
;; go(prog)
;; check "Skipped call to 'branch-one'"
;; check "Skipped call to 'branch-two'"
;; check "Skipped call to 'branch-three'"
;; ok()

;; flags {"--symbol", "test", "--all-solutions"}
;; flags {"--pointer-concretization", "ptr-conc-unsound"}
;; flags {"--skip", "branch-one", "--skip", "branch-two", "--skip", "branch-three"}
;; go(prog)
;; ok()

;; flags {"--symbol", "test", "--all-solutions"}
;; flags {"--pointer-concretization", "ptr-conc-unique"}
;; flags {"--skip", "branch-one", "--skip", "branch-two", "--skip", "branch-three"}
;; go(prog)
;; check "Skipped call to 'branch-one'"
;; check "Skipped call to 'branch-two'"
;; check "Skipped call to 'branch-three'"
;; ok()

;; flags {"--symbol", "test", "--all-solutions"}
;; flags {"--pointer-concretization", "ptr-conc-resolve"}
;; flags {"--skip", "branch-one", "--skip", "branch-two", "--skip", "branch-three"}
;; go(prog)
;; check "Skipped call to 'branch-one'"
;; check "Skipped call to 'branch-two'"
;; check "Skipped call to 'branch-three'"
;; ok()
