; Copyright (c) Galois, Inc. 2026

; Test the various concretization overrides

;; flags {"--symbol", "test"}
;; go(prog)

(declare @conc-bool ((b Bool)) Bool)
(declare @conc-integer ((i Integer)) Integer)
(declare @conc-nat ((n Nat)) Nat)
(declare @conc-bv-8 ((x (Bitvector 8))) (Bitvector 8))
(declare @conc-bv-16 ((x (Bitvector 16))) (Bitvector 16))
(declare @conc-bv-32 ((x (Bitvector 32))) (Bitvector 32))
(declare @conc-bv-64 ((x (Bitvector 64))) (Bitvector 64))
(declare @conc-ptr-32 ((p (Ptr 32))) (Ptr 32))
(declare @conc-ptr-64 ((p (Ptr 64))) (Ptr 64))
(declare @conc-vector-bv-8 ((v (Vector (Bitvector 8)))) (Vector (Bitvector 8)))
(declare @fresh-bytes ((name (String Unicode)) (num (Bitvector 64))) (Vector (Bitvector 8)))

(defun @test () Unit
  (start start:
    ; Test conc-bool
    (let b0 (fresh Bool))
    (assume! b0 "assuming b0 is true")
    (let b (funcall @conc-bool b0))
    (assert! b "b should be true")

    ; Test conc-integer
    (let i0 (fresh Integer))
    (assume! (equal? i0 42) "assuming i0 == 42")
    (let i (funcall @conc-integer i0))
    (assert! (equal? i 42) "i should be 42")

    ; Test conc-nat
    (let n0 (fresh Nat))
    (assume! (equal? n0 10) "assuming n0 == 10")
    (let n (funcall @conc-nat n0))
    (assert! (equal? n 10) "n should be 10")

    ; Test conc-bv-8
    (let bv8_0 (fresh (Bitvector 8)))
    (let bv8_val (bv 8 0x42))
    (assume! (equal? bv8_0 bv8_val) "assuming bv8_0 == 0x42")
    (let bv8 (funcall @conc-bv-8 bv8_0))
    (assert! (equal? bv8 bv8_val) "bv8 should be 0x42")

    ; Test conc-bv-16
    (let bv16_0 (fresh (Bitvector 16)))
    (let bv16_val (bv 16 0x1234))
    (assume! (equal? bv16_0 bv16_val) "assuming bv16_0 == 0x1234")
    (let bv16 (funcall @conc-bv-16 bv16_0))
    (assert! (equal? bv16 bv16_val) "bv16 should be 0x1234")

    ; Test conc-bv-32
    (let bv32_0 (fresh (Bitvector 32)))
    (let bv32_val (bv 32 0xDEADBEEF))
    (assume! (equal? bv32_0 bv32_val) "assuming bv32_0 == 0xDEADBEEF")
    (let bv32 (funcall @conc-bv-32 bv32_0))
    (assert! (equal? bv32 bv32_val) "bv32 should be 0xDEADBEEF")

    ; Test conc-bv-64
    (let bv64_0 (fresh (Bitvector 64)))
    (let z (bv 64 0x0))
    (assume! (equal? bv64_0 z) "assuming bv64_0 == 0")
    (let bv64 (funcall @conc-bv-64 bv64_0))
    ; Use fresh-bytes to verify it's concrete
    (funcall @fresh-bytes "test" bv64)
    (assert! (equal? bv64 z) "bv64 should be 0")

    ; Test conc-ptr-32
    (let off32_0 (fresh (Bitvector 32)))
    (assume! (equal? off32_0 (bv 32 0)) "assuming off32_0 == 0")
    (let p32_0 (ptr 32 0 off32_0))
    (funcall @conc-ptr-32 p32_0)

    ; Test conc-ptr-64
    (let off64_0 (fresh (Bitvector 64)))
    (assume! (equal? off64_0 (bv 64 0)) "assuming off64_0 == 0")
    (let p64_0 (ptr 64 0 off64_0))
    (funcall @conc-ptr-64 p64_0)

    ; Test conc-vector-bv-8
    (let v_bv0 (fresh (Bitvector 8)))
    (let v_bv1 (fresh (Bitvector 8)))
    (assume! (equal? v_bv0 (bv 8 0xAA)) "assuming v_bv0 == 0xAA")
    (assume! (equal? v_bv1 (bv 8 0xBB)) "assuming v_bv1 == 0xBB")
    (let vec8 (vector v_bv0 v_bv1))
    (let vec8_conc (funcall @conc-vector-bv-8 vec8))
    (let ve0 (vector-get vec8_conc 0))
    (let ve1 (vector-get vec8_conc 1))
    (assert! (equal? ve0 (bv 8 0xAA)) "ve0 should be 0xAA")
    (assert! (equal? ve1 (bv 8 0xBB)) "ve1 should be 0xBB")

    (return ())))

;; ok()
