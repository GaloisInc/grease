; Copyright (c) Galois, Inc. 2025

; Test that GREASE handles all the Crucible types targeted by Crucible-LLVM.

;; flags {"--symbol", "test"}
;; go(prog)

(defun @test
  ((x0 Bool)
   (x1 (Ptr 1))
   (x2 (Ptr 8))
   (x3 (Ptr 16))
   (x4 (Ptr 32))
   (x5 (Ptr 64))
   (x6 (Ptr 128))
   (x7 (Ptr 256))
   (x8 (Ptr 512))
   (x9 (FP Half))
   (x10 (FP Float))
   (x11 (FP Double))
   (x12 (FP Quad))
   (x13 (FP DoubleDouble))
   (x14 (FP X86_80))
   (x15 (Struct Bool Unit))
   (x16 Unit))
  Unit
  (start start:
    (return ())))

;; check [[
;; %0: bool
;; %1: 
;; %2: XX
;; %3: XX XX
;; %4: XX XX XX XX
;; %5: XX XX XX XX XX XX XX XX
;; %6: XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
;; %7: XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
;; %8: XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX XX
;; %9: Half
;; %10: Float
;; %11: Double
;; %12: Quad
;; %13: DoubleDouble
;; %14: X86_80
;; %15: {bool, unit}
;; %16: unit
;; ]]
