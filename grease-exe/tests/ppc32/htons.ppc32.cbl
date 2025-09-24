; Copyright (c) Galois, Inc. 2025

;; flags {"--symbol", "test"}
;; go(prog)

(declare @htons ((host-short (Bitvector 16))) (Bitvector 16))

(defun @test ((regs PPC32Regs)) PPC32Regs
  (start start:
    (let host-short (bv 16 5000))
    ; PowerPC is big-endian, so `htons` should return the input unchanged
    (let expected-network-short host-short)
    (let actual-network-short (funcall @htons host-short))
    (assert!
      (equal? expected-network-short actual-network-short)
      "htons does not behave as expected")
    (return regs)))
;; ok()
