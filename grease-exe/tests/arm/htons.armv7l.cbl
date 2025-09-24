; Copyright (c) Galois, Inc. 2025

;; flags {"--symbol", "test"}
;; go(prog)

(declare @htons ((host-short (Bitvector 16))) (Bitvector 16))

(defun @test ((regs AArch32Regs)) AArch32Regs
  (start start:
    (let host-short (bv 16 5000))
    ; AArch32 is little-endian, so `htons` should swap the byte order
    (let expected-network-short (bv 16 34835))
    (let actual-network-short (funcall @htons host-short))
    (assert!
      (equal? expected-network-short actual-network-short)
      "htons does not behave as expected")
    (return regs)))
;; ok()
