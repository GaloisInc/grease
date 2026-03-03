; objdump -Mintel --no-show-raw-insn -d test-data/fig2-sdse.elf | \
;   grep '<go>:' --after-context=40
;
; yields
;
; 25a0:      	cmp	dword ptr [rbp - 0x18], 0x4
; 25a4:      	jl	0x25ab <go+0x5b>
; 25a6:      	call	0x2530 <vuln>
; 25ab:      	mov	eax, dword ptr [rbp - 0x18]
; 25ae:      	mov	ecx, eax
; 25b0:      	add	ecx, 0x1
; 25b3:      	mov	dword ptr [rbp - 0x18], ecx


(declare @reached ((x Bool)) Unit)

(defun @tgt-ov-interesting ((regs X86Regs)) Unit
  (start start:
    (let rcx_ (get-reg rcx regs))
    (let rcx (pointer-to-bits rcx_))
    (let four (bv 64 4))
    (let b (<= four rcx))
    (funcall @reached b)
    (return ())))
