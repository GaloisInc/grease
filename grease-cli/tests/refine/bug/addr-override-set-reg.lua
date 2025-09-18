-- Copyright (c) Galois, Inc. 2025

-- Test the ability of address overrides to set registers

-- FYI:
--
-- objdump -Mintel --no-show-raw-insn -d inc_ptr.x64.elf | \
--   grep '<test>:' --after-context=40
--
-- yields
--
-- 0000000000401010 <test>:
--   401010:      	push	rbp
--   401011:      	mov	rbp, rsp
--   401014:      	sub	rsp, 0x10
--   401018:      	mov	qword ptr [rbp - 0x8], rdi
--   40101c:      	mov	rax, qword ptr [rbp - 0x8]
--   401020:      	mov	edi, dword ptr [rax]

path = "tests/refine/bug/extra/addr-override-set-reg.x64.cbl"
flags {"--addr-override", "0x401018:" .. path}  -- see above
flags {"--symbol", "test"}
go "tests/refine/pos/inc_ptr/test.x64.elf"
must_fail()
check "0xdeadbeef"
