# test that at the nop we can read from rsi by checking assert rsi == rdi 

.text
.global test
.type test, %function
test:
    movq %rdi, %rsi
    nop 
    ret
