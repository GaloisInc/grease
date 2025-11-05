# test that at the nop we can read from rsi by checking assert rsi == rdi 
# this tests that we can observe the result of the function call

.text

.global callee
.type callee, %function

callee:
    movq %rdi, %rsi
    ret

.global test
.type test, %function
test:
    call callee
    nop 
    ret
