main: 
.globl main
pushq $4
pushq $2
popq %rax
popq %rbx
cltd 
idiv %rbx
pushq %rax
popq %rax
ret 
