main: 
.globl main
pushq $1
pushq $0
movq $2,%rax
popq %rbx
cqo 
idiv %rbx
addq %rax,(%rsp)
popq %rax
ret 
