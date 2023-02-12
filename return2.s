main: 
.globl main
pushq $5
pushq $3
pushq $4
movq $15,%rax
orq %rax,(%rsp)
popq %rax
xorq %rax,(%rsp)
popq %rax
andq %rax,(%rsp)
popq %rax
ret 
