main: 
.globl main
pushq $5
pushq $3
pushq $4
movq $15,%rbx
popq %rax
imulq %rbx,%rax
addq %rax,(%rsp)
pushq $3
movq $3,%rbx
popq %rax
imulq %rbx,%rax
addq %rax,(%rsp)
movq $3,%rcx
sarq %rcx,(%rsp)
popq %rax
xorq %rax,(%rsp)
popq %rax
ret 
