.globl main
main: 
pushq %rbp
movq %rsp,%rbp
pushq $8
movq (%rsp),-8(%rbp)
movq -8(%rbp),%rax
cdq 
popq %rbp
ret 
