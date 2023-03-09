.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $8, -8(%rbp)
pushq $9
movq -8(%rbp), %rax
popq %rbx
popq %rbp
ret 
