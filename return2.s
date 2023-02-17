.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $7,%rax
movq %rax,-8(%rbp)
movq $0,%rax
cmpq $0,%rax
je .else0
movq $8,%rax
movq %rax,-8(%rbp)
movq %rax,%rbx
.else0: 
movq -8(%rbp),%rax
cdq 
popq %rbp
ret 
