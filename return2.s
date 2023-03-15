.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $7,%rax
movq %rax,-8(%rbp)
subq $8,%rsp
movq -8(%rbp),%rax
movq %eax,-12(%rbp)
subq $4,%rsp
movq -8(%rbp),%rax
# $12 being returned
addq $12,%rsp
popq %rbp
ret 
