.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $7,%rax
movq %rax,-8(%rbp)
movq $8,%rax
movq %rax,-16(%rbp)
movq $5,%rax
movq %rax,-16(%rbp)
movq -8(%rbp),%rax
popq %rbp
ret 
