.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $5,%rax
movq %rax,-8(%rbp)
movq $0, -16(%rbp)
movq $7,%rax
movq %rax,-24(%rbp)
pushq -8(%rbp)
movq $3,%rax
addq %rax,(%rsp)
popq %rax
movq %rax,-8(%rbp)
pushq %rax
movq -24(%rbp),%rbx
popq %rax
cqo 
idivq %rbx
movq %rdx,%rax
movq %rax,-16(%rbp)
movq %rax,%rbx
pushq -16(%rbp)
movq $5,%rbx
popq %rax
cqo 
idivq %rbx
movq %rdx,%rax
cdq 
popq %rbp
ret 
