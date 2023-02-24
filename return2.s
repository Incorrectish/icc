.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $7,%rax
movq %rax,-8(%rbp)
movq $3,%rax
movq %rax,-16(%rbp)
movq -16(%rbp),%rax
movq %rax,%rbx
movq $1,%rax
xchg %rbx,%rax
addq %rbx,%rax
movq %rax,-16(%rbp)
movq -8(%rbp),%rax
movq %rax,%rbx
movq $2,%rax
xchg %rbx,%rax
addq %rbx,%rax
movq %rax,-8(%rbp)
movq $5,%rax
movq %rax,-16(%rbp)
movq -16(%rbp),%rax
popq %rbp
ret 
