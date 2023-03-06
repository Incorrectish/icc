.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $0,%rax
movq %rax,-8(%rbp)
movq -8(%rbp),%rax
movq %rax,%rbx
movq $9,%rax
xchg %rbx,%rax
addq %rbx,%rax
movq %rax,-8(%rbp)
movq $0,%rax
movq %rax,-24(%rbp)
movq $1,%rax
movq %rax,-24(%rbp)
movq -24(%rbp),%rax
movq -24(%rbp),%rax
addq $1,-24(%rbp)
movq -8(%rbp),%rax
popq %rbp
ret 
