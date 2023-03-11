.globl sum
sum: 
pushq %rbp
movq %rsp,%rbp
movq 24(%rbp),%rax
movq %rax,%rbx
movq 16(%rbp),%rax
xchg %rbx,%rax
addq %rbx,%rax
addq $0,%rsp
popq %rbp
ret 
.globl main
main: 
pushq %rbp
movq %rsp,%rbp
pushq %rbx
movq $2,%rax
pushq %rax
movq $1,%rax
pushq %rax
call sum
addq $16,%rsp
popq %rbx
movq %rax,%rbx
pushq %rbx
movq $2,%rax
pushq %rax
movq $1,%rax
pushq %rax
call sum
addq $16,%rsp
popq %rbx
movq %rax,%rbx
movq $2,%rax
xchg %rbx,%rax
cqo 
idivq %rbx
movq %rax,%rbx
movq $2,%rax
xchg %rbx,%rax
imulq %rbx,%rax
xchg %rbx,%rax
subq %rbx,%rax
movq %rax,-8(%rbp)
subq $8,%rsp
movq -8(%rbp),%rax
addq $8,%rsp
popq %rbp
ret 
