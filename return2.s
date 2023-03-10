.globl add
add: 
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
movq $7,%rax
movq %rax,-8(%rbp)
subq $8,%rsp
movq $0,%rax
movq %rax,-16(%rbp)
subq $8,%rsp
movq $0,%rax
movq %rax,-24(%rbp)
subq $8,%rsp
movq $0,%rax
movq %rax,-32(%rbp)
subq $8,%rsp
.L0: 
movq -32(%rbp),%rax
movq %rax,%rbx
movq $10,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
setl %al
cmpq $0, %rax
je .L1
subq $8,%rsp
movq $0,%rax
movq %rax,-40(%rbp)
.L2: 
movq -40(%rbp),%rax
movq %rax,%rbx
movq $10,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
setl %al
cmpq $0, %rax
je .L3
movq -8(%rbp),%rax
movq %rax,%rbx
pushq %rbx
movq -40(%rbp),%rax
addq $1,-40(%rbp)
movq -40(%rbp),%rax
pushq %rax
movq -32(%rbp),%rax
movq -32(%rbp),%rax
addq $1,-32(%rbp)
pushq %rax
call add
addq $16,%rsp
popq %rbx
xchg %rbx,%rax
addq %rbx,%rax
movq %rax,-8(%rbp)
addq $0,%rsp
movq -40(%rbp),%rax
addq $1,-40(%rbp)
movq -40(%rbp),%rax
jmp .L2
.L3: 
addq $8,%rsp
movq -32(%rbp),%rax
movq -32(%rbp),%rax
addq $1,-32(%rbp)
jmp .L0
.L1: 
addq $8,%rsp
movq -8(%rbp),%rax
addq $24,%rsp
popq %rbp
ret 
