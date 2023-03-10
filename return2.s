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
.L0: 
movq -16(%rbp),%rax
movq %rax,%rbx
movq $10,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
setl %al
cmpq $0, %rax
je .L1
movq -16(%rbp),%rax
movq -16(%rbp),%rax
addq $1,-16(%rbp)
subq $8,%rsp
movq $0,%rax
movq %rax,-24(%rbp)
.L2: 
movq -24(%rbp),%rax
movq %rax,%rbx
movq $10,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
setl %al
cmpq $0, %rax
je .L3
movq -24(%rbp),%rax
addq $1,-24(%rbp)
movq -24(%rbp),%rax
movq -8(%rbp),%rax
movq %rax,%rbx
movq -24(%rbp),%rax
addq $1,-24(%rbp)
movq -24(%rbp),%rax
pushq %rax
movq -16(%rbp),%rax
movq -16(%rbp),%rax
addq $1,-16(%rbp)
pushq %rax
call add
addq $16,%rsp
xchg %rbx,%rax
addq %rbx,%rax
movq %rax,-8(%rbp)
addq $0,%rsp
jmp .L2
.L3: 
addq $8,%rsp
jmp .L0
.L1: 
addq $8,%rsp
movq -8(%rbp),%rax
addq $8,%rsp
popq %rbp
ret 
