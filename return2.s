.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $5,%rax
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
cmpq $0,%rax
je .L1
movq -16(%rbp),%rax
movq %rax,%rbx
movq $5,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
sete %al
cmpq $0,%rax
je .L3
jmp .L2
addq $0,%rsp
.L3: 
movq -8(%rbp),%rax
movq -8(%rbp),%rax
addq $1,-8(%rbp)
addq $0,%rsp
.L2: 
movq -16(%rbp),%rax
movq -16(%rbp),%rax
addq $1,-16(%rbp)
jmp .L0
.L1: 
addq $8,%rsp
movq -8(%rbp),%rax
addq $8,%rsp
popq %rbp
ret 
