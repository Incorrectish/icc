.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $0,%rax
movq %rax,-8(%rbp)
.L0: 
movq -8(%rbp),%rax
movq %rax,%rbx
movq $10,%rax
xchg %rbx,%rax
cmpq %rbx,%rax
movq $0,%rax
setl %al
cmpq $0, %rax
je .L1
movq -8(%rbp),%rax
movq -8(%rbp),%rax
addq $1,-8(%rbp)
jmp .L0
.L1: 
movq -8(%rbp),%rax
popq %rbp
ret 
