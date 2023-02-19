.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $7,%rax
movq %rax,-8(%rbp)
movq -8(%rbp),%rax
movq %rax,%rbx
movq $5,%rax
xchg %rbx,%rax
cmpq %rax,%rbx
xorq %rax,%rax
setg %al
cmpq $0,%rax
je .L0
movq -8(%rbp),%rax
movq -8(%rbp),%rax
subq $1,-8(%rbp)
jmp .L1
.L0: 
movq -8(%rbp),%rax
addq $1,-8(%rbp)
movq -8(%rbp),%rax
.L1: 
popq %rbp
ret 
