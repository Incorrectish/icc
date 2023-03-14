.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $0,%rax
movq %rax,-8(%rbp)
subq $8,%rsp
.L0: 
movq -8(%rbp),%rax
pushq %rax
movq $1,%rax
movq %rax,%rcx
popq %rax
addq %rcx,%rax
movq %rax,-8(%rbp)
# start of if
movq -8(%rbp),%rax
pushq %rax
movq $3,%rax
movq %rax,%rcx
popq %rax
cmpq %rcx,%rax
movq $0,%rax
setg %al
cmpq $0,%rax
je .L3
# start of conditional body
jmp .L1
.L3: 
addq $0,%rsp
.L2: 
jmp .L0
.L1: 
movq -8(%rbp),%rax
# $8 being returned
addq $8,%rsp
popq %rbp
ret 
