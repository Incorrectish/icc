.globl main
main: 
pushq %rbp
movq %rsp,%rbp
movq $4,%rax
movq %rax,-8(%rbp)
movq $0, -16(%rbp)
movq $3,%rax
movq %rax,-24(%rbp)
pushq -8(%rbp)
movq $3,%rax
addq %rax,(%rsp)
popq %rax
movq %rax,-8(%rbp)
pushq %rax
pushq $4
movq -24(%rbp),%rax
movq %rax,-8(%rbp)
movq %rax,%rbx
popq %rax
imulq %rbx,%rax
movq %rax,-32(%rbp)
pushq -32(%rbp)
pushq $3
pushq $5
pushq -8(%rbp)
movq -24(%rbp),%rbx
popq %rax
cqo 
idivq %rbx
movq %rdx,%rax
addq %rax,(%rsp)
popq %rbx
cmpq $0,(%rsp)
je .logicaljump0
cmpq $0,%rbx
je .logicaljump0
movq $1,(%rsp)
jmp .logicaljump1
.logicaljump0: 
movq $0,(%rsp)
.logicaljump1: 
popq %rcx
salq %rcx,(%rsp)
popq %rax
ret 
