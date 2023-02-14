.globl main
main: 
    pushq %rbp
    movq %rsp,%rbp
    movq $0, -8(%rbp)
    movq $3,%rax
    movq %rax,-16(%rbp)
    movq $2,%rax
    movq %rax,-8(%rbp)
    pushq $3
    movq $2,%rax
    movq %rax,-8(%rbp)
    popq %rbx
    popq %rax
    imulq %rbx,%rax
    movq %rax,-16(%rbp)
    pushq $3
    movq -16(%rbp),%rbx
    popq %rax
    imulq %rbx,%rax
    pushq %rax
    movq -8(%rbp),%rax
    addq %rax,(%rsp)
    popq %rax
    movq %rax,-8(%rbp)
    pushq $3
    movq $5,%rbx
    cmpq $0,(%rsp)
    je .logicaljump0
    cmpq $0,%rbx
    je .logicaljump0
    movq $1,(%rsp)
    jmp .logicaljump1
.logicaljump0: 
    movq $0,(%rsp)
.logicaljump1: 
    pushq -8(%rbp)
    movq $6,%rbx
    popq %rax
    imulq %rbx,%rax
    pushq %rax
    movq $5,%rax
    addq %rax,(%rsp)
    popq %rcx
    sarq %rcx,(%rsp)
    popq %rax
    ret 
