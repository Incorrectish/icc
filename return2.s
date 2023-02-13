.globl main
main: 
    pushq $5
    movq $3,%rbx
    cmpq $0,(%rsp)
    je .logicaljump0
    cmpq $0,%rbx
    je .logicaljump0
    movq $1,(%rsp)
    jmp .logicaljump1
.logicaljump0: 
    movq $0,(%rsp)
.logicaljump1: 
    popq %rax
    ret 
