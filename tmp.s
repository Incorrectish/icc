.globl main
main:
pushq $2
pushq $2
popq %rax
popq %rbx
addq %rbx, %rax
pushq %rax
pushq $4
popq %rax
popq %rbx
imul %rbx, %rax
pushq %rax
pushq $8
popq %rax
popq %rbx
addq %rbx, %rax
pushq %rax
popq %rax
ret
